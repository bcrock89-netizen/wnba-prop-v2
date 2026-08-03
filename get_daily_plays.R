# ==============================================================================
# PIPELINE STEP 2: AI PREDICTION ENGINE (get_daily_plays.R)
# ==============================================================================
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(wehoop, dplyr, readr, httr2, jsonlite, janitor, stringr)

# ------------------------------------------------------------------------------
# 1. LOAD DATASETS AND CLEAN NAMES
# ------------------------------------------------------------------------------
message("Loading historical tracking and seasonal datasets...")
props_history <- readr::read_csv("data/tracked_props.csv", show_col_types = FALSE) %>%
  janitor::clean_names() %>% 
  mutate(Parsed_Date = as.Date(date))

# ------------------------------------------------------------------------------
# 2. CHECK TODAY'S SCHEDULE VIA WEHOOP
# ------------------------------------------------------------------------------
today_date <- Sys.Date()
message(paste("Checking matchups for:", today_date))

schedule <- wehoop::load_wnba_schedule(seasons = 2026) %>%
  filter(as.Date(game_date) == today_date)

if (nrow(schedule) == 0) {
  if (!dir.exists("predictions")) dir.create("predictions")
  writeLines("# 📊 Daily Report\nNo games scheduled for today.", paste0("predictions/plays_", today_date, ".md"))
  message("No games scheduled today. Created empty report and exiting cleanly.")
  quit(status = 0)
}

# ------------------------------------------------------------------------------
# 3. CALCULATE HIGH-LEVEL ROI & MOMENTUM BREAKDOWNS
# ------------------------------------------------------------------------------
message("Calculating macroscopic portfolio ROI metrics...")

# Global Timeline Summary
roi_summary <- props_history %>%
  summarize(
    season_bets   = n(),
    season_profit = sum(profit, na.rm = TRUE),
    season_roi    = (season_profit / season_bets) * 100,
    
    bets_30       = sum(Parsed_Date >= (today_date - 30), na.rm = TRUE),
    profit_30     = sum(ifelse(Parsed_Date >= (today_date - 30), profit, 0), na.rm = TRUE),
    roi_30        = (profit_30 / max(bets_30, 1)) * 100,
    
    bets_14       = sum(Parsed_Date >= (today_date - 14), na.rm = TRUE),
    profit_14     = sum(ifelse(Parsed_Date >= (today_date - 14), profit, 0), na.rm = TRUE),
    roi_14        = (profit_14 / max(bets_14, 1)) * 100
  )

# Micro Strategy Breakdown (Using your fixed bet_type column name)
bet_type_momentum <- props_history %>%
  group_by(bet_type) %>%
  summarize(
    season_tracked    = n(),
    season_roi        = (sum(profit, na.rm = TRUE) / n()) * 100,
    recent_14_tracked = sum(Parsed_Date >= (today_date - 14), na.rm = TRUE),
    recent_14_roi     = (sum(ifelse(Parsed_Date >= (today_date - 14), profit, 0), na.rm = TRUE) / max(recent_14_tracked, 1)) * 100,
    .groups           = "drop"
  ) %>%
  filter(season_tracked >= 15)

# ------------------------------------------------------------------------------
# 4. CONSTRUCT COMPACT AI PAYLOAD AND SYSTEM PROMPT
# ------------------------------------------------------------------------------
matchups_text <- paste(schedule$name, collapse = ", ")

system_prompt <- "You are a quantitative sports betting machine. Your target is maximizing overall portfolio ROI by identifying shifting market efficiencies."

user_prompt <- paste0(
  "Today's Matchups: ", matchups_text, "\n\n",
  "--- OVERALL ACCURACY HEADER DATA ---\n", jsonlite::toJSON(roi_summary, auto_unbox = TRUE), "\n\n",
  "--- ROI BY BET TYPE STRATIFICATION ---\n", jsonlite::toJSON(bet_type_momentum, auto_unbox = TRUE), "\n\n",
  "Instructions:\n",
  "1. CRITICAL: You must begin your response with a clean Markdown dashboard header grid summarizing our overall portfolio health (Total Bets, Profit, and ROI) for Season-Long, Last 30 Days, and Last 14 Days using the exact values from the OVERALL ACCURACY HEADER DATA.\n",
  "2. Evaluate today's game matchups against our historical data splits.\n",
  "3. Prioritize 'bet_type' categories where our last 14-day ROI is climbing significantly above our season baseline (positive momentum), or high steady baselines.\n",
  "4. Output the top 3 high-value prop plays for today. For each selection, include a brief 2-sentence statistical justification highlighting season-long baseline vs recent momentum."
)

# ------------------------------------------------------------------------------
# 5. CALL ANTHROPIC CLAUDE API
# ------------------------------------------------------------------------------
api_key <- Sys.getenv("ANTHROPIC_API_KEY")
if (api_key == "") stop("CRITICAL: ANTHROPIC_API_KEY environment variable is missing!")

message("Sending data payload to Claude 3.5 Sonnet...")
req <- request("https://anthropic.com") %>%
  req_headers(
    `x-api-key`         = api_key,
    `anthropic-version` = "2023-06-01",
    `content-type`      = "application/json"
  ) %>%
  req_body_json(list(
    model      = "claude-3-5-sonnet-20240620",
    max_tokens = 1200,
    system     = system_prompt,
    messages   = list(list(role = "user", content = user_prompt))
  ))

response <- req_perform(req)
body     <- resp_body_json(response)
ai_play_selections <- body$content[]$text

# ------------------------------------------------------------------------------
# 6. ARCHIVE OUTPUT SELECTIONS
# ------------------------------------------------------------------------------
if (!dir.exists("predictions")) dir.create("predictions")
file_path <- paste0("predictions/plays_", today_date, ".md")
writeLines(ai_play_selections, file_path)

message(paste("Success! Daily selections saved to:", file_path))
# ==============================================================================
