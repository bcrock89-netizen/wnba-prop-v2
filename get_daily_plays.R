# ==============================================================================
# PIPELINE STEP 2: AI PREDICTION ENGINE WITH FULL BACKLOG AGGREGATION
# ==============================================================================
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(wehoop, dplyr, readr, httr2, jsonlite, stringr)

# ------------------------------------------------------------------------------
# 1. LOAD DATASETS AND REFORMAT RAW DATA ARRAYS
# ------------------------------------------------------------------------------
message("Loading historical tracking backlog...")
raw_data <- readr::read_csv("data/tracked_props.csv", skip = 1, col_names = FALSE, show_col_types = FALSE)

colnames(raw_data) <- c(
  "date", "player", "bet_type", "side", "line", "odds", 
  "win_probability", "dtm", "result", "projection", 
  "stat_value", "profit", "be_prob", "day", "month"
)

# Robustly clean tracking data formatting elements
props_history <- raw_data %>%
  mutate(
    Parsed_Date = as.Date(date),
    # Force text percentages (e.g., 64% or 64) into numerical decimals (0.64)
    win_probability = as.numeric(stringr::str_remove(as.character(win_probability), "%")),
    win_probability = ifelse(win_probability > 1, win_probability / 100, win_probability),
    # FIX: Force text values or empty rows inside the DTM column to become pure numeric data
    dtm = as.numeric(as.character(dtm)),
    profit = as.numeric(as.character(profit))
  )

# ------------------------------------------------------------------------------
# 2. CHECK TODAY'S MATCHUPS (FIXED FOR 2026 SCHEMA)
# ------------------------------------------------------------------------------
today_date <- Sys.Date()
schedule <- wehoop::load_wnba_schedule(seasons = 2026) %>%
  filter(as.Date(game_date) == today_date)

if (nrow(schedule) == 0) {
  if (!dir.exists("predictions")) dir.create("predictions")
  writeLines("# 📊 Daily Report\nNo games scheduled for today.", paste0("predictions/plays_", today_date, ".md"))
  quit(status = 0)
}

# ------------------------------------------------------------------------------
# 3. HIGH-LEVEL GLOBAL ROI (Dashboard Header)
# ------------------------------------------------------------------------------
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

# ------------------------------------------------------------------------------
# 4. COMPRESS ALL 7,500+ ROWS INTO MACHINE-READABLE INFERENCE VALUES
# ------------------------------------------------------------------------------
message("Compressing backlog trends...")

# Insight A: Full Backlog Player Profiles
player_backlog_profiles <- props_history %>%
  group_by(player) %>%
  summarize(
    total_tracked_bets = n(),
    overall_win_rate   = sum(result %in% c("Win", "W"), na.rm = TRUE) / n(),
    accumulated_profit = sum(profit, na.rm = TRUE),
    avg_model_dtm      = mean(dtm, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(total_tracked_bets >= 3)

# Insight B: Full Backlog Strategic Systems
system_backlog_profiles <- props_history %>%
  group_by(bet_type, side) %>%
  summarize(
    total_system_bets = n(),
    system_win_rate   = sum(result %in% c("Win", "W"), na.rm = TRUE) / n(),
    system_net_profit = sum(profit, na.rm = TRUE),
    system_roi        = (system_net_profit / total_system_bets) * 100,
    .groups = "drop"
  )

# Insight C: Active 30-Bet Momentum Layer
recent_30_momentum <- props_history %>%
  tail(30) %>%
  select(player, bet_type, side, line, dtm, result, profit)

# ------------------------------------------------------------------------------
# 5. CONSTRUCT COMPACT AI DATA PAYLOAD AND TRANSMIT
# ------------------------------------------------------------------------------
# FIX: Handle variation in table name updates between 'matchup' and 'name'
matchups_text <- if("matchup" %in% names(schedule)) {
  paste(schedule$matchup, collapse = ", ")
} else {
  paste(schedule$name, collapse = ", ")
}

system_prompt <- "You are a specialized risk-management AI for a sports analytics firm. Your goal is utilizing heavily aggregated data layers to isolate sports betting inefficiencies."

user_prompt <- paste0(
  "Today's Matchups: ", matchups_text, "\n\n",
  "--- TIMELINE PERFORMANCE CONTEXT ---\n", jsonlite::toJSON(roi_summary, auto_unbox = TRUE), "\n\n",
  "--- BACKLOG DATA INSIGHT A: PLAYER PROFILES (All-Time Record) ---\n", jsonlite::toJSON(player_backlog_profiles, auto_unbox = TRUE), "\n\n",
  "--- BACKLOG DATA INSIGHT B: SYSTEM PROFILES (All-Time Category Trends) ---\n", jsonlite::toJSON(system_backlog_profiles, auto_unbox = TRUE), "\n\n",
  "--- ACTIVE 30-BET TREND SNAPSHOT ---\n", jsonlite::toJSON(recent_30_momentum, auto_unbox = TRUE), "\n\n",
  "Instructions:\n",
  "1. CRITICAL: Start with the Markdown dashboard summary detailing Season-Long, Last 30, and Last 14 Days metrics.\n",
  "2. Cross-reference today's active matchups against our entire processed historical backlog dataset.\n",
  "3. Identify 3 optimal plays where today's target fits into an all-time profitable player track record AND an all-time profitable category system.\n",
  "4. Provide a clear 2-sentence mathematical defense for each play contrasting long-term baseline results against the recent 30-bet momentum layer."
)

# API Post Request Execution
api_key <- Sys.getenv("ANTHROPIC_API_KEY")
if (api_key == "") stop("CRITICAL: ANTHROPIC_API_KEY environment variable is missing!")

message("Sending data payload to Claude Sonnet 5...")
req <- request("https://anthropic.com") %>%
  req_headers(
    `x-api-key`         = api_key,
    `anthropic-version` = "2023-06-01",
    `content-type`      = "application/json"
  ) %>%
  req_body_json(list(
    model      = "claude-sonnet-5",
    max_tokens = 1200,
    system     = system_prompt,
    messages   = list(list(role = "user", content = user_prompt))
  ))

response <- req_perform(req)
body     <- resp_body_json(response)
ai_play_selections <- body$content[[1]]$text # Explicit vector index unpacking fix

if (!dir.exists("predictions")) dir.create("predictions")
writeLines(ai_play_selections, paste0("predictions/plays_", today_date, ".md"))
message("Success! Selections written.")
