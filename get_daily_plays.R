# ==============================================================================
# PIPELINE STEP 2: AI MATCHUP ENGINE (TIME-ZONE FIXED PRODUCTION VERSION)
# ==============================================================================
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(wehoop, dplyr, readr, httr2, jsonlite, stringr, lubridate)

# ------------------------------------------------------------------------------
# 1. LOAD DATASET AND FORCE ALL HEADERS TO LOWERCASE
# ------------------------------------------------------------------------------
message("Loading full dataset backlog and forcing headers to lowercase...")
raw_props <- readr::read_csv("data/tracked_props.csv", show_col_types = FALSE)
colnames(raw_props) <- tolower(stringr::str_replace_all(colnames(raw_props), " ", "_"))

props_history <- raw_props %>%
  mutate(
    Parsed_Date = as.Date(date),
    profit = as.numeric(stringr::str_remove_all(as.character(profit), "[\\$, ]")),
    dtm    = as.numeric(stringr::str_remove_all(as.character(dtm), "[\\$, ]")),
    win_probability = as.numeric(stringr::str_remove_all(as.character(win_probability), "[% ]")),
    win_probability = ifelse(win_probability > 1, win_probability / 100, win_probability)
  ) %>%
  mutate(
    profit = ifelse(is.na(profit), 0, profit),
    dtm    = ifelse(is.na(dtm), 0, dtm)
  )

# ------------------------------------------------------------------------------
# 2. CHECK TODAY'S SLATE BEFORE RUNNING ADVANCED MATH (TIME-ZONE FIX)
# ------------------------------------------------------------------------------
# TIME-ZONE FIX: Force the cloud runner to check dates based on New York local time, not UTC
today_date <- as.Date(lubridate::with_tz(Sys.time(), tzone = "America/New_York"))
message(paste("Checking local New York date schedule for:", today_date))

full_schedule <- wehoop::load_wnba_schedule(seasons = 2026) %>%
  mutate(game_date = as.Date(game_date))

today_schedule <- full_schedule %>%
  filter(game_date == today_date)

# FAIL-SAFE SCHEDULE GUARD
if (nrow(today_schedule) == 0) {
  if (!dir.exists("predictions")) dir.create("predictions")
  
  roi_summary <- props_history %>%
    summarize(
      bets_30   = sum(Parsed_Date >= (today_date - 30), na.rm = TRUE), 
      profit_30 = sum(ifelse(Parsed_Date >= (today_date - 30), profit, 0), na.rm = TRUE), 
      roi_30    = (profit_30 / max(bets_30, 1)) * 100,
      
      bets_14   = sum(Parsed_Date >= (today_date - 14), na.rm = TRUE), 
      profit_14 = sum(ifelse(Parsed_Date >= (today_date - 14), profit, 0), na.rm = TRUE), 
      roi_14    = (profit_14 / max(bets_14, 1)) * 100
    )
  
  no_game_report <- paste0(
    "# 📊 Short-Term Model Performance Dashboard\n\n",
    "| Timeline | Total Tracked Bets | Total Profit (Units) | Return on Investment (ROI) |\n",
    "| :--- | :--- | :--- | :--- |\n",
    "| **Last 30 Days** | ", roi_summary$bets_30, " | ", round(roi_summary$profit_30, 2), " | **", round(roi_summary$roi_30, 2), "%** |\n",
    "| **Last 14 Days** | ", roi_summary$bets_14, " | ", round(roi_summary$profit_14, 2), " | **", round(roi_summary$roi_14, 2), "%** |\n\n",
    "## 📅 Daily Slate Alert\nNo WNBA games scheduled for today. Filter metrics updated successfully."
  )
  
  writeLines(no_game_report, paste0("predictions/plays_", today_date, ".md"))
  message("Success: No games today. Only 30 and 14-day header metrics saved.")
  quit(status = 0)
}

# ------------------------------------------------------------------------------
# 3. LOAD PBP DATA AND GENERATE LIVE OFFENSIVE/DEFENSIVE METRICS
# ------------------------------------------------------------------------------
message("Calculating Team Efficiency and Defensive Matchup Rankings...")
pbp_season <- readr::read_csv("data/wnba_pbp_2026.csv", show_col_types = FALSE)

team_defense <- pbp_season %>%
  filter(scoring_play == TRUE) %>%
  group_by(opponent = away_team_name) %>% 
  summarize(pts_allowed = sum(score_value, na.rm = TRUE), .groups = "drop")

team_games <- pbp_season %>%
  group_by(home_team_name) %>%
  summarize(games_played = n_distinct(game_id), .groups = "drop")

team_rankings <- pbp_season %>%
  filter(scoring_play == TRUE) %>%
  group_by(team = home_team_name) %>%
  summarize(total_pts_scored = sum(score_value, na.rm = TRUE), .groups = "drop") %>%
  left_join(team_defense, by = c("team" = "opponent")) %>%
  left_join(team_games, by = c("team" = "home_team_name")) %>%
  mutate(
    off_ppg = total_pts_scored / games_played,
    def_ppg_allowed = pts_allowed / games_played,
    off_rank = min_rank(desc(off_ppg)),
    def_rank = min_rank(def_ppg_allowed)
  ) %>%
  select(team, off_rank, def_rank, off_ppg, def_ppg_allowed)

# ------------------------------------------------------------------------------
# 4. CALCULATE REST AND BACK-TO-BACK FATIGUE VARIABLES
# ------------------------------------------------------------------------------
message("Calculating team rest advantages and back-to-back schedules...")
historical_dates <- full_schedule %>% filter(game_date <= today_date)

get_last_game_date <- function(team_name, current_date) {
  last_game <- historical_dates %>%
    filter(game_date < current_date & (home_name == team_name | away_name == team_name)) %>%
    arrange(desc(game_date)) %>%
    head(1)
  if (nrow(last_game) == 0) return(current_date - 5)
  return(last_game$game_date)
}

matchup_metrics <- list()
for(i in 1:nrow(today_schedule)) {
  h_team <- today_schedule$home_name[i]
  a_team <- today_schedule$away_name[i]
  
  h_last_date <- get_last_game_date(h_team, today_date)
  a_last_date <- get_last_game_date(a_team, today_date)
  
  h_rest <- as.numeric(today_date - h_last_date) - 1
  a_rest <- as.numeric(today_date - a_last_date) - 1
  
  h_rank <- team_rankings %>% filter(team == h_team)
  a_rank <- team_rankings %>% filter(team == a_team)
  
  matchup_metrics[[i]] <- tibble(
    home_team     = h_team,
    away_team     = a_team,
    home_off_rank = ifelse(nrow(h_rank) > 0, h_rank$off_rank, 6),
    home_def_rank = ifelse(nrow(h_rank) > 0, h_rank$def_rank, 6),
    away_off_rank = ifelse(nrow(a_rank) > 0, a_rank$off_rank, 6),
    away_def_rank = ifelse(nrow(a_rank) > 0, a_rank$def_rank, 6),
    home_days_rest = h_rest,
    away_days_rest = a_rest,
    home_is_b2b    = ifelse(h_rest == 0, TRUE, FALSE),
    away_is_b2b    = ifelse(a_rest == 0, TRUE, FALSE)
  )
}
matchup_metrics_df <- bind_rows(matchup_metrics)

# ------------------------------------------------------------------------------
# 5. AGGREGATE BOTH THE SNAPSHOT HEADER AND THE ALL-TIME BACKLOG
# ------------------------------------------------------------------------------
message("Aggregating timelines and compressing full 7,500+ row backlog...")

# High-Level Header ROI (30 and 14 days only)
roi_summary <- props_history %>%
  summarize(
    bets_30       = sum(Parsed_Date >= (today_date - 30), na.rm = TRUE), 
    profit_30     = sum(ifelse(Parsed_Date >= (today_date - 30), profit, 0), na.rm = TRUE), 
    roi_30        = (profit_30 / max(bets_30, 1)) * 100,
    
    bets_14       = sum(Parsed_Date >= (today_date - 14), na.rm = TRUE), 
    profit_14     = sum(ifelse(Parsed_Date >= (today_date - 14), profit, 0), na.rm = TRUE), 
    roi_14        = (profit_14 / max(bets_14, 1)) * 100
  )

player_backlog_profiles <- props_history %>%
  group_by(player) %>%
  summarize(
    total_tracked_bets = n(), 
    overall_win_rate = sum(result %in% c("Win", "W"), na.rm = TRUE) / n(), 
    accumulated_profit = sum(profit, na.rm = TRUE), 
    avg_model_dtm = mean(dtm, na.rm = TRUE), 
    .groups = "drop"
  ) %>%
  filter(total_tracked_bets >= 3)

system_backlog_profiles <- props_history %>%
  group_by(bet_type, side) %>%
  summarize(
    total_system_bets = n(), 
    system_win_rate = sum(result %in% c("Win", "W"), na.rm = TRUE) / n(), 
    system_net_profit = sum(profit, na.rm = TRUE), 
    system_roi = (system_net_profit / total_system_bets) * 100, 
    .groups = "drop"
  )

recent_30_momentum <- props_history %>%
  tail(30) %>%
  select(player, bet_type, side, line, dtm, result, profit)

# ------------------------------------------------------------------------------
# 5b. LOAD TODAY'S SPORTSBOOK ALT LINES (OPTIONAL, MANUALLY UPLOADED CSV)
# ------------------------------------------------------------------------------
odds_path <- "data/todays_odds.csv"
todays_odds_json <- "No sportsbook odds file uploaded for today."

if (file.exists(odds_path)) {
  message("Loading today's sportsbook odds from data/todays_odds.csv...")
  todays_odds <- readr::read_csv(odds_path, show_col_types = FALSE)
  colnames(todays_odds) <- tolower(stringr::str_replace_all(colnames(todays_odds), " ", "_"))
  todays_odds <- todays_odds %>% filter(as.Date(date) == today_date)

  if (nrow(todays_odds) == 0) {
    message("todays_odds.csv exists but has no rows for today's date - skipping alt line analysis.")
  } else {
    todays_odds_json <- jsonlite::toJSON(todays_odds, auto_unbox = TRUE)
  }
} else {
  message("No todays_odds.csv found - skipping alt line analysis.")
}

# ------------------------------------------------------------------------------
# 6. TRANSMIT INFERENCE PAYLOAD TO CLAUDE
# ------------------------------------------------------------------------------
# Build matchup text regardless of which schedule columns exist
if ("matchup" %in% names(today_schedule)) {

  matchups_text <- paste(today_schedule$matchup, collapse = ", ")

} else if ("name" %in% names(today_schedule)) {

  matchups_text <- paste(today_schedule$name, collapse = ", ")

} else if (all(c("away_name", "home_name") %in% names(today_schedule))) {

  matchups_text <- paste(
    paste(today_schedule$away_name, "at", today_schedule$home_name),
    collapse = ", "
  )

} else if ("short_name" %in% names(today_schedule)) {

  matchups_text <- paste(today_schedule$short_name, collapse = ", ")

} else {

  matchups_text <- "Today's WNBA Schedule"

}

system_prompt <- "You are a specialized risk-management AI for a sports betting syndicate. Your expertise is cross-referencing full historical backlog arrays against recent momentum shifts."

user_prompt <- paste0(
  "--- TODAY'S SCHEDULE, EFFICIENCY RANKINGS, AND FATIGUE METRICS ---\n", jsonlite::toJSON(matchup_metrics_df, auto_unbox = TRUE), "\n\n",
  "--- PORTFOLIO TIMELINE ROI SUMMARY (30 & 14 DAYS ONLY) ---\n", jsonlite::toJSON(roi_summary, auto_unbox = TRUE), "\n\n",
  "--- FULL ALL-TIME BACKLOG PLAYER BASELINES (7,500+ ROWS COMPRESSED) ---\n", jsonlite::toJSON(player_backlog_profiles, auto_unbox = TRUE), "\n\n",
  "--- FULL ALL-TIME BACKLOG SYSTEM PROP type ROIs (7,500+ ROWS COMPRESSED) ---\n", jsonlite::toJSON(system_backlog_profiles, auto_unbox = TRUE), "\n\n",
  "--- CURRENT ACTIVE MOMENTUM SNAPSHOT ---\n", jsonlite::toJSON(recent_30_momentum, auto_unbox = TRUE), "\n\n",
  "--- TODAY'S SPORTSBOOK ALT LINES ---\n", todays_odds_json, "\n\n",
  "Instructions:\n",
  "1. Start your response with a clean Markdown dashboard header grid tracking our overall portfolio performance (Total Bets, Profit, and ROI) for Last 30 Days and Last 14 Days ONLY.\n",
  "2. Select the top 3 high-value prop plays for today.\n",
  "3. Critically analyze lines where teams are on a back-to-back or have severely diminished rest metrics while on the road.\n",
  "4. For each play, use this exact structure:\n",
  "   * **Selection:** [Player Name - Prop Type - Over/Under - Line]\n",
  "   * **Matchup, Rest & Travel Matrix:** Detail how today's defensive rankings combined with the team's travel and rest levels create a high-probability situational betting edge.\n",
  "   * **Historical System Context:** Defend using your all-time backlog data trends contrasted against the active 14-day momentum layer.\n",
  "5. If sportsbook alt lines were provided above, add an **Alt Line Value** bullet for each play naming the specific book and line (using only the books present in the data - do not assume any particular set of books) offering the best value versus your projection. If no odds data was provided, omit this bullet entirely - do not invent prices."
)

api_key <- Sys.getenv("ANTHROPIC_API_KEY")
if (api_key == "") stop("CRITICAL: ANTHROPIC_API_KEY environment variable is missing!")

payload <- list(
  model = "claude-sonnet-5",
  max_tokens = 4000,
  system = system_prompt,
  messages = list(list(role = "user", content = user_prompt))
)

message("Transmitting data to Claude Sonnet...")

req <- request("https://api.anthropic.com/v1/messages") %>%
  req_method("POST") %>%
  req_headers(
    `x-api-key` = api_key,
    `anthropic-version` = "2023-06-01",
    `content-type` = "application/json"
  ) %>%
  req_body_json(payload) %>%
  req_timeout(60) %>%
  req_error(is_error = function(resp) FALSE)

response <- req_perform(req)

# Stop immediately if Anthropic returns an API error, printing the
# response body so the actual reason for the rejection is visible in logs
if (resp_status(response) >= 400) {
  stop("Anthropic API error (HTTP ", resp_status(response), "): ", resp_body_string(response))
}

body <- resp_body_json(response)

text_blocks <- Filter(function(block) identical(block$type, "text"), body$content)
if (length(text_blocks) == 0) {
  stop("Anthropic API response contained no text block: ", jsonlite::toJSON(body, auto_unbox = TRUE))
}
if (identical(body$stop_reason, "max_tokens")) {
  stop("Anthropic response was truncated by max_tokens before finishing. Increase max_tokens.")
}
ai_play_selections <- text_blocks[[1]]$text

if (!dir.exists("predictions")) dir.create("predictions")
writeLines(ai_play_selections, paste0("predictions/plays_", today_date, ".md"))
message("Success! Selections written.")
