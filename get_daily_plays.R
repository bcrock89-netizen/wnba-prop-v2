# ==============================================================================
# PIPELINE STEP 2: AI MATCHUP ENGINE (WITH EFFICIENCY, REST, & TRAVEL ANALYSIS)
# ==============================================================================
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(wehoop, dplyr, readr, httr2, jsonlite, stringr)

# ------------------------------------------------------------------------------
# 1. LOAD BACKLOG DATA AND SCRUB NUMBERS
# ------------------------------------------------------------------------------
message("Loading historical tracking backlog...")
raw_data <- readr::read_csv("data/tracked_props.csv", skip = 1, col_names = FALSE, show_col_types = FALSE)

colnames(raw_data) <- c(
  "date", "player", "bet_type", "side", "line", "odds", 
  "win_probability", "dtm", "result", "projection", 
  "stat_value", "profit", "be_prob", "day", "month"
)

props_history <- raw_data %>%
  mutate(
    Parsed_Date = as.Date(date),
    profit = as.numeric(stringr::str_remove_all(as.character(profit), "[\\$, ]")),
    dtm    = as.numeric(stringr::str_remove_all(as.character(dtm), "[\\$, ]")),
    win_probability = as.numeric(stringr::str_remove_all(as.character(win_probability), "[% ]")),
    win_probability = ifelse(win_probability > 1, win_probability / 100, win_probability)
  )

# ------------------------------------------------------------------------------
# 2. LOAD PBP DATA AND GENERATE LIVE OFFENSIVE/DEFENSIVE METRICS
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
# 3. CALCULATE REST AND BACK-TO-BACK FATIGUE VARIABLES
# ------------------------------------------------------------------------------
message("Calculating team rest advantages and back-to-back schedules...")
today_date <- Sys.Date()

# Load the comprehensive season schedule to trace historical calendar logs
full_schedule <- wehoop::load_wnba_schedule(seasons = 2026) %>%
  mutate(game_date = as.Date(game_date))

# Isolate all completed or scheduled games prior to or including today
historical_dates <- full_schedule %>%
  filter(game_date <= today_date)

# Function to get the last game date for a team prior to today
get_last_game_date <- function(team_name, current_date) {
  last_game <- historical_dates %>%
    filter(game_date < current_date & (home_name == team_name | away_name == team_name)) %>%
    arrange(desc(game_date)) %>%
    head(1)
  
  if (nrow(last_game) == 0) return(current_date - 5) # Default baseline if it's the season opener
  return(last_game$game_date)
}

# Isolate today's active schedule matrix
today_schedule <- full_schedule %>%
  filter(game_date == today_date)

if (nrow(today_schedule) == 0) {
  if (!dir.exists("predictions")) dir.create("predictions")
  writeLines("# 📊 Daily Report\nNo games scheduled for today.", paste0("predictions/plays_", today_date, ".md"))
  quit(status = 0)
}

# Loop through today's matchups and dynamically inject rest timelines
matchup_metrics <- list()
for(i in 1:nrow(today_schedule)) {
  h_team <- today_schedule$home_name[i]
  a_team <- today_schedule$away_name[i]
  
  h_last_date <- get_last_game_date(h_team, today_date)
  a_last_date <- get_last_game_date(a_team, today_date)
  
  # Days of rest calculation
  h_rest <- as.numeric(today_date - h_last_date) - 1
  a_rest <- as.numeric(today_date - a_last_date) - 1
  
  # Fetch corresponding team rankings
  h_rank <- team_rankings %>% filter(team == h_team)
  a_rank <- team_rankings %>% filter(team == a_team)
  
  matchup_metrics[[i]] <- tibble(
    home_team     = h_team,
    away_team     = a_team,
    home_off_rank = ifelse(nrow(h_rank) > 0, h_rank$off_rank, 6),
    home_def_rank = ifelse(nrow(h_rank) > 0, h_rank$def_rank, 6),
    away_off_rank = ifelse(nrow(a_rank) > 0, a_rank$off_rank, 6),
    away_def_rank = ifelse(nrow(a_rank) > 0, a_rank$def_rank, 6),
    # FATIGUE FACTORS:
    home_days_rest = h_rest,
    away_days_rest = a_rest,
    home_is_b2b    = ifelse(h_rest == 0, TRUE, FALSE),
    away_is_b2b    = ifelse(a_rest == 0, TRUE, FALSE)
  )
}
matchup_metrics_df <- bind_rows(matchup_metrics)

# ------------------------------------------------------------------------------
# 4. COMPRESS BACKLOG PROFILES (Timeline summaries)
# ------------------------------------------------------------------------------
roi_summary <- props_history %>%
  summarize(
    season_bets = n(), season_profit = sum(profit, na.rm = TRUE), season_roi = (season_profit / season_bets) * 100,
    bets_30 = sum(Parsed_Date >= (today_date - 30), na.rm = TRUE), profit_30 = sum(ifelse(Parsed_Date >= (today_date - 30), profit, 0), na.rm = TRUE), roi_30 = (profit_30 / max(bets_30, 1)) * 100,
    bets_14 = sum(Parsed_Date >= (today_date - 14), na.rm = TRUE), profit_14 = sum(ifelse(Parsed_Date >= (today_date - 14), profit, 0), na.rm = TRUE), roi_14 = (profit_14 / max(bets_14, 1)) * 100
  )

player_backlog_profiles <- props_history %>%
  group_by(player) %>%
  summarize(total_tracked_bets = n(), overall_win_rate = sum(result %in% c("Win", "W"), na.rm = TRUE) / n(), accumulated_profit = sum(profit, na.rm = TRUE), avg_model_dtm = mean(dtm, na.rm = TRUE), .groups = "drop" ) %>%
  filter(total_tracked_bets >= 3)

system_backlog_profiles <- props_history %>%
  group_by(bet_type, side) %>%
  summarize(total_system_bets = n(), system_win_rate = sum(result %in% c("Win", "W"), na.rm = TRUE) / n(), system_net_profit = sum(profit, na.rm = TRUE), system_roi = (system_net_profit / total_system_bets) * 100, .groups = "drop")

recent_30_momentum <- props_history %>% tail(30) %>% select(player, bet_type, side, line, dtm, result, profit)

# ------------------------------------------------------------------------------
# 5. REST & TRAVEL POWERED AI PROMPT
# ------------------------------------------------------------------------------
system_prompt <- "You are a specialized risk-management AI for a sports betting syndicate. Your expertise is cross-referencing all-time trend data against raw situational fatigue variables."

user_prompt <- paste0(
  "--- TODAY'S SCHEDULE, EFFICIENCY RANKINGS, AND FATIGUE METRICS ---\n", jsonlite::toJSON(matchup_metrics_df, auto_unbox = TRUE), "\n\n",
  "--- PORTFOLIO TIMELINE ROI SUMMARY ---\n", jsonlite::toJSON(roi_summary, auto_unbox = TRUE), "\n\n",
  "--- HISTORICAL SPREADSHEET PLAYER BASELINES ---\n", jsonlite::toJSON(player_backlog_profiles, auto_unbox = TRUE), "\n\n",
  "--- HISTORICAL STRATEGIC SYSTEM PROP type ROIs ---\n", jsonlite::toJSON(system_backlog_profiles, auto_unbox = TRUE), "\n\n",
  "--- CURRENT 30-BET ACTIVE MOMENTUM SNAPSHOT ---\n", jsonlite::toJSON(recent_30_momentum, auto_unbox = TRUE), "\n\n",
  "Instructions:\n",
  "1. Start your response with a clean Markdown dashboard header grid tracking our overall portfolio performance (Total Bets, Profit, and ROI) for Season-Long, Last 30, and Last 14 Days.\n",
  "2. Select the top 3 high-value prop plays for today. You must prioritize situational fatigue dynamics.\n",
  "3. Critically analyze lines where teams are on a back-to-back (`is_b2b: true`) or have severely diminished rest metrics (`days_rest <= 1`) while on the road, as these variables frequently lead to reduced athletic volume, slower paces, or defensive breakdowns.\n",
  "4. For each play, use this exact structure:\n",
  "   * **Selection:** [Player Name - Prop Type - Over/Under - Line]\n",
  "   * **Matchup, Rest & Travel Matrix:** Detail how today's defensive rankings combined with the team's travel and rest levels create a high-probability situational betting edge.\n",
  "   * **Historical System Context:** Defend using your all-time backlog data trends paired with the active 14-day momentum layer."
)

# API Call Execution
api_key <- Sys.getenv("ANTHROPIC_API_KEY")
if (api_key == "") stop("CRITICAL: ANTHROPIC_API_KEY environment variable is missing!")

message("Transmitting fatigue-aware data to Claude...")
req <- request("https://anthropic.com") %>%
  req_headers(`x-api-key` = api_key, `anthropic-version` = "2023-06-01", `content-type` = "application/json") %>%
  req_body_json(list(
    model       = "claude-3-5-sonnet-20241022",
    max_tokens  = 1200,
    temperature = 0.2,
    system      = system_prompt,
    messages    = list(list(role = "user", content = user_prompt))
  ))

response <- req_perform(req)
body     <- resp_body_json(response)
ai_play_selections <- body$content[]$text

if (!dir.exists("predictions")) dir.create("predictions")
writeLines(ai_play_selections, paste0("predictions/plays_", today_date, ".md"))
message("Success! Fatigue-aware selections generated.")
