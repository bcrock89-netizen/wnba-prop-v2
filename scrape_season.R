# scrape_season.R
# Install/Load required packages
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(wehoop, dplyr, readr)

# 1. Fetch current 2026 WNBA play-by-play or schedule data
# Replace with load_wbb_pbp(2026) if you want Women's College Basketball
tryCatch({
  message("Fetching current 2026 season data...")
  season_data <- wehoop::load_wnba_pbp(seasons = 2026)
  
  # 2. Create data directory if it doesn't exist
  if (!dir.exists("data")) dir.create("data")
  
  # 3. Save to a CSV file in your repository
  readr::write_csv(season_data, "data/wnba_pbp_2026.csv")
  message("Data successfully saved!")
}, error = function(e) {
  message("Error encountered during scraping: ", e$message)
  quit(status = 1)
})
