# Til siste blokk i notebook
rm(list = ls(all = TRUE))

# Add required packages for API calls
library(httr)
library(jsonlite)
library(tidyverse)

# Function to get fixtures from FPL API
get_fpl_fixtures <- function() {
  # Make API request
  response <- GET("https://fantasy.premierleague.com/api/fixtures/")
  
  # Check if request was successful
  if (http_status(response)$category != "Success") {
    stop("Failed to fetch fixtures from FPL API. Status code: ", status_code(response))
  }
  
  # Parse JSON response
  fixtures_json <- content(response, "text", encoding = "UTF-8")
  fixtures_data <- fromJSON(fixtures_json, flatten = TRUE)
  
  # Basic transformation to required format
  formatted_fixtures <- tibble(
    GW = fixtures_data$event,
    home_team_id = fixtures_data$team_h,
    away_team_id = fixtures_data$team_a,
    kickoff_time = fixtures_data$kickoff_time
  ) %>%
    # Filter out fixtures with no scheduled gameweek (NULL or NA values)
    filter(!is.na(GW)) %>%
    # Create the same format as your previous code
    transmute(
      GW = GW,
      # Create a row for home team
      team_id = home_team_id,
      opponent_id = away_team_id,
      is_home = 1
    ) %>%
    # Also create corresponding away team rows
    bind_rows(
      tibble(
        GW = fixtures_data$event,
        home_team_id = fixtures_data$team_h,
        away_team_id = fixtures_data$team_a
      ) %>%
        filter(!is.na(GW)) %>%
        transmute(
          GW = GW,
          team_id = away_team_id,
          opponent_id = home_team_id,
          is_home = 0
        )
    )
  
  # Convert team IDs to match your internal team IDs if needed
  # This depends on how your team IDs are structured compared to FPL's
  # You might need to create a mapping table
  
  return(formatted_fixtures)
}

# In your prediction code, replace the file reading with:
future_fixtures <- get_fpl_fixtures()
future_fixtures

# If you need to filter to specific gameweeks:
future_gws <- (last_gw + 1):(last_gw + forecast_horizon)
future_fixtures <- future_fixtures %>% filter(GW %in% future_gws)

cat("Loaded fixture data for gameweeks:", paste(unique(future_fixtures$GW), collapse=", "), "\n")
# 1. Get fixtures from API
raw_fixtures <- get_fpl_fixtures()

# 2. Get team mapping
team_mapping <- get_team_mapping()

# 3. Apply mapping to fixtures
future_fixtures <- raw_fixtures %>%
  left_join(team_mapping, by = c("team_id" = "team_id")) %>%
  left_join(team_mapping, by = c("opponent_id" = "team_id"), suffix = c("", "_opponent")) %>%
  rename(
    team_name = team_name,
    opponent_name = team_name_opponent
  )

# 4. Filter to relevant gameweeks
future_fixtures <- future_fixtures %>%
  filter(GW %in% future_gws)

# 5. Now you can match your player data with fixtures using either ID or name
# If your dataset uses team names rather than IDs:
player_team_mapping <- alternativsammensatt %>%
  group_by(player_id) %>%
  slice_max(GW) %>%
  select(player_id, team) %>%  # 'team' is the team name in your dataset
  ungroup()

# In your forecast loop, join using the team name
player_fixtures <- active_players %>%
  left_join(player_team_mapping, by = "player_id") %>%
  left_join(future_fixtures, by = c("team" = "team_name", "GW" = "GW"))



# Function to get team mapping from FPL API
get_team_mapping <- function() {
  # Make API request to bootstrap-static endpoint which contains team data
  response <- GET("https://fantasy.premierleague.com/api/bootstrap-static/")
  
  # Check if request was successful
  if (http_status(response)$category != "Success") {
    stop("Failed to fetch bootstrap data from FPL API. Status code: ", status_code(response))
  }
  
  # Parse JSON response
  bootstrap_json <- content(response, "text", encoding = "UTF-8")
  bootstrap_data <- fromJSON(bootstrap_json, flatten = TRUE)
  
  # Extract team data
  team_mapping <- tibble(
    team_id = bootstrap_data$teams$id,
    team_name = bootstrap_data$teams$name,
    team_short_name = bootstrap_data$teams$short_name
  )
  
  return(team_mapping)
}

# Get the team mapping
team_mapping <- get_team_mapping()

# Apply mapping to your fixtures data
future_fixtures_with_names <- future_fixtures %>%
  # Map team_id to team name
  left_join(team_mapping, by = c("team_id" = "team_id")) %>%
  # Map opponent_id to opponent name
  left_join(team_mapping, by = c("opponent_id" = "team_id"), suffix = c("", "_opponent")) %>%
  # Rename columns for clarity
  rename(
    team_name = team_name,
    team_short = team_short_name,
    opponent_name = team_name_opponent,
    opponent_short = team_short_name_opponent
  )

# Display the fixtures with team names
head(future_fixtures_with_names)
