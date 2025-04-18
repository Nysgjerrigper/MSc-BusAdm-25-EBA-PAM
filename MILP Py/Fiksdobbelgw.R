# Slå sammen doble gameweeks observasjoner.
rm(list = ls(all = TRUE))

library(tidyverse)

prognoser <- read_csv("R Forecast\\Predicted, E-20, W-4.csv")

glimpse(prognoser)


# This groups by player and GW. summarise() then processes each group.
# - For single GWs (1 row per group): sum() returns the single score, first() returns the single detail.
# - For double GWs (2+ rows per group): sum() adds the scores, first() takes details from the first row.
prognoser_agg <- prognoser |> 
    select(player_id, GW, name, position, team, predicted_total_points, value) |> 
    group_by(player_id, GW) |> 
    summarise(
        # Sum points. Handles both single and double GWs correctly.
        predicted_total_points = sum(predicted_total_points, na.rm = TRUE), # Use na.rm=TRUE
        # Keep the first occurrence of details. Handles both single/double GWs.
        name = first(name),
        position = first(position),
        team = first(team),
        value = first(value), # Or mean(value, na.rm = TRUE) if preferred for DGWs
        .groups = 'drop' # Ungroup after summarising
    ) |> 
    arrange(GW, player_id)

print(paste("Final rows after aggregation:", nrow(prognoser_agg)))
glimpse(prognoser_agg)

prognoser_agg |> filter(GW == 105)

# Explanation: The final row count is the number of unique player-GW pairs
# in the input CSV. The aggregation correctly combines DGWs within that data.
# distinct() cannot sum points, so summarise() is the appropriate function.




