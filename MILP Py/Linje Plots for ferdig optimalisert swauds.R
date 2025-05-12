# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr) # For str_extract and str_pad

# Set working directory (adjust path as needed)
setwd("C:/Users/peram/OneDrive/Skrivebord/Squad Opt Predicted Poeng")

# Initialize an empty data frame to store combined data
combined_data_lstm <- data.frame()

# Loop through the n files
for (i in 1:10) {
  file_name <- paste0("Squad Selection t-auto, W77-108,SHL", i, ".csv")
  temp_data <- read.csv(file_name)
  temp_data <- temp_data %>%
    select(gameweek, objective_gw, actual_lineup_points) %>%
    mutate(sub_horizon = paste0("SHL", i))
  combined_data_lstm <- rbind(combined_data_lstm, temp_data)
}

# --- FIX SUB-HORIZON ORDER FOR LSTM ---
# Extract numeric part and order
horizon_numbers_lstm <- as.integer(stringr::str_extract(unique(combined_data_lstm$sub_horizon), "\\d+"))
ordered_levels_lstm <- paste0("SHL", sort(horizon_numbers_lstm))

combined_data_lstm <- combined_data_lstm %>%
  mutate(sub_horizon = factor(sub_horizon, levels = ordered_levels_lstm))

# Reshape data to long format for ggplot
plot_data_lstm <- combined_data_lstm %>%
  pivot_longer(cols = c(objective_gw, actual_lineup_points),
               names_to = "line_type",
               values_to = "points") %>%
  mutate(line_type = recode(line_type,
                            "objective_gw" = "Objective GW",
                            "actual_lineup_points" = "Actual Lineup"))

# Create the original line plot
p1_lstm <- ggplot(plot_data_lstm, aes(x = gameweek, y = points, color = sub_horizon, linetype = line_type)) +
  geom_line(size = 0.8) +
  labs(title = "Gameweek Points by Sub-Horizon and Line Type (LSTM)",
       x = "Gameweek (GW)",
       y = "Points",
       color = "Sub-Horizon",
       linetype = "Line Type") +
  theme_gray() +
  theme(legend.position = "right")
print(p1_lstm)
ggsave("fantasy_points_plot_lstm.png", plot = p1_lstm, width = 10, height = 6) # Increased width for legend

# --- CUMULATIVE PLOT FOR LSTM DATA ---
cumulative_plot_data_lstm <- combined_data_lstm %>% # Use already factor-ordered combined_data_lstm
  group_by(sub_horizon) %>%
  arrange(gameweek) %>%
  mutate(cumulative_objective_points = cumsum(objective_gw)) %>%
  ungroup()

p_cumulative_lstm <- ggplot(cumulative_plot_data_lstm, aes(x = gameweek, y = cumulative_objective_points, color = sub_horizon)) +
  geom_line(size = 0.8) +
  labs(title = "Cumulative Objective GW Points by Sub-Horizon (LSTM)",
       x = "Gameweek (GW)",
       y = "Cumulative Objective GW Points",
       color = "Sub-Horizon") +
  theme_gray() +
  theme(legend.position = "right")
print(p_cumulative_lstm)
ggsave("cumulative_fantasy_points_plot_lstm.png", plot = p_cumulative_lstm, width = 10, height = 6)

end_values_lstm <- cumulative_plot_data_lstm %>%
  group_by(sub_horizon) %>%
  filter(gameweek == max(gameweek)) %>%
  select(sub_horizon, end_gameweek = gameweek, final_cumulative_points = cumulative_objective_points) %>%
  ungroup()
cat("\n--- End Cumulative Values for LSTM Data ---\n")

print(xtable::xtable(end_values_lstm))


# Actual ----
setwd("C:/Users/peram/OneDrive/Skrivebord/Squad Opt Faktisk Poeng")
combined_data_actual <- data.frame()

# Loop through the n files
for (i in 1:11) {
  file_name <- paste0("Squad Selection AUTO-MILP-GC-TOTAL_POINTS, GW77-108,SHL", i, ".csv")
  temp_data <- read.csv(file_name)
  temp_data <- temp_data %>%
    select(gameweek, objective_gw) %>%
    mutate(sub_horizon = paste0("SHL", i))
  combined_data_actual <- rbind(combined_data_actual, temp_data)
}

# --- FIX SUB-HORIZON ORDER FOR ACTUAL ---
# Extract numeric part and order
horizon_numbers_actual <- as.integer(stringr::str_extract(unique(combined_data_actual$sub_horizon), "\\d+"))
ordered_levels_actual <- paste0("SHL", sort(horizon_numbers_actual))

combined_data_actual <- combined_data_actual %>%
  mutate(sub_horizon = factor(sub_horizon, levels = ordered_levels_actual))


plot_data_actual <- combined_data_actual %>%
  pivot_longer(cols = objective_gw,
               names_to = "line_type",
               values_to = "points") %>%
  mutate(line_type = recode(line_type,
                            "objective_gw" = "Objective GW"))

p1_actual <- ggplot(plot_data_actual, aes(x = gameweek, y = points, color = sub_horizon, linetype = line_type)) +
  geom_line(size = 0.8) +
  labs(title = "Gameweek Points by Sub-Horizon (Actual Data)",
       x = "Gameweek (GW)",
       y = "Points",
       color = "Sub-Horizon",
       linetype = "Line Type") +
  theme_gray() +
  theme(legend.position = "right")
print(p1_actual)
ggsave("fantasy_points_plot_actual.png", plot = p1_actual, width = 10, height = 6)


# --- CUMULATIVE PLOT FOR ACTUAL DATA ---
cumulative_plot_data_actual <- combined_data_actual %>% # Use already factor-ordered combined_data_actual
  group_by(sub_horizon) %>%
  arrange(gameweek) %>%
  mutate(cumulative_objective_points = cumsum(objective_gw)) %>%
  ungroup()

p_cumulative_actual <- ggplot(cumulative_plot_data_actual, aes(x = gameweek, y = cumulative_objective_points, color = sub_horizon)) +
  geom_line(size = 0.5) +
  labs(title = "Cumulative Objective GW Points by Sub-Horizon (Actual Data)",
       x = "Gameweek (GW)",
       y = "Cumulative Objective GW Points",
       color = "Sub-Horizon") +
  theme_grey() +
  theme(legend.position = "right")
print(p_cumulative_actual)
ggsave("cumulative_fantasy_points_plot_actual.png", plot = p_cumulative_actual, width = 10, height = 6)

end_values_actual <- cumulative_plot_data_actual %>%
  group_by(sub_horizon) %>%
  filter(gameweek == max(gameweek)) %>%
  select(sub_horizon, end_gameweek = gameweek, final_cumulative_points = cumulative_objective_points) %>%
  ungroup()
cat("\n--- End Cumulative Values for Actual Data ---\n")
print(xtable::xtable(end_values_actual))
