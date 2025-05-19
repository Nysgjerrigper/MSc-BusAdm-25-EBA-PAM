# EDA ----
# Analysis of point distributions for players based on different criteria
rm(list = ls(all = TRUE))
# Load required packages for statistical description
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pastecs,tidyverse,ggridges, xtable, moments,
qqplotr)

# Create directory for saving plots
setwd("C:/Users/peram/Documents/test")
plot_directory <- "C:/Users/peram/Documents/test/R Forecast/EDA" 
dir.create(plot_directory, recursive = TRUE)


# Data Fetch ----
season_22_23 <- read_csv("Datasett/Sesong 22 til 23.csv")
season_23_24 <- read_csv("Datasett/Sesong 23 til 24.csv")
season_24_25 <- read_csv("Datasett/Sesong 24 til 25.csv")

# Define position order for consistent plotting
position_levels <- c("GK", "DEF", "MID", "FWD")

# Starting Players ----

## Create datasets of starting players ----

starters_22_23 <- season_22_23 |> 
  filter(starts == 1) |> 
  select(total_points, position, GW) |>
  mutate(position = factor(position, levels = position_levels))

starters_23_24 <- season_23_24 |> 
  filter(starts == 1) |> 
  select(total_points, position, GW) |>
  mutate(position = factor(position, levels = position_levels))

starters_24_25 <- season_24_25 |> 
  filter(starts == 1) |> 
  select(total_points, position, GW) |>
  mutate(position = factor(position, levels = position_levels)) 

## Display dataset structure ----
glimpse(starters_22_23)

## Create boxplots for point distributions by position for starters ----

plot1 <- ggplot(starters_22_23, aes(x = position, y = total_points)) + 
  geom_boxplot() +
  labs(title = "Boxplot for starting players in season 22/23 by position", 
       x = "Position", 
       y = "Observed points per week") +
  scale_y_continuous(breaks = seq(0, max(starters_22_23$total_points, na.rm = TRUE), 2))

plot2 <- ggplot(starters_23_24, aes(x = position, y = total_points)) +
  geom_boxplot() +
  labs(title = "Boxplot for starting players in season 23/24 by position", 
       x = "Position", 
       y = "Observed points per week") +
  scale_y_continuous(breaks = seq(0, max(starters_23_24$total_points, na.rm = TRUE), 2)) 

plot3 <- ggplot(starters_24_25, aes(x = position, y = total_points)) +
  geom_boxplot() +
  labs(title = "Boxplot for starting players in season 24/25 by position", 
       x = "Position", 
       y = "Observed points per week") +
  scale_y_continuous(breaks = seq(0, max(starters_24_25$total_points, na.rm = TRUE), 2)) 

## Display box plots ----
plot1;plot2;plot3

## Distribution density plots for starting players ----
# Create distribution plots with different colors by position
dist_plot1 <- ggplot(starters_22_23, aes(x = total_points, fill = position, color = position)) + 
  geom_density(alpha = 0.2) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Point distribution for starting players in season 22/23 by position", 
       x = "Total points", 
       y = "Density") +
  theme_grey() +
  scale_x_continuous(breaks = seq(0, max(starters_22_23$total_points, na.rm = TRUE), 2))

dist_plot2 <- ggplot(starters_23_24, aes(x = total_points, fill = position, color = position)) + 
  geom_density(alpha = 0.2) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Point distribution for starting players in season 23/24 by position", 
       x = "Total points", 
       y = "Density") +
  theme_grey() +
  scale_x_continuous(breaks = seq(0, max(starters_23_24$total_points, na.rm = TRUE), 2))

dist_plot3 <- ggplot(starters_24_25, aes(x = total_points, fill = position, color = position)) + 
  geom_density(alpha = 0.2) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Point distribution for starting players in season 24/25 by position", 
       x = "Total points", 
       y = "Density") +
  theme_grey() +
  scale_x_continuous(breaks = seq(0, max(starters_24_25$total_points, na.rm = TRUE), 2))

## Display density plots for starters
dist_plot1;dist_plot2;dist_plot3

# Atleaset 1 min of play time ----

## Create datasets of players with at least 1 minute ----
players_played_1 <- season_22_23 |> 
  filter(minutes >= 1) |> 
  select(total_points, position) |>
  mutate(position = factor(position, levels = position_levels))

players_played_2 <- season_23_24 |> 
  filter(minutes >= 1) |> 
  select(total_points, position) |>
  mutate(position = factor(position, levels = position_levels))

players_played_3 <- season_24_25 |> 
  filter(minutes >= 1) |> 
  select(total_points, position) |>
  mutate(position = factor(position, levels = position_levels))

## Create boxplots for players with atleast 1 min playtime ----
plot4 <- ggplot(players_played_1, aes(x = position, y = total_points)) + 
  geom_boxplot() +
  labs(title = "Boxplots for players with ≥1 minute of play time by position (22/23)", 
       x = "Position", 
       y = "Observed points per week") +
  scale_y_continuous(breaks = seq(0, max(players_played_1$total_points, na.rm = TRUE), 2))

plot5 <- ggplot(players_played_2, aes(x = position, y = total_points)) +
  geom_boxplot() +
  labs(title = "Boxplots for players with ≥1 minute of play time by position (23/24)", 
       x = "Position", 
       y = "Observed points per week") +
  scale_y_continuous(breaks = seq(0, max(players_played_2$total_points, na.rm = TRUE), 2))

plot6 <- ggplot(players_played_3, aes(x = position, y = total_points)) +
  geom_boxplot() +
  labs(title = "Boxplots for players with ≥1 minute of play time by position (24/25)", 
       x = "Position", 
       y = "Observed points per week") +
  scale_y_continuous(breaks = seq(0, max(players_played_3$total_points, na.rm = TRUE), 2))

# Display plots
plot4;plot5;plot6

## Create Distribution plots for players with atleast 1 minute ----
dist_plot4 <- ggplot(players_played_1, aes(x = total_points, fill = position, color = position)) + 
  geom_density(alpha = 0.2) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Point distribution for players with ≥1 minute (22/23)", 
       x = "Total points", 
       y = "Density") +
  theme_grey() +
  scale_x_continuous(breaks = seq(0, max(players_played_1$total_points, na.rm = TRUE), 2))

dist_plot5 <- ggplot(players_played_2, aes(x = total_points, fill = position, color = position)) + 
  geom_density(alpha = 0.2) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Point distribution for players with ≥1 minute (23/24)", 
       x = "Total points", 
       y = "Density") +
  theme_grey() +
  scale_x_continuous(breaks = seq(0, max(players_played_2$total_points, na.rm = TRUE), 2))

dist_plot6 <- ggplot(players_played_3, aes(x = total_points, fill = position, color = position)) + 
  geom_density(alpha = 0.2) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Point distribution for players with ≥1 minute (24/25)", 
       x = "Total points", 
       y = "Density") +
  theme_grey() +
  scale_x_continuous(breaks = seq(0, max(players_played_3$total_points, na.rm = TRUE), 2))

# Display density plots for players with atleast 1 min of play time
dist_plot4;dist_plot5;dist_plot6

# All players ----
## Create datasets of all players ----
all1 <- season_22_23 |> 
  select(total_points, position) |>
  mutate(position = factor(position, levels = position_levels))
all2 <- season_23_24 |> 
  select(total_points, position) |>
  mutate(position = factor(position, levels = position_levels))
all3 <- season_24_25 |> 
  select(total_points, position) |>
  mutate(position = factor(position, levels = position_levels)) |>
  na.omit()

## Create boxplots for all players ----

plot7 <- ggplot(all1, aes(x = position, y = total_points)) + 
  geom_boxplot() +
  labs(title = "Boxplots for all players by position (22/23)", 
       x = "Position", 
       y = "Observed points per week") +
  scale_y_continuous(breaks = seq(0, max(all1$total_points, na.rm = TRUE), 2))

plot8 <- ggplot(all2, aes(x = position, y = total_points)) +
  geom_boxplot() +
  labs(title = "Boxplots for all players by position (23/24)", 
       x = "Position", 
       y = "Observed points per week") +
  scale_y_continuous(breaks = seq(0, max(all2$total_points, na.rm = TRUE), 2))

plot9 <- ggplot(all3, aes(x = position, y = total_points)) +
  geom_boxplot() +
  labs(title = "Boxplots for all players by position (24/25)", 
       x = "Position", 
       y = "Observed points per week") +
  scale_y_continuous(breaks = seq(0, max(all3$total_points, na.rm = TRUE), 2))

plot7;plot8;plot9

## Create distribution density plots for all players ----

dist_plot7 <- ggplot(all1, aes(x = total_points, fill = position, color = position)) + 
  geom_density(alpha = 0.2) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Point distribution for all players by position (22/23)", 
       x = "Total points", 
       y = "Density") +
  theme_grey() +
  scale_x_continuous(breaks = seq(0, max(players_played_1$total_points, na.rm = TRUE), 2))

dist_plot8 <- ggplot(all2, aes(x = total_points, fill = position, color = position)) + 
  geom_density(alpha = 0.2) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Point distribution for all players by position (23/24)", 
       x = "Total points", 
       y = "Density") +
  theme_grey() +
  scale_x_continuous(breaks = seq(0, max(all2$total_points, na.rm = TRUE), 2))

dist_plot9 <- ggplot(all3, aes(x = total_points, fill = position, color = position)) + 
  geom_density(alpha = 0.2) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Point distribution for all players by position (24/25)", 
       x = "Total points", 
       y = "Density") +
  theme_grey() +
  scale_x_continuous(breaks = seq(0, max(all3$total_points, na.rm = TRUE), 2))

dist_plot7;dist_plot8;dist_plot9

# Export plots ----

## Save plots for starting players ----

## Save boxplots ----
ggsave(file.path(plot_directory, "Box Plot for starting players(POS) in season 22-23.png"), 
       plot = plot1, width = 8, height = 6)
ggsave(file.path(plot_directory, "Box Plot for starting players(POS) in season 23-24.png"), 
       plot = plot2, width = 8, height = 6)
ggsave(file.path(plot_directory, "Box Plot for starting players(POS) in season 24-25.png"), 
       plot = plot3, width = 8, height = 6)

### Save density plots for starters ----
ggsave(file.path(plot_directory, "Point density distribution for starting players 22-23.png"), 
       plot = dist_plot1, width = 10, height = 6)
ggsave(file.path(plot_directory, "Point density distribution for starting players 23-24.png"), 
       plot = dist_plot2, width = 10, height = 6)
ggsave(file.path(plot_directory, "Point density distribution for starting players 24-25.png"), 
       plot = dist_plot3, width = 10, height = 6)

##  Save plots for 1+ minutes players ----


### Save boxplots plots ----
ggsave(file.path(plot_directory, "Boxplots for players with 1+ playing time 22-23 by position.png"), 
       plot = plot3, width = 8, height = 6)
ggsave(file.path(plot_directory, "Boxplots for players with 1+ playing time 23-24 by position.png"), 
       plot = plot4, width = 8, height = 6)
ggsave(file.path(plot_directory, "Boxplots for players with 1+ playing time 24-25 by position.png"), 
       plot = plot4, width = 8, height = 6)

### Save density plots for 1+ minute players ----
ggsave(file.path(plot_directory, "Point density distribution for players with 1min+ 22-23 by position.png"), 
       plot = dist_plot4, width = 10, height = 6)
ggsave(file.path(plot_directory, "Point density distribution for players with 1min+ 23-24 by position.png"), 
       plot = dist_plot5, width = 10, height = 6)
ggsave(file.path(plot_directory, "Point density distribution for players with 1min+ 24-25 by position.png"), 
       plot = dist_plot6, width = 10, height = 6)

## Save plots for all players ----

### Save boxplots ----
ggsave(file.path(plot_directory, "Boxplots for all players by position 22-23.png"), 
       plot = plot7, width = 8, height = 6)
ggsave(file.path(plot_directory, "Boxplots for all players by position 23-24.png"), 
       plot = plot8, width = 8, height = 6)
ggsave(file.path(plot_directory, "Boxplots for all players by position 24-25.png"), 
       plot = plot9, width = 8, height = 6)

### Save density plots for all players ----
ggsave(file.path(plot_directory, "Point density distribution for all players 22-23 by position.png"), 
       plot = dist_plot7, width = 10, height = 6)
ggsave(file.path(plot_directory, "Point density distribution for all players 23-24 by position.png"), 
       plot = dist_plot8, width = 10, height = 6)
ggsave(file.path(plot_directory, "Point density distribution for all players 24-25 by position.png"), 
       plot = dist_plot9, width = 10, height = 6)

# Comparison by position across seasons ----

## For starters ----
combined_starters <- bind_rows(
  mutate(starters_22_23, season = "22/23"),
  mutate(starters_23_24, season = "23/24"),
  mutate(starters_24_25, season = "24/25")
)

box_compare_starter <- ggplot(combined_starters, aes(x = total_points, color = season)) + 
  geom_boxplot() +
  facet_wrap(~ position, scales = "free_y") +
  scale_color_manual(values = c("22/23" = "blue" , "23/24" = "navy", "24/25" = "grey")) +
  labs(title = "Comparing boxplots across seasons for starting players",
       x = "Total points",
       y = "Density") +
  theme_grey()

box_compare_starter

### Create Ridgeline Plot by Gameweek and Position ----

# Generate the ridgeline plot

gameweek_pos_ridgeline <- ggplot(combined_starters,
                                 aes(x = total_points,
                                     y = factor(GW), # Treat gameweek as a categorical variable on y-axis
                                     fill = factor(season))) + # Fill by position
  geom_density_ridges(alpha = 0.4) + # Use geom_density_ridges for overlapping densities
  scale_fill_manual(values = c("22/23" = "yellow", # Define colors for each position
                               "23/24" = "red",
                               "24/25" = "blue")) +
  labs(title = "Distribution of Total Points by Season for Starters",
       x = "Total Points",
       y = "Gameweek") +
  theme_ridges() + # Apply a ridgeline theme
  theme_grey() 
  theme(legend.position = "bottom") # Position the legend at the bottom

# Display the plot and save ridge for all players
gameweek_pos_ridgeline

#save
ggsave(file.path(plot_directory, "Gameweek_Ridgeline_Plot_by_Position.png"),
       plot = gameweek_pos_ridgeline, width = 12, height = 10)

## For 1+ minute players ----
combined_1_min <- bind_rows(
  mutate(players_played_1, season = "22/23"),
  mutate(players_played_2, season = "23/24"),
  mutate(players_played_3, season = "24/25")
)

box_compare_1_min <- ggplot(combined_1_min, aes(x = total_points, color = season)) + 
  geom_boxplot() +
  facet_wrap(~ position, scales = "free_y") +
  scale_color_manual(values = c("22/23" = "blue", "23/24" = "navy", "24/25" = "grey")) +
  labs(title = "Comparing boxplots across seasons for players with ≥1 minutes by position",
       x = "Total points",
       y = "Density") +
  theme_grey()

box_compare_1_min

## For all players ----

combined_all <- bind_rows(
  mutate(all1, season = "22/23"),
  mutate(all2, season = "23/24"),
  mutate(all3, season = "24/25")
)

box_compare_all <- ggplot(combined_all, aes(x = total_points, color = season)) + 
  geom_boxplot() +
  facet_wrap(~ position, scales = "free_y") +
  scale_color_manual(values = c("22/23" = "blue", "23/24" = "navy", "24/25" = "grey")) +
  labs(title = "Comparing boxplots across seasons for all players by position",
       x = "Total points",
       y = "Density") +
  theme_grey()

box_compare_all


# Save the combined comparison plots
ggsave(file.path(plot_directory, "Season comparison starters by position.png"), 
       plot = box_compare_starter, width = 10, height = 8)
ggsave(file.path(plot_directory, "Season comparison at least 1 min by position.png"), 
       plot = box_compare_1_min, width = 10, height = 8)
ggsave(file.path(plot_directory, "Season comparison all players by position.png"),
       plot = box_compare_all, width = 10, height = 8)


# Descriptives tables ----
# Function to calculate comprehensive descriptive statistics
calc_stats <- function(data) {
  stats <- stat.desc(data)
  # Add skewness and kurtosis 
  stats["skewness"] <- skewness(data, na.rm = TRUE)
  stats["kurtosis"] <- kurtosis(data, na.rm = TRUE)
  return(stats)
}

# Calculate statistics for all groups
stats_starter_1 <- calc_stats(starters_22_23$total_points)
stats_starter_2 <- calc_stats(starters_23_24$total_points)
stats_starter_3 <- calc_stats(starters_24_25$total_points)

stats_1min_1 <- calc_stats(players_played_1$total_points)
stats_1min_2 <- calc_stats(players_played_2$total_points)
stats_1min_3 <- calc_stats(players_played_3$total_points)

stats_all_1 <- calc_stats(all1$total_points)
stats_all_2 <- calc_stats(all2$total_points)
stats_all_3 <- calc_stats(all3$total_points)

# Create comparison dataframes
stats_comparison <- data.frame(
  Metric = names(stats_starter_1),
  "Starting players (22/23)" = stats_starter_1,
  "Starting players (23/24)" = stats_starter_2,
  "Starting players (24/25)" = stats_starter_3
)

stats_comparison2 <- data.frame(
  Metric = names(stats_1min_1),
  "Minimum 1 minute players (22/23)" = stats_1min_1,
  "Minimum 1 minute players (23/24)" = stats_1min_2,
  "Minimum 1 minute players (24/25)" = stats_1min_3
)

stats_comparison3 <- data.frame(
  Metric = names(stats_all_1),
  "All players (22/23)" = stats_all_1,
  "All players (23/24)" = stats_all_2,
  "All players (24/25)" = stats_all_3
)

print(stats_comparison)
print(stats_comparison2)
print(stats_comparison3)

# Create normal text tables for all three stats comparison dataframes
# For stats_comparison (starting players)
print(xtable(stats_comparison, 
caption = "Descriptive Statistics for Starting Players"),
file = file.path(plot_directory, "Descriptive_statistics_starters.txt"),
include.rownames = FALSE,
floating = TRUE,
latex.environments = "center",
type = "latex")

# For stats_comparison2 (players with at least 1 minute)
print(xtable(stats_comparison2, 
caption = "Descriptive Statistics for Players with At Least 1 Minute"),
file = file.path(plot_directory, "Descriptive_statistics_1min_players.txt"),
include.rownames = FALSE,
floating = TRUE,
latex.environments = "center",
type = "latex")

  # For stats_comparison3 (all players)
print(xtable(stats_comparison3, 
caption = "Descriptive Statistics for All Players"),
file = file.path(plot_directory, "Descriptive_statistics_all_players.txt"),
include.rownames = FALSE,
floating = TRUE,
latex.environments = "center",
type = "latex")

# QQ Plots to check normality (using ggplot) ----

## Function to create ggplot QQ plot with theme_grey
create_qq_plot <- function(data, title) {
  ggplot(data = data.frame(x = data), aes(sample = x)) +
    stat_qq_point(color = "blue", size = 2) +
    stat_qq_line(color = "red", size = 1) +
    labs(title = title,
         x = "Theoretical Quantiles",
         y = "Sample Quantiles") +
    theme_grey()
}

## QQ Plots for starters ----
# QQ Plot for starters 22/23
qq_starters_22_23 <- create_qq_plot(
  starters_22_23$total_points,
  "Q-Q Plot of Starters 22/23 Points"
)
ggsave(file.path(plot_directory, "QQ Plot of Starters 22-23 Points.png"), 
       plot = qq_starters_22_23, width = 8, height = 6)

# QQ Plot for starters 23/24
qq_starters_23_24 <- create_qq_plot(
  starters_23_24$total_points,
  "Q-Q Plot of Starters 23/24 Points"
)
ggsave(file.path(plot_directory, "QQ Plot of Starters 23-24 Points.png"), 
       plot = qq_starters_23_24, width = 8, height = 6)

# QQ Plot for starters 24/25
qq_starters_24_25 <- create_qq_plot(
  starters_24_25$total_points,
  "Q-Q Plot of Starters 24/25 Points"
)
ggsave(file.path(plot_directory, "QQ Plot of Starters 24-25 Points.png"), 
       plot = qq_starters_24_25, width = 8, height = 6)

## QQ Plots for players with at least 1 minute ----
# QQ Plot for players with 1+ min 22/23
qq_1min_22_23 <- create_qq_plot(
  players_played_1$total_points,
  "Q-Q Plot of Players with 1+ Min 22/23 Points"
)
ggsave(file.path(plot_directory, "QQ Plot of Players with 1+ Min 22-23 Points.png"), 
       plot = qq_1min_22_23, width = 8, height = 6)

# QQ Plot for players with 1+ min 23/24
qq_1min_23_24 <- create_qq_plot(
  players_played_2$total_points,
  "Q-Q Plot of Players with 1+ Min 23/24 Points"
)
ggsave(file.path(plot_directory, "QQ Plot of Players with 1+ Min 23-24 Points.png"), 
       plot = qq_1min_23_24, width = 8, height = 6)

# QQ Plot for players with 1+ min 24/25
qq_1min_24_25 <- create_qq_plot(
  players_played_3$total_points,
  "Q-Q Plot of Players with 1+ Min 24/25 Points"
)
ggsave(file.path(plot_directory, "QQ Plot of Players with 1+ Min 24-25 Points.png"), 
       plot = qq_1min_24_25, width = 8, height = 6)

## QQ Plots for all players ----
# QQ Plot for all players 22/23
qq_all_22_23 <- create_qq_plot(
  all1$total_points,
  "Q-Q Plot of All Players 22/23 Points"
)
ggsave(file.path(plot_directory, "QQ Plot of All Players 22-23 Points.png"), 
       plot = qq_all_22_23, width = 8, height = 6)

# QQ Plot for all players 23/24
qq_all_23_24 <- create_qq_plot(
  all2$total_points,
  "Q-Q Plot of All Players 23/24 Points"
)
ggsave(file.path(plot_directory, "QQ Plot of All Players 23-24 Points.png"), 
       plot = qq_all_23_24, width = 8, height = 6)

# QQ Plot for all players 24/25
qq_all_24_25 <- create_qq_plot(
  all3$total_points,
  "Q-Q Plot of All Players 24/25 Points"
)
ggsave(file.path(plot_directory, "QQ Plot of All Players 24-25 Points.png"), 
       plot = qq_all_24_25, width = 8, height = 6)

# Additional analysis - Points by position ----

## Position statistics for starters across all seasons ----
position_stats_starters_22_23 <- starters_22_23 |>
  group_by(position) |>
  summarize(
    Count = n(),
    Mean = mean(total_points, na.rm = TRUE),
    Median = median(total_points, na.rm = TRUE),
    SD = sd(total_points, na.rm = TRUE),
    Min = min(total_points, na.rm = TRUE),
    Max = max(total_points, na.rm = TRUE),
    Skewness = skewness(total_points, na.rm = TRUE),
    Kurtosis = kurtosis(total_points, na.rm = TRUE)
  )
postion_stats_starters_22_23
position_stats_starters_23_24 <- starters_23_24 |>
  group_by(position) |>
  summarize(
    Count = n(),
    Mean = mean(total_points, na.rm = TRUE),
    Median = median(total_points, na.rm = TRUE),
    SD = sd(total_points, na.rm = TRUE),
    Min = min(total_points, na.rm = TRUE),
    Max = max(total_points, na.rm = TRUE),
    Skewness = skewness(total_points, na.rm = TRUE),
    Kurtosis = kurtosis(total_points, na.rm = TRUE)
  )

position_stats_starters_24_25 <- starters_24_25 |>
  group_by(position) |>
  summarize(
    Count = n(),
    Mean = mean(total_points, na.rm = TRUE),
    Median = median(total_points, na.rm = TRUE),
    SD = sd(total_points, na.rm = TRUE),
    Min = min(total_points, na.rm = TRUE),
    Max = max(total_points, na.rm = TRUE),
    Skewness = skewness(total_points, na.rm = TRUE),
    Kurtosis = kurtosis(total_points, na.rm = TRUE)
  )

## Position statistics for players with at least 1 minute ----
position_stats_1min_22_23 <- players_played_1 |>
  group_by(position) |>
  summarize(
    Count = n(),
    Mean = mean(total_points, na.rm = TRUE),
    Median = median(total_points, na.rm = TRUE),
    SD = sd(total_points, na.rm = TRUE),
    Min = min(total_points, na.rm = TRUE),
    Max = max(total_points, na.rm = TRUE),
    Skewness = skewness(total_points, na.rm = TRUE),
    Kurtosis = kurtosis(total_points, na.rm = TRUE)
  )

position_stats_1min_23_24 <- players_played_2 |>
  group_by(position) |>
  summarize(
    Count = n(),
    Mean = mean(total_points, na.rm = TRUE),
    Median = median(total_points, na.rm = TRUE),
    SD = sd(total_points, na.rm = TRUE),
    Min = min(total_points, na.rm = TRUE),
    Max = max(total_points, na.rm = TRUE),
    Skewness = skewness(total_points, na.rm = TRUE),
    Kurtosis = kurtosis(total_points, na.rm = TRUE)
  )

position_stats_1min_24_25 <- players_played_3 |>
  group_by(position) |>
  summarize(
    Count = n(),
    Mean = mean(total_points, na.rm = TRUE),
    Median = median(total_points, na.rm = TRUE),
    SD = sd(total_points, na.rm = TRUE),
    Min = min(total_points, na.rm = TRUE),
    Max = max(total_points, na.rm = TRUE),
    Skewness = skewness(total_points, na.rm = TRUE),
    Kurtosis = kurtosis(total_points, na.rm = TRUE)
  )

## Position statistics for all players ----
position_stats_all_22_23 <- all1 |>
  group_by(position) |>
  summarize(
    Count = n(),
    Mean = mean(total_points, na.rm = TRUE),
    Median = median(total_points, na.rm = TRUE),
    SD = sd(total_points, na.rm = TRUE),
    Min = min(total_points, na.rm = TRUE),
    Max = max(total_points, na.rm = TRUE),
    Skewness = skewness(total_points, na.rm = TRUE),
    Kurtosis = kurtosis(total_points, na.rm = TRUE)
  )

position_stats_all_23_24 <- all2 |>
  group_by(position) |>
  summarize(
    Count = n(),
    Mean = mean(total_points, na.rm = TRUE),
    Median = median(total_points, na.rm = TRUE),
    SD = sd(total_points, na.rm = TRUE),
    Min = min(total_points, na.rm = TRUE),
    Max = max(total_points, na.rm = TRUE),
    Skewness = skewness(total_points, na.rm = TRUE),
    Kurtosis = kurtosis(total_points, na.rm = TRUE)
  )

position_stats_all_24_25 <- all3 |>
  group_by(position) |>
  summarize(
    Count = n(),
    Mean = mean(total_points, na.rm = TRUE),
    Median = median(total_points, na.rm = TRUE),
    SD = sd(total_points, na.rm = TRUE),
    Min = min(total_points, na.rm = TRUE),
    Max = max(total_points, na.rm = TRUE),
    Skewness = skewness(total_points, na.rm = TRUE),
    Kurtosis = kurtosis(total_points, na.rm = TRUE)
  )

# Save position statistics in TeX format to text files
print(xtable(position_stats_starters_22_23, 
  caption = "Position Statistics for Starters 22/23"),
    file = file.path(plot_directory, "Position_Statistics_Starters_22_23.txt"),
    include.rownames = FALSE,
    floating = TRUE,
    latex.environments = "center",
    type = "latex")

print(xtable(position_stats_starters_23_24, 
  caption = "Position Statistics for Starters 23/24"),
    file = file.path(plot_directory, "Position_Statistics_Starters_23_24.txt"),
    include.rownames = FALSE,
    floating = TRUE,
    latex.environments = "center",
    type = "latex")

print(xtable(position_stats_starters_24_25, 
  caption = "Position Statistics for Starters 24/25"),
    file = file.path(plot_directory, "Position_Statistics_Starters_24_25.txt"),
    include.rownames = FALSE,
    floating = TRUE,
    latex.environments = "center",
    type = "latex")

print(xtable(position_stats_1min_22_23, 
  caption = "Position Statistics for Players with 1+ Min 22/23"),
    file = file.path(plot_directory, "Position_Statistics_1min_22_23.txt"),
    include.rownames = FALSE,
    floating = TRUE,
    latex.environments = "center",
    type = "latex")

print(xtable(position_stats_1min_23_24, 
  caption = "Position Statistics for Players with 1+ Min 23/24"),
    file = file.path(plot_directory, "Position_Statistics_1min_23_24.txt"),
    include.rownames = FALSE,
    floating = TRUE,
    latex.environments = "center",
    type = "latex")

print(xtable(position_stats_1min_24_25, 
  caption = "Position Statistics for Players with 1+ Min 24/25"),
    file = file.path(plot_directory, "Position_Statistics_1min_24_25.txt"),
    include.rownames = FALSE,
    floating = TRUE,
    latex.environments = "center",
    type = "latex")

print(xtable(position_stats_all_22_23, 
  caption = "Position Statistics for All Players 22/23"),
    file = file.path(plot_directory, "Position_Statistics_All_22_23.txt"),
    include.rownames = FALSE,
    floating = TRUE,
    latex.environments = "center",
    type = "latex")

print(xtable(position_stats_all_23_24, 
  caption = "Position Statistics for All Players 23/24"),
    file = file.path(plot_directory, "Position_Statistics_All_23_24.txt"),
    include.rownames = FALSE,
    floating = TRUE,
    latex.environments = "center",
    type = "latex")

print(xtable(position_stats_all_24_25, 
  caption = "Position Statistics for All Players 24/25"),
    file = file.path(plot_directory, "Position_Statistics_All_24_25.txt"),
    include.rownames = FALSE,
    floating = TRUE,
    latex.environments = "center",
    type = "latex")

# Print to console as well
print(position_stats_starters_22_23)
print(position_stats_starters_23_24)
print(position_stats_starters_24_25)
print(position_stats_1min_22_23)
print(position_stats_1min_23_24)
print(position_stats_1min_24_25)
print(position_stats_all_22_23)
print(position_stats_all_23_24)
print(position_stats_all_24_25)

# After thought
df1 <- season_22_23
df2 <- season_23_24
df3 <- season_24_25
# Count unique names
df1 |> distinct(name) |> nrow()
df2 |> distinct(name) |> nrow()
df3 |> distinct(name) |> nrow()

# Count unique names starts
df1 |> filter(starts == 1) |>distinct(name) |> nrow()
df2 |> filter(starts == 1) |>distinct(name) |> nrow()
df3 |> filter(starts == 1) |>distinct(name) |> nrow()

# Same for \geq 1
df1 |> filter(minutes >= 1) |>distinct(name) |> nrow()
df2 |> filter(minutes >= 1) |>distinct(name) |> nrow()
df3 |> filter(minutes >= 1) |>distinct(name) |> nrow()
