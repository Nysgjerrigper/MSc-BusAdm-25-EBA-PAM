# EDA ----
# Analysis of point distributions for players based on different criteria
rm(list = ls(all = TRUE))
# Load required packages for statistical description
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pastecs,tidyverse)

# Create directory for saving plots
plot_directory <- "C:/Users/peram/Plots from R" 
dir.create(plot_directory, recursive = TRUE)

# Define position order for consistent plotting
position_levels <- c("GK", "DEF", "MID", "FWD")

# Question 1: Point distribution for starting players
# ---------------------------------------------------

# Split data by season (assuming alternativsammensatt contains multiple seasons)
# Filter for season boundaries based on GW
season_22_23 <- read_csv("Datasett/Sesong 22 til 23.csv")
season_23_24 <- read_csv("Datasett/Sesong 23 til 24.csv")
season_24_25 <- read_csv("Datasett/Sesong 24 til 25.csv")

# Create datasets of starting players

starters_22_23 <- season_22_23 |> 
  filter(starts == 1) |> 
  select(total_points, position) |>
  mutate(position = factor(position, levels = position_levels))

starters_23_24 <- season_23_24 |> 
  filter(starts == 1) |> 
  select(total_points, position) |>
  mutate(position = factor(position, levels = position_levels))

starters_24_25 <- season_23_24 |> 
  filter(starts == 1) |> 
  select(total_points, position) |>
  mutate(position = factor(position, levels = position_levels))

# Display dataset structure
glimpse(starters_22_23)
cat("Position levels consistent between seasons:", 
    identical(levels(starters_22_23$position), levels(starters_23_24$position), levels(starters_24_25$position)), "\n")

# Create boxplots for point distributions by position for starters
plot1 <- ggplot(starters_22_23, aes(x = position, y = total_points)) + 
  geom_boxplot() +
  labs(title = "Point distribution for starting players in season 22/23", 
       x = "Position", 
       y = "Observed points per week") +
  scale_y_continuous(breaks = seq(0, max(starters_22_23$total_points, na.rm = TRUE), 2))

plot2 <- ggplot(starters_23_24, aes(x = position, y = total_points)) +
  geom_boxplot() +
  labs(title = "Point distribution for starting players in season 23/24", 
       x = "Position", 
       y = "Observed points per week") +
  scale_y_continuous(breaks = seq(0, max(starters_23_24$total_points, na.rm = TRUE), 2)) 

plot3 <- ggplot(starters_24_25, aes(x = position, y = total_points)) +
  geom_boxplot() +
  labs(title = "Point distribution for starting players in season 23/24", 
       x = "Position", 
       y = "Observed points per week") +
  scale_y_continuous(breaks = seq(0, max(starters_24_25$total_points, na.rm = TRUE), 2)) 

# Display plots
plot1;plot2;plot3

# Save plots
ggsave(file.path(plot_directory, "Point distribution for starting players in season 22-23.png"), 
       plot = plot1, width = 8, height = 6)
ggsave(file.path(plot_directory, "Point distribution for starting players in season 23-24.png"), 
       plot = plot2, width = 8, height = 6)
ggsave(file.path(plot_directory, "Point distribution for starting players in season 24-25.png"), 
       plot = plot3, width = 8, height = 6)

# Distribution density plots for starting players ----
# Create distribution plots with different colors by position
dist_plot1 <- ggplot(starters_22_23, aes(x = total_points, fill = position, color = position)) + 
  geom_density(alpha = 0.2) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Point distribution for starting players in season 22/23", 
       x = "Total points", 
       y = "Density") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, max(starters_22_23$total_points, na.rm = TRUE), 2))

dist_plot2 <- ggplot(starters_23_24, aes(x = total_points, fill = position, color = position)) + 
  geom_density(alpha = 0.2) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Point distribution for starting players in season 23/24", 
       x = "Total points", 
       y = "Density") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, max(starters_23_24$total_points, na.rm = TRUE), 2))

dist_plot3 <- ggplot(starters_23_24, aes(x = total_points, fill = position, color = position)) + 
  geom_density(alpha = 0.2) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Point distribution for starting players in season 23/24", 
       x = "Total points", 
       y = "Density") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, max(starters_23_24$total_points, na.rm = TRUE), 2))

# Display density plots for starters
dist_plot1;dist_plot2;dist_plot3

# Save density plots for starters
ggsave(file.path(plot_directory, "Point density distribution for starting players 22-23.png"), 
       plot = dist_plot1, width = 10, height = 6)
ggsave(file.path(plot_directory, "Point density distribution for starting players 23-24.png"), 
       plot = dist_plot2, width = 10, height = 6)
ggsave(file.path(plot_directory, "Point density distribution for starting players 23-24.png"), 
       plot = dist_plot3, width = 10, height = 6)

# Question 2: Point distribution for players with minimum playing time
# -------------------------------------------------------------------

# Create datasets of players with at least 15 minutes
players_played_1 <- season_22_23 |> 
  filter(minutes >= 1) |> 
  select(total_points, position) |>
  mutate(position = factor(position, levels = position_levels))

players_played_2 <- season_23_24 |> 
  filter(minutes >= 1) |> 
  select(total_points, position) |>
  mutate(position = factor(position, levels = position_levels))

players_played_3 <- season_23_25 |> 
  filter(minutes >= 1) |> 
  select(total_points, position) |>
  mutate(position = factor(position, levels = position_levels))

# Create boxplots for players with atleast 1 min playtime
plot4 <- ggplot(players_played_1, aes(x = position, y = total_points)) + 
  geom_boxplot() +
  labs(title = "Point distribution for players with ≥15 minutes (22/23)", 
       x = "Position", 
       y = "Observed points per week") +
  scale_y_continuous(breaks = seq(0, max(players_played_1$total_points, na.rm = TRUE), 2))

plot5 <- ggplot(players_played_2, aes(x = position, y = total_points)) +
  geom_boxplot() +
  labs(title = "Point distribution for players with ≥15 minutes (23/24)", 
       x = "Position", 
       y = "Observed points per week") +
  scale_y_continuous(breaks = seq(0, max(players_played_2$total_points, na.rm = TRUE), 2))

plot6 <- ggplot(players_played_3, aes(x = position, y = total_points)) +
  geom_boxplot() +
  labs(title = "Point distribution for players with ≥15 minutes (23/24)", 
       x = "Position", 
       y = "Observed points per week") +
  scale_y_continuous(breaks = seq(0, max(players_played_3$total_points, na.rm = TRUE), 2))

# Display plots
plot4;plot5;plot6

# Save plots
ggsave(file.path(plot_directory, "Point distribution for players with 15min+ playing time 22-23.png"), 
       plot = plot3, width = 8, height = 6)
ggsave(file.path(plot_directory, "Point distribution for players with 15min+ playing time 23-24.png"), 
       plot = plot4, width = 8, height = 6)
ggsave(file.path(plot_directory, "Point distribution for players with 15min+ playing time 24-25.png"), 
       plot = plot4, width = 8, height = 6)

# Distribution plots for players with atleast 1 minute
dist_plot4 <- ggplot(players_played_1, aes(x = total_points, fill = position, color = position)) + 
  geom_density(alpha = 0.2) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Point distribution for players with ≥1 minute (22/23)", 
       x = "Total points", 
       y = "Density") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, max(players_played_1$total_points, na.rm = TRUE), 2))

dist_plot5 <- ggplot(players_played_2, aes(x = total_points, fill = position, color = position)) + 
  geom_density(alpha = 0.2) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Point distribution for players with ≥1 minute (23/24)", 
       x = "Total points", 
       y = "Density") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, max(players_played_2$total_points, na.rm = TRUE), 2))

dist_plot6 <- ggplot(players_played_2, aes(x = total_points, fill = position, color = position)) + 
  geom_density(alpha = 0.2) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Point distribution for players with ≥1 minute (24/25)", 
       x = "Total points", 
       y = "Density") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, max(players_played_2$total_points, na.rm = TRUE), 2))

# Display density plots for players with atleast 1 min of play time
dist_plot4
dist_plot5
dist_plot6

# Save density plots for 1+ minute players
ggsave(file.path(plot_directory, "Point density distribution for players with 1min+ 22-23.png"), 
       plot = dist_plot4, width = 10, height = 6)
ggsave(file.path(plot_directory, "Point density distribution for players with 1min+ 23-24.png"), 
       plot = dist_plot5, width = 10, height = 6)
ggsave(file.path(plot_directory, "Point density distribution for players with 1min+ 24-25.png"), 
       plot = dist_plot6, width = 10, height = 6)

# Alternative overlaid distributions (combined in single plots)
# This shows the direct comparison between seasons for each position

# For starters: Compare 22/23 vs 23/24 by position
combined_starters <- bind_rows(
  mutate(starters_22_23, season = "22/23"),
  mutate(starters_23_24, season = "23/24")
)

pos_dist_starters <- ggplot(combined_starters, aes(x = total_points, color = season)) + 
  geom_density(linewidth = 1) +
  facet_wrap(~ position, scales = "free_y") +
  scale_color_manual(values = c("22/23" = "#1b9e77", "23/24" = "#d95f02")) +
  labs(title = "Comparing point distributions across seasons for starting players",
       x = "Total points",
       y = "Density") +
  theme_minimal()

pos_dist_starters

# For 15+ minute players: Compare 22/23 vs 23/24 by position
combined_15min <- bind_rows(
  mutate(players_played_1, season = "22/23"),
  mutate(players_played_2, season = "23/24")
)

pos_dist_15min <- ggplot(combined_15min, aes(x = total_points, color = season)) + 
  geom_density(linewidth = 1) +
  facet_wrap(~ position, scales = "free_y") +
  scale_color_manual(values = c("22/23" = "#1b9e77", "23/24" = "#d95f02")) +
  labs(title = "Comparing point distributions across seasons for players with ≥15 minutes",
       x = "Total points",
       y = "Density") +
  theme_minimal()

pos_dist_15min

# Save the combined comparison plots
ggsave(file.path(plot_directory, "Season comparison starters by position.png"), 
       plot = pos_dist_starters, width = 10, height = 8)
ggsave(file.path(plot_directory, "Season comparison 15min+ by position.png"), 
       plot = pos_dist_15min, width = 10, height = 8)

# Statistical summaries
# --------------------

# Create statistical summary tables
stats_starters_22_23 <- stat.desc(starters_22_23$total_points)
stats_starters_23_24 <- stat.desc(starters_23_24$total_points)
stats_15min_22_23 <- stat.desc(players_played_1$total_points)
stats_15min_23_24 <- stat.desc(players_played_2$total_points)

# Combine statistics for comparison
stats_comparison <- data.frame(
  Metric = rownames(as.data.frame(stats_starters_22_23)),
  Starters_22_23 = stats_starters_22_23,
  Starters_23_24 = stats_starters_23_24,
  Min15_22_23 = stats_15min_22_23,
  Min15_23_24 = stats_15min_23_24
)

print(stats_comparison)

# QQ Plots to check normality
# --------------------------

# QQ Plot for starters 22/23
png(file.path(plot_directory, "QQ Plot of Starters 22-23 Points.png"), width = 800, height = 600)
qqnorm(starters_22_23$total_points, col = "blue", pch = 20,
       main = "Q-Q Plot of Starters 22/23 Points",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles")
qqline(starters_22_23$total_points, col = "red", lwd = 2)
dev.off()

# QQ Plot for starters 23/24
png(file.path(plot_directory, "QQ Plot of Starters 23-24 Points.png"), width = 800, height = 600)
qqnorm(starters_23_24$total_points, col = "blue", pch = 20,
       main = "Q-Q Plot of Starters 23/24 Points",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles")
qqline(starters_23_24$total_points, col = "red", lwd = 2)
dev.off()

# QQ Plot for 15+ minutes players 22/23
png(file.path(plot_directory, "QQ Plot of Players 15min+ 22-23 Points.png"), width = 800, height = 600)
qqnorm(players_played_1$total_points, col = "blue", pch = 20,
       main = "Q-Q Plot of Players with ≥15 Minutes 22/23 Points",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles")
qqline(players_played_1$total_points, col = "red", lwd = 2)
dev.off()

# QQ Plot for 15+ minutes players 23/24
png(file.path(plot_directory, "QQ Plot of Players 15min+ 23-24 Points.png"), width = 800, height = 600)
qqnorm(players_played_2$total_points, col = "blue", pch = 20,
       main = "Q-Q Plot of Players with ≥15 Minutes 23/24 Points",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles")
qqline(players_played_2$total_points, col = "red", lwd = 2)
dev.off()

# Additional analysis - Points by position
# ---------------------------------------
# Create summary statistics by position for each dataset
position_stats_starters_22_23 <- starters_22_23 |>
  group_by(position) |>
  summarize(
    Count = n(),
    Mean = mean(total_points, na.rm = TRUE),
    Median = median(total_points, na.rm = TRUE),
    SD = sd(total_points, na.rm = TRUE),
    Min = min(total_points, na.rm = TRUE),
    Max = max(total_points, na.rm = TRUE)
  )

position_stats_starters_23_24 <- starters_23_24 |>
  group_by(position) |>
  summarize(
    Count = n(),
    Mean = mean(total_points, na.rm = TRUE),
    Median = median(total_points, na.rm = TRUE),
    SD = sd(total_points, na.rm = TRUE),
    Min = min(total_points, na.rm = TRUE),
    Max = max(total_points, na.rm = TRUE)
  )

# Print position-based statistics
cat("Position statistics for starters 22/23:\n")
print(position_stats_starters_22_23)
cat("\nPosition statistics for starters 23/24:\n")
print(position_stats_starters_23_24)

# Create datasets of all players (no minutes filter)
all_players_22_23 <- season_22_23 |> 
  select(total_points, position) |>
  mutate(position = factor(position, levels = position_levels))

all_players_23_24 <- season_23_24 |> 
  select(total_points, position) |>
  mutate(position = factor(position, levels = position_levels))

# Create boxplots for all players
plot3 <- ggplot(all_players_22_23, aes(x = position, y = total_points)) + 
  geom_boxplot() +
  labs(title = "Point distribution for all players (22/23)", 
       x = "Position", 
       y = "Observed points per week") +
  scale_y_continuous(breaks = seq(0, max(all_players_22_23$total_points, na.rm = TRUE), 2))

plot4 <- ggplot(all_players_23_24, aes(x = position, y = total_points)) +
  geom_boxplot() +
  labs(title = "Point distribution for all players (23/24)", 
       x = "Position", 
       y = "Observed points per week") +
  scale_y_continuous(breaks = seq(0, max(all_players_23_24$total_points, na.rm = TRUE), 2))

# Display plots
plot3
plot4

# Save plots for all players
ggsave(file.path(plot_directory, "Point distribution for all players 22-23.png"), 
       plot = plot3, width = 8, height = 6)
ggsave(file.path(plot_directory, "Point distribution for all players 23-24.png"), 
       plot = plot4, width = 8, height = 6)

# Statistical summaries
# --------------------

# Create statistical summary tables
stats_starters_22_23 <- stat.desc(starters_22_23$total_points)
stats_starters_23_24 <- stat.desc(starters_23_24$total_points)
stats_all_22_23 <- stat.desc(all_players_22_23$total_points)
stats_all_23_24 <- stat.desc(all_players_23_24$total_points)

# Combine statistics for comparison
stats_comparison <- data.frame(
  Metric = rownames(as.data.frame(stats_starters_22_23)),
  Starters_22_23 = stats_starters_22_23,
  Starters_23_24 = stats_starters_23_24,
  All_22_23 = stats_all_22_23,
  All_23_24 = stats_all_23_24
)

print(stats_comparison)

# Distribution plots for all players
dist_plot3 <- ggplot(all_players_22_23, aes(x = total_points, fill = position, color = position)) + 
  geom_density(alpha = 0.2) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Point distribution for all players (22/23)", 
       x = "Total points", 
       y = "Density") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, max(all_players_22_23$total_points, na.rm = TRUE), 2))

dist_plot4 <- ggplot(all_players_23_24, aes(x = total_points, fill = position, color = position)) + 
  geom_density(alpha = 0.2) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Point distribution for all players (23/24)", 
       x = "Total points", 
       y = "Density") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, max(all_players_23_24$total_points, na.rm = TRUE), 2))

# Display density plots for all players
dist_plot3
dist_plot4

# Save density plots for all players
ggsave(file.path(plot_directory, "Point density distribution for all players 22-23.png"), 
       plot = dist_plot3, width = 10, height = 6)
ggsave(file.path(plot_directory, "Point density distribution for all players 23-24.png"), 
       plot = dist_plot4, width = 10, height = 6)

# For comparison: Starters vs All Players by position
combined_all <- bind_rows(
  mutate(all_players_22_23, season = "22/23"),
  mutate(all_players_23_24, season = "23/24")
)

pos_dist_all <- ggplot(combined_all, aes(x = total_points, color = season)) + 
  geom_density(linewidth = 1) +
  facet_wrap(~ position, scales = "free_y") +
  scale_color_manual(values = c("22/23" = "#1b9e77", "23/24" = "#d95f02")) +
  labs(title = "Comparing point distributions across seasons for all players",
       x = "Total points",
       y = "Density") +
  theme_minimal()

pos_dist_all

# Save the combined comparison plots
ggsave(file.path(plot_directory, "Season comparison all players by position.png"), 
       plot = pos_dist_all, width = 10, height = 8)

# QQ Plots to check normality
# --------------------------

# QQ Plot for all players 22/23
png(file.path(plot_directory, "QQ Plot of All Players 22-23 Points.png"), width = 800, height = 600)
qqnorm(all_players_22_23$total_points, col = "blue", pch = 20,
       main = "Q-Q Plot of All Players 22/23 Points",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles")
qqline(all_players_22_23$total_points, col = "red", lwd = 2)
dev.off()

# QQ Plot for all players 23/24 
png(file.path(plot_directory, "QQ Plot of All Players 23-24 Points.png"), width = 800, height = 600)
qqnorm(all_players_23_24$total_points, col = "blue", pch = 20,
       main = "Q-Q Plot of All Players 23/24 Points",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles")
qqline(all_players_23_24$total_points, col = "red", lwd = 2)
dev.off()

# Additional analysis - Points by position
# ---------------------------------------
# Create summary statistics by position for each dataset
position_stats_all_22_23 <- all_players_22_23 |>
  group_by(position) |>
  summarize(
    Count = n(),
    Mean = mean(total_points, na.rm = TRUE),
    Median = median(total_points, na.rm = TRUE),
    SD = sd(total_points, na.rm = TRUE),
    Min = min(total_points, na.rm = TRUE),
    Max = max(total_points, na.rm = TRUE)
  )

position_stats_all_23_24 <- all_players_23_24 |>
  group_by(position) |>
  summarize(
    Count = n(),
    Mean = mean(total_points, na.rm = TRUE),
    Median = median(total_points, na.rm = TRUE),
    SD = sd(total_points, na.rm = TRUE),
    Min = min(total_points, na.rm = TRUE),
    Max = max(total_points, na.rm = TRUE)
  )

# Print position-based statistics
cat("Position statistics for all players 22/23:\n")
print(position_stats_all_22_23)
cat("\nPosition statistics for all players 23/24:\n")
print(position_stats_all_23_24)