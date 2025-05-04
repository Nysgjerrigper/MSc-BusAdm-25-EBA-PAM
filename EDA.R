# EDA ----
# Analysis of point distributions for players based on different criteria
rm(list = ls(all = TRUE))
# Load required packages for statistical description
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pastecs,tidyverse,ggridges)

# Create directory for saving plots
setwd("C:/Users/peram/Documents/test")
plot_directory <- "C:/Users/peram/Plots from R" 
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
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, max(starters_22_23$total_points, na.rm = TRUE), 2))

dist_plot2 <- ggplot(starters_23_24, aes(x = total_points, fill = position, color = position)) + 
  geom_density(alpha = 0.2) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Point distribution for starting players in season 23/24 by position", 
       x = "Total points", 
       y = "Density") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, max(starters_23_24$total_points, na.rm = TRUE), 2))

dist_plot3 <- ggplot(starters_24_25, aes(x = total_points, fill = position, color = position)) + 
  geom_density(alpha = 0.2) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Point distribution for starting players in season 24/25 by position", 
       x = "Total points", 
       y = "Density") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, max(starters_24_25$total_points, na.rm = TRUE), 2))

## Display density plots for starters
dist_plot1;dist_plot2;dist_plot3


# --- Optional: Save the plot ---
# Uncomment the line below to save the plot to your plot_directory
# ggsave(file.path(plot_directory, "Gameweek_Ridgeline_Plot_by_Position.png"),
#        plot = gameweek_pos_ridgeline, width = 12, height = 10)



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

dist_plot6 <- ggplot(players_played_3, aes(x = total_points, fill = position, color = position)) + 
  geom_density(alpha = 0.2) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Point distribution for players with ≥1 minute (24/25)", 
       x = "Total points", 
       y = "Density") +
  theme_minimal() +
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
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, max(players_played_1$total_points, na.rm = TRUE), 2))

dist_plot8 <- ggplot(all2, aes(x = total_points, fill = position, color = position)) + 
  geom_density(alpha = 0.2) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Point distribution for all players by position (23/24)", 
       x = "Total points", 
       y = "Density") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, max(all2$total_points, na.rm = TRUE), 2))

dist_plot9 <- ggplot(all3, aes(x = total_points, fill = position, color = position)) + 
  geom_density(alpha = 0.2) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Point distribution for all players by position (24/25)", 
       x = "Total points", 
       y = "Density") +
  theme_minimal() +
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
ggsave(file.path(plot_directory, "Point density distribution for starting players 23-24.png"), 
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
  theme_minimal()

### Create Ridgeline Plot by Gameweek and Position ----

# Generate the ridgeline plot
gameweek_pos_ridgeline <- ggplot(combined_starters,
                                 aes(x = total_points,
                                     y = factor(GW), # Treat gameweek as a categorical variable on y-axis
                                     fill = factor(season))) + # Fill by position
  geom_density_ridges(alpha = 0.4) + # Use geom_density_ridges for overlapping densities
  scale_fill_manual(values = c("22/23" = "navy", # Define colors for each position
                               "23/24" = "darkgreen",
                               "24/25" = "cyan")) +
  labs(title = "Distribution of Gameweek Points by Gameweek and Position",
       x = "Gameweek Points",
       y = "Gameweek") +
  theme_ridges() + # Apply a ridgeline theme
  theme(legend.position = "bottom") # Position the legend at the bottom

# Display the plot
gameweek_pos_ridgeline

box_compare_starter
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
  theme_minimal()

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
  theme_minimal()

box_compare_all

# Save the combined comparison plots
ggsave(file.path(plot_directory, "Season comparison starters by position.png"), 
       plot = box_compare_starter, width = 10, height = 8)
ggsave(file.path(plot_directory, "Season comparison at least 1 min by position.png"), 
       plot = box_compare_1_min, width = 10, height = 8)
ggsave(file.path(plot_directory, "Season comparison all players by position.png"),
       plot = box_compare_all, width = 10, height = 8)


# Descriptives table ----

## stat.desc(total_points) for starters ----
statdesc_starter_1 <- stat.desc(starters_22_23$total_points, norm = FALSE)
statdesc_starter_2 <- stat.desc(starters_23_24$total_points)
statdesc_starter_3 <- stat.desc(starters_24_25$total_points)

## stat.desc(total_points) for starters ----
statdesc_1min_1<- stat.desc(players_played_1$total_points)
statdesc_1min_2 <- stat.desc(players_played_2$total_points)
statdesc_1min_3 <- stat.desc(players_played_3$total_points)

## stat.desc(total_points) for all players ----
statdesc_all_1<- stat.desc(all1$total_points)
statdesc_all_2 <- stat.desc(all2$total_points)
statdesc_all_3 <- stat.desc(all3$total_points)

# Combine statistics for comparison
stats_comparison <- data.frame(
  Metric = rownames(as.data.frame(statdesc_starter_1)),
  "Starting players (22/23)" = statdesc_starter_1,
  "Starting players (23/24)" = statdesc_starter_2,
  "Starting players (24/25)" = statdesc_starter_3,
  "Minimum 1 minute players (22/23)" = statdesc_1min_1,
  "Minimum 1 minute players (23/24)" = statdesc_1min_2,
  "Minimum 1 minute players (24/25)" = statdesc_1min_3,
  "All players (22/23)" = statdesc_all_1,
  "All players (23/24)" = statdesc_all_2,
  "All players (24/25)" = statdesc_all_3
)

print(stats_comparison)

xtable::print.xtable(stats_comparison, 
             file = file.path(plot_directory, "Descriptive statistics comparison.txt"),
             include.rownames = FALSE,
             floating = FALSE,
             tabular.environment = "longtable",
             caption = "Descriptive statistics for different player groups across seasons")

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
# QQ Plots to check normality
# --------------------------

# QQ Plots for starters
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

# QQ Plot for starters 24/25
png(file.path(plot_directory, "QQ Plot of Starters 24-25 Points.png"), width = 800, height = 600)
qqnorm(starters_24_25$total_points, col = "blue", pch = 20,
       main = "Q-Q Plot of Starters 24/25 Points",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles")
qqline(starters_24_25$total_points, col = "red", lwd = 2)
dev.off()

# QQ Plots for players with at least 1 minute
# QQ Plot for players with 1+ min 22/23
png(file.path(plot_directory, "QQ Plot of Players with 1+ Min 22-23 Points.png"), width = 800, height = 600)
qqnorm(players_played_1$total_points, col = "blue", pch = 20,
       main = "Q-Q Plot of Players with 1+ Min 22/23 Points",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles")
qqline(players_played_1$total_points, col = "red", lwd = 2)
dev.off()

# QQ Plot for players with 1+ min 23/24
png(file.path(plot_directory, "QQ Plot of Players with 1+ Min 23-24 Points.png"), width = 800, height = 600)
qqnorm(players_played_2$total_points, col = "blue", pch = 20,
       main = "Q-Q Plot of Players with 1+ Min 23/24 Points",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles")
qqline(players_played_2$total_points, col = "red", lwd = 2)
dev.off()

# QQ Plot for players with 1+ min 24/25
png(file.path(plot_directory, "QQ Plot of Players with 1+ Min 24-25 Points.png"), width = 800, height = 600)
qqnorm(players_played_3$total_points, col = "blue", pch = 20,
       main = "Q-Q Plot of Players with 1+ Min 24/25 Points",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles")
qqline(players_played_3$total_points, col = "red", lwd = 2)
dev.off()

# QQ Plots for all players
# QQ Plot for all players 22/23
png(file.path(plot_directory, "QQ Plot of All Players 22-23 Points.png"), width = 800, height = 600)
qqnorm(all1$total_points, col = "blue", pch = 20,
       main = "Q-Q Plot of All Players 22/23 Points",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles")
qqline(all1$total_points, col = "red", lwd = 2)
dev.off()

# QQ Plot for all players 23/24
png(file.path(plot_directory, "QQ Plot of All Players 23-24 Points.png"), width = 800, height = 600)
qqnorm(all2$total_points, col = "blue", pch = 20,
       main = "Q-Q Plot of All Players 23/24 Points",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles")
qqline(all2$total_points, col = "red", lwd = 2)
dev.off()

# QQ Plot for all players 24/25
png(file.path(plot_directory, "QQ Plot of All Players 24-25 Points.png"), width = 800, height = 600)
qqnorm(all3$total_points, col = "blue", pch = 20,
       main = "Q-Q Plot of All Players 24/25 Points",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles")
qqline(all3$total_points, col = "red", lwd = 2)
dev.off()

# Additional analysis - Points by position
# ---------------------------------------

# Position statistics for starters across all seasons
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

position_stats_starters_24_25 <- starters_24_25 |>
  group_by(position) |>
  summarize(
    Count = n(),
    Mean = mean(total_points, na.rm = TRUE),
    Median = median(total_points, na.rm = TRUE),
    SD = sd(total_points, na.rm = TRUE),
    Min = min(total_points, na.rm = TRUE),
    Max = max(total_points, na.rm = TRUE)
  )

# Print position-based statistics for starters
cat("Position statistics for starters 22/23:\n")
print(position_stats_starters_22_23)
cat("\nPosition statistics for starters 23/24:\n")
print(position_stats_starters_23_24)
cat("\nPosition statistics for starters 24/25:\n")
print(position_stats_starters_24_25)

# Position statistics for players with at least 1 minute
position_stats_1min_22_23 <- players_played_1 |>
  group_by(position) |>
  summarize(
    Count = n(),
    Mean = mean(total_points, na.rm = TRUE),
    Median = median(total_points, na.rm = TRUE),
    SD = sd(total_points, na.rm = TRUE),
    Min = min(total_points, na.rm = TRUE),
    Max = max(total_points, na.rm = TRUE)
  )

position_stats_1min_23_24 <- players_played_2 |>
  group_by(position) |>
  summarize(
    Count = n(),
    Mean = mean(total_points, na.rm = TRUE),
    Median = median(total_points, na.rm = TRUE),
    SD = sd(total_points, na.rm = TRUE),
    Min = min(total_points, na.rm = TRUE),
    Max = max(total_points, na.rm = TRUE)
  )

position_stats_1min_24_25 <- players_played_3 |>
  group_by(position) |>
  summarize(
    Count = n(),
    Mean = mean(total_points, na.rm = TRUE),
    Median = median(total_points, na.rm = TRUE),
    SD = sd(total_points, na.rm = TRUE),
    Min = min(total_points, na.rm = TRUE),
    Max = max(total_points, na.rm = TRUE)
  )

# Print position-based statistics for players with at least 1 minute
cat("\nPosition statistics for players with at least 1 minute 22/23:\n")
print(position_stats_1min_22_23)
cat("\nPosition statistics for players with at least 1 minute 23/24:\n")
print(position_stats_1min_23_24)
cat("\nPosition statistics for players with at least 1 minute 24/25:\n")
print(position_stats_1min_24_25)

# Position statistics for all players
position_stats_all_22_23 <- all1 |>
  group_by(position) |>
  summarize(
    Count = n(),
    Mean = mean(total_points, na.rm = TRUE),
    Median = median(total_points, na.rm = TRUE),
    SD = sd(total_points, na.rm = TRUE),
    Min = min(total_points, na.rm = TRUE),
    Max = max(total_points, na.rm = TRUE)
  )

position_stats_all_23_24 <- all2 |>
  group_by(position) |>
  summarize(
    Count = n(),
    Mean = mean(total_points, na.rm = TRUE),
    Median = median(total_points, na.rm = TRUE),
    SD = sd(total_points, na.rm = TRUE),
    Min = min(total_points, na.rm = TRUE),
    Max = max(total_points, na.rm = TRUE)
  )

position_stats_all_24_25 <- all3 |>
  group_by(position) |>
  summarize(
    Count = n(),
    Mean = mean(total_points, na.rm = TRUE),
    Median = median(total_points, na.rm = TRUE),
    SD = sd(total_points, na.rm = TRUE),
    Min = min(total_points, na.rm = TRUE),
    Max = max(total_points, na.rm = TRUE)
  )

# Print position-based statistics for all players
cat("\nPosition statistics for all players 22/23:\n")
print(position_stats_all_22_23)
cat("\nPosition statistics for all players 23/24:\n")
print(position_stats_all_23_24)
cat("\nPosition statistics for all players 24/25:\n")
print(position_stats_all_24_25)