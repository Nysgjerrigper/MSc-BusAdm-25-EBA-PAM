# Forecasts for FPL Script to Master thesis EBA&PAM
# To load save weights ----
# Kaggle Export: The .hdf5 weight files saved in /kaggle/working/ will appear in the "Output" section of your Kaggle notebook session after it completes. You can download them from there.
# Loading Later: Remember, when you want to use these weights later (in another notebook or locally), you MUST:
# Rebuild the exact same model architecture using keras_model().
# Compile the model using the exact same optimizer, loss, and metrics.
# Load the weights using load_model_weights_hdf5().
# Load the corresponding scaling factors (mu, sigma, numF) to prepare input data and unscale predictions.

rm(list = ls(all = TRUE))

## 0: Preamble ----
Sys.setenv(WORKON_HOME = "C:/Users/peram/Documents/test/R Forecast/.virtualenvs")
reticulate::use_virtualenv("C:/Users/peram/Documents/test/R Forecast/.virtualenvs/r-reticulate", required = TRUE)
# Adjust code to chosen place of
# 1. Virtual Python Environment
# 2. Forces R session to use the a specific virtualenv
# This way the virtual environment does not get installed
# in your onedrive\documents folder.


#install.packages("keras")
library(reticulate)
#virtualenv_create("r-reticulate", python = install_python())
library(keras)
#install_keras(envname = "r-reticulate")

# Rest of the packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggthemes, tidyverse, slider,
               slider,glmnet,httr,jsonlite,tensorflow,
               Metrics, pastecs, stats)

# Lists to store results ----
model_list       <- list()
scaling_factors  <- list()
predictions_list <- list()
metrics_list     <- list()

# Global user inputs ----
# Trainingsplit : in-sample <= split_gw < out-of-sample
split_gw <- 38+38
# LSTM
epoker <- 15
vindu <- 3

#Metrics for LSTM Models. Outside is depreceated needs to be implemented inside the models
# metrics_regression <- metric_root_mean_squared_error()
# metric_mean_absolute_error(),
# metric_mean_squared_error(),
# metric_mean_absolute_percentage_error(),
# metric_cosine_similarity()

# Til Prognosering, hvor mange uker frem ønsker du å spå fremover
antall_uker <- 7

# Til patience epoker i callback.
# Altså hvor mange epoker skal den vente før den slutter å trene modellen og gå tilbake til siste beste
num_patience <- 5

# Global embedding dimensions for all positions
emb_dim <- 16


# Fetch data and prelim data manipulation ----
df <- read_csv("C:/Users/peram/Documents/test/Datasett/Ekstra kolonner, stigende GW, alle tre sesonger(22-24), heltall.csv")
alternativsammensatt <- df

## Positions Partitions ----
gk <- df %>%
  filter(position == "GK") %>%
  # create a unique row ID and re-index factors
  mutate(
    row_id  = row_number(),
    pID_idx = as.integer(factor(player_id)),
    tID_idx = as.integer(factor(tID)),
    oID_idx = as.integer(factor(oID))
  )
def <- df %>%
  filter(position == "DEF") %>%
  # create a unique row ID and re-index factors
  mutate(
    row_id  = row_number(),
    pID_idx = as.integer(factor(player_id)),
    tID_idx = as.integer(factor(tID)),
    oID_idx = as.integer(factor(oID))
  )
mid <- df %>%
  filter(position == "MID") %>%
  # create a unique row ID and re-index factors
  mutate(
    row_id  = row_number(),
    pID_idx = as.integer(factor(player_id)),
    tID_idx = as.integer(factor(tID)),
    oID_idx = as.integer(factor(oID))
  )
fwd <- df %>%
  filter(position == "FWD") %>%
  # create a unique row ID and re-index factors
  mutate(
    row_id  = row_number(),
    pID_idx = as.integer(factor(player_id)),
    tID_idx = as.integer(factor(tID)),
    oID_idx = as.integer(factor(oID))
  )

posliste <- list(gk = gk, def = def, mid = mid, fwd = fwd)
lapply(posliste, head, n = 1)
lapply(posliste, tail, n = 1)

# Save for later
unscaled_gk <- gk
unscaled_def <- def
unscaled_mid <- mid
unscaled_fwd <- fwd

# EDA ----------------
# Where to save plots?
plot_directory <- "C:/Users/peram/Documents/test/R Forecast/EDA"
dir.create(plot_directory, recursive = TRUE)

season_22_23 <- df |> filter(GW <= 38)
season_23_24 <- df |> filter(GW > 38, GW < 38 + 38 + 1)
season_24_25 <- df |> filter(GW >= 38+38+1)

# Define position order for consistent plotting
position_levels <- c("GK", "DEF", "MID", "FWD")

## Starting Players ----

### Create dataset of starting players ----

starters_22_23 <- season_22_23 |>
  filter(starts == 1) |>
  select(total_points, position)|>
  mutate(position = factor(position, levels = position_levels))
starters_23_24 <- season_23_24 |> 
  filter(starts == 1) |> 
  select(total_points, position) |>
  mutate(position = factor(position, levels = position_levels))

starters_24_25 <- season_24_25 |> 
  filter(starts == 1) |> 
  select(total_points, position) |>
  mutate(position = factor(position, levels = position_levels)) 

### Display dataset structure ----
glimpse(starters_22_23)

### Create boxplots for point distributions by position for starters ----

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

### Print box plots ----
plot1;plot2;plot3

### Distribution density plots for starting players ----
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

### Print density plots for starters ----
dist_plot1;dist_plot2;dist_plot3

## Atleaset 1 min of play time ----

### Create datasets of players with at least 1 minute ----
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

### Create boxplots for players with atleast 1 min playtime ----
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

### Print boxplots for atleast 1 min 
plot4;plot5;plot6

### Create Distribution plots for players with atleast 1 minute ----
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

### print density plots for players with atleast 1 min of play time ----
dist_plot4;dist_plot5;dist_plot6

## All players ----
### Create datasets of all players ----
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

### Create boxplots for all players ----

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

### Print boxplots all players ----
plot7;plot8;plot9

### Create distribution density plots for all players ----

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

## Export plots ----

### Save plots for starting players ----

### Save boxplots ----
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

## Save plots for 1+ minutes players ----
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

## Comparison by position across seasons ----

### For starters ----
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

box_compare_starter
### For 1+ minute players ----
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

### For all players ----

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

### Save the combined comparison plots ----

ggsave(file.path(plot_directory, "Season comparison starters by position.png"), 
       plot = box_compare_starter, width = 10, height = 8)
ggsave(file.path(plot_directory, "Season comparison at least 1 min by position.png"), 
       plot = box_compare_1_min, width = 10, height = 8)
ggsave(file.path(plot_directory, "Season comparison all players by position.png"),
       plot = box_compare_all, width = 10, height = 8)

## Descriptives table ----

### stat.desc(total_points) for starters ----
statdesc_starter_1 <- stat.desc(starters_22_23$total_points, norm = FALSE)
statdesc_starter_2 <- stat.desc(starters_23_24$total_points)
statdesc_starter_3 <- stat.desc(starters_24_25$total_points)

### stat.desc(total_points) for starters ----
statdesc_1min_1<- stat.desc(players_played_1$total_points)
statdesc_1min_2 <- stat.desc(players_played_2$total_points)
statdesc_1min_3 <- stat.desc(players_played_3$total_points)

### stat.desc(total_points) for all players ----
statdesc_all_1<- stat.desc(all1$total_points)
statdesc_all_2 <- stat.desc(all2$total_points)
statdesc_all_3 <- stat.desc(all3$total_points)

### Combine statistics for comparison ----
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

## QQ Plots to check normality ----

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

## Additional analysis - Points by position ----

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

# 1: Goalkeepers (GK) ----

## GK 1.1 Define Features, Target & Scale ----
#numF = numerical features, tar = target
numF <- c(
  "assists","creativity","minutes","goals_conceded","saves","bonus","bps",
  "expected_assists","expected_goal_involvements","expected_goals","ict_index",
  "own_goals","red_cards","threat","transfers_in","transfers_out","yellow_cards",
  "expected_goals_conceded","penalties_saved","value","selected",
  "transfers_balance","starts","influence","clean_sheets"
)
tar <- "total_points"
initital_numF <- length(numF)
cat("Numeriske features:", numF, "\n")
cat("Target variabel:", tar, "\n")
cat("Ferdig\n")

gk <- gk %>%
  arrange(pID_idx, GW) %>%
  group_by(pID_idx) %>%
  ungroup()

# Compute and store the mean and standard deviation for the target variable

mu <- mean(gk[[tar]], na.rm = TRUE)
sigma <- sd(gk[[tar]], na.rm = TRUE)

# Scale numerical features and target
gk <- gk %>%
  mutate(across(all_of(numF), 
                ~ (.x - mean(.x, na.rm = TRUE)) / sd(.x, na.rm = TRUE)),
         !!tar := (.data[[tar]] - mu) / sigma)
cat("Chunk done")

## GK 1.2: Build rolling numerical windows ----

# Define window size (number of past gameweeks to use)
ws <- vindu

# Build numerical rolling windows (numW)
numW <- gk %>%
  group_by(pID_idx) %>%
  filter(n() > ws) %>%
  group_modify(~{
    W   <- slide(.x[, numF], .f=~as.matrix(.x),
                 .before=ws, .after=-1, .complete=TRUE)
    valid <- W[(ws+1):length(W)]
    tgt   <- .x[[tar]][(ws+1):nrow(.x)]
    rid   <- .x$row_id[(ws+1):nrow(.x)]
    tibble(window=valid, target=tgt, row_id=rid)
  }) %>% ungroup() %>%
  left_join(select(gk, row_id, GW), by="row_id")

cat("Chunk done\n")

## GK 1.3: Lasso Regression ----

# 1. Aggregate each rolling window by column means (for each sample)
Xl <- do.call(rbind, lapply(numW$window, colMeans, na.rm = TRUE))
sum(is.na(Xl))

# 2. Extract the target variable for each window
yl <- numW$target

# 3. Perform cross-validated Lasso regression to select lambda
cvfit <- cv.glmnet(Xl, yl, alpha = 1)

# 4. Get the lambda value that minimizes cross-validated error
minlambda <- cvfit$lambda.min

# 5. Fit Lasso regression with the selected lambda
lassofit <- glmnet(Xl, yl, alpha = 1, lambda = minlambda)

# 6. Extract the names of features with non-zero coefficients (selected by Lasso)
keepF <- rownames(as.matrix(coef(lassofit)))[coef(lassofit)[, 1] != 0] %>%
  setdiff("(Intercept)")

# 7. Update numF to only include selected features
numF <- keepF
cat("Lasso selected features:", numF,"Initial number of features:", initital_numF, "\n",
    "Number of features:",length(numF), "\n")

## GK 1.4.1: Rebuild Numeric Windows (numW) with Lasso selected variables ----
ws <- vindu
# Rebuild rolling numerical windows using only Lasso-selected features (numF)
# Why: After feature selection, 
# we want to ensure that only the most relevant variables are used for modeling, 
# which can improve model performance and reduce overfitting.
numW <- gk %>%
  group_by(pID_idx) %>%
  filter(n() > ws) %>%
  group_modify(~{
    W     <- slide(.x[, numF], .f=~as.matrix(.x),
                   .before=ws, .after=-1, .complete=TRUE)
    valid <- W[(ws+1):length(W)]
    tgt   <- .x[[tar]][(ws+1):nrow(.x)]
    rid   <- .x$row_id[(ws+1):nrow(.x)]
    tibble(window=valid, target=tgt, row_id=rid)
  }) %>% ungroup() %>%
  left_join(select(gk, row_id, GW), by="row_id")
head(numW, n = 2)
cat("Chunk done\n")

### GK 1.4.2: Build & align categorical windows (by player_id!) ----

# Build rolling windows for categorical variables (pID_idx, tID_idx, oID_idx, hID) for each player
catW_all <- gk %>%
  group_by(player_id) %>%
  filter(n() > ws) %>%
  group_modify(~{
    # For each player, create rolling windows of categorical indices
    W <- slide(
      .x %>% select(pID_idx, tID_idx, oID_idx, hID),
      .f = ~as.matrix(.x), .before = ws, .after = -1, .complete = TRUE
    )
    # Keep only valid windows (those with enough history)
    valid <- W[(ws + 1):length(W)]
    # Get corresponding row_ids for alignment
    rid <- .x$row_id[(ws + 1):nrow(.x)]
    tibble(window = valid, row_id = rid)
  }) %>% ungroup()

# Align categorical windows to numeric windows using row_id
catW <- numW %>%
  select(row_id) %>%
  left_join(catW_all, by = "row_id")

### GK 1.4.3: To Arrays
# Number of samples (number of rolling windows)
nSamp <- nrow(numW)

# Convert list of rolling numerical windows to a 3D array: [samples, window size, num features]
num_array <- array(unlist(numW$window), dim = c(nSamp, ws, length(numF)))

# Extract targets (scaled total_points) as a column matrix
targets <- matrix(numW$target, ncol = 1)

# Extract historical categorical indices for each window (for possible use in sequence models)
pID_hist_array <- t(sapply(catW$window, function(m) m[, "pID_idx"]))
tID_hist_array <- t(sapply(catW$window, function(m) m[, "tID_idx"]))
oID_hist_array <- t(sapply(catW$window, function(m) m[, "oID_idx"]))
hID_hist_array <- t(sapply(catW$window, function(m) m[, "hID"]))

# Extract current (last in window) categorical values for each sample
cat_cur_mat <- do.call(rbind, lapply(catW$window, function(m) m[nrow(m), ]))
pID_cur_array <- matrix(cat_cur_mat[, "pID_idx"], ncol = 1)
tID_cur_array <- matrix(cat_cur_mat[, "tID_idx"], ncol = 1)
oID_cur_array <- matrix(cat_cur_mat[, "oID_idx"], ncol = 1)
hID_cur_array <- matrix(cat_cur_mat[, "hID"], ncol = 1)


## GK 1.5: Split Data into Training and Validation Sets ----
idx <- which(numW$GW <= split_gw)
Xnum_train <- num_array[idx,,,drop=FALSE]
Xnum_val <- num_array[-idx,,,drop=FALSE]
y_train <- targets[idx,,drop=FALSE]
y_val <- targets[-idx,,drop=FALSE]

Xcat_hist_tr <- list(
  pID_hist = pID_hist_array[idx,,drop=FALSE],
  tID_hist = tID_hist_array[idx,,drop=FALSE],
  oID_hist = oID_hist_array[idx,,drop=FALSE],
  hID_hist = hID_hist_array[idx,,drop=FALSE]
)
Xcat_hist_va <- list(
  pID_hist = pID_hist_array[-idx,,drop=FALSE],
  tID_hist = tID_hist_array[-idx,,drop=FALSE],
  oID_hist = oID_hist_array[-idx,,drop=FALSE],
  hID_hist = hID_hist_array[-idx,,drop=FALSE]
)

Xcat_cur_tr <- list(
  pID_cur = pID_cur_array[idx,,drop=FALSE],
  tID_cur = tID_cur_array[idx,,drop=FALSE],
  oID_cur = oID_cur_array[idx,,drop=FALSE],
  hID_cur = hID_cur_array[idx,,drop=FALSE]
)
Xcat_cur_va <- list(
  pID_cur = pID_cur_array[-idx,,drop=FALSE],
  tID_cur = tID_cur_array[-idx,,drop=FALSE],
  oID_cur = oID_cur_array[-idx,,drop=FALSE],
  hID_cur = hID_cur_array[-idx,,drop=FALSE]
)

cat("Chunk Done\n")

## GK 1.6: Building the Keras Neural Network Model ----
### GK Dimensions of input layers ----
# nP == Number of players, nT == Number of teams, nO == Number of opponent teams
nP <- max(gk$pID_idx, na.rm=TRUE)
nT <- max(gk$tID_idx, na.rm=TRUE)
nO <- max(gk$oID_idx, na.rm=TRUE)

### GK Input Layers ----
inp_num <- layer_input(shape=c(ws,length(numF)), name="input_seq")
inp_ph  <- layer_input(shape=c(ws), dtype="int32", name="pID_hist")
inp_th  <- layer_input(shape=c(ws), dtype="int32", name="tID_hist")
inp_oh  <- layer_input(shape=c(ws), dtype="int32", name="oID_hist")
inp_hh  <- layer_input(shape=c(ws), dtype="int32", name="hID_hist")
inp_pc  <- layer_input(shape=c(1),   dtype="int32", name="pID_cur")
inp_tc  <- layer_input(shape=c(1),   dtype="int32", name="tID_cur")
inp_oc  <- layer_input(shape=c(1),   dtype="int32", name="oID_cur")
inp_hc  <- layer_input(shape=c(1),   dtype="int32", name="hID_cur")

### GK Embedding Layers ----
emb_ph <- inp_ph %>%
  layer_embedding(input_dim = nP + 1, output_dim = emb_dim, mask_zero = TRUE)
emb_th <- inp_th %>%
  layer_embedding(input_dim = nT + 1, output_dim = emb_dim, mask_zero = TRUE)
emb_oh <- inp_oh %>%
  layer_embedding(input_dim = nO + 1, output_dim = emb_dim, mask_zero = TRUE)
emb_hh <- inp_hh %>%
  layer_lambda(function(x){
    x_f <- tf$cast(x, tf$float32)
    tf$expand_dims(x_f, axis = as.integer(-1))
  })

### GK Concatenate Historical Embeddings for LSTM Sequence ----
cat_hist_seq <- layer_concatenate(list(emb_ph,emb_th,emb_oh,emb_hh), axis=-1)

### GK LSTM Layer for Sequential Modeling ----
lstm_in  <- layer_concatenate(list(inp_num, cat_hist_seq), axis=-1)
lstm_out <- lstm_in %>%
  layer_lstm(units=64, return_sequences=FALSE) %>%
  layer_dropout(rate=0.2)

### GK Embedding Layers for Current Categorical Inputs ----
emb_pc <- inp_pc %>%
  layer_embedding(input_dim = nP + 1, output_dim = emb_dim, mask_zero = TRUE) %>%
  layer_flatten()
emb_tc <- inp_tc %>%
  layer_embedding(input_dim = nT + 1, output_dim = emb_dim, mask_zero = TRUE) %>%
  layer_flatten()
emb_oc <- inp_oc %>%
  layer_embedding(input_dim = nO + 1, output_dim = emb_dim, mask_zero = TRUE) %>%
  layer_flatten()
emb_hc <- inp_hc %>%
  layer_lambda(function(x) tf$cast(x, tf$float32))

### GK Concatenate Current Categorical Embeddings ----
cat_cur_vec <- layer_concatenate(list(emb_pc,emb_tc,emb_oc,emb_hc), axis=-1)

### GK Merge LSTM Output and Current Categorical Features ----
merged <- layer_concatenate(list(lstm_out, cat_cur_vec), axis=-1)
head   <- merged %>% layer_dense(units=32, activation="relu")
out    <- head   %>% layer_dense(units=1,  activation="linear")

### GK Build and Compile Model ----
model <- keras_model(
  inputs  = list(inp_num, inp_ph, inp_th, inp_oh, inp_hh,
                 inp_pc, inp_tc, inp_oc, inp_hc),
  outputs = out
)

model %>% compile(
  optimizer = optimizer_adam(),
  loss      = "mse",
  metrics   = metric_root_mean_squared_error()
)

### GK Model Summary and Visualization ----
summary(model)
plot(model)

### GK Save Model Plot to File ----
keras_python <- import("keras")
keras_python$utils$plot_model(
  model,
  to_file = "keras-model-gk.png",
  show_shapes = TRUE,
  show_dtype = FALSE,
  show_layer_names = TRUE,
  rankdir = "TB",
  expand_nested = FALSE,
  dpi = 200,
  show_layer_activations = FALSE
)

### GK Display Model Plot in R ----
library(png)
img <- readPNG("keras-model-gk.png")
grid::grid.raster(img)

## GK 1.7: Callback Functions and Scaling Factors ----

# Filepath for saving the BEST weights (unique for each position)
weights_filepath_gk <- "R Forecast/best_gk.hdf5"
cat("Will save best GK weights to:", weights_filepath_gk, "\n")

# 1. Tidlig stopp funksjon, laster inn siste beste loss value.
# patience: how many epochs to wait after last improvement before stopping
# restore_best_weights: load the weights from the epoch with the best val_loss
early_stopping <- callback_early_stopping(
  monitor = "val_loss",
  patience = num_patience, 
  restore_best_weights = TRUE
)

# 2. Model Checkpoint Callback (to save best weights)
model_checkpoint <- callback_model_checkpoint(
  filepath = weights_filepath_gk, # Path defined above
  monitor = "val_loss",           # Monitor the same metric as early stopping
  save_best_only = TRUE,          # IMPORTANT: Only save when val_loss improves
  save_weights_only = TRUE,       # IMPORTANT: Save ONLY the weights, not the whole model
  mode = "min",                   # We want to minimize loss
  verbose = 1                     # Optional: print message when weights are saved
)

# 3. Reduce Learning Rate on Plateau Callback
reduce_lr <- callback_reduce_lr_on_plateau(
  monitor = "val_loss",  # Monitor validation loss (same as early stopping)
  factor = 0.2,          # Reduce LR to 20% of its current value (lr * factor)
  patience = num_patience,           # Reduce LR if val_loss doesn't improve for 5 epochs
  min_lr = 1e-6,         # Don't reduce the LR below this value
  verbose = 1            # Print a message when LR is reduced
)

scaling_factors$gk <- list(mu = mu,
                           sigma = sigma,
                           numF = numF) # numF here is the one selected by Lasso for GK
cat("GK scaling factors stored.\n")

## GK 1.8: Train Goalkeeper LSTM Model ----

cat("Starting GK model training...\n")

history <- model %>% fit(
  x = c(
    list(input_seq = Xnum_train),
    Xcat_hist_tr,
    Xcat_cur_tr
  ),
  y              = y_train,
  epochs         = epoker,
  batch_size     = 16,
  validation_data= list(
    c(
      list(input_seq = Xnum_val),
      Xcat_hist_va,
      Xcat_cur_va
    ),
    y_val
  ),
  callbacks = list(early_stopping, model_checkpoint, reduce_lr)
)
cat("GK model training finished.\n")


## GK 1.9: Generate Predictions & Invert Scaling on Validation Set ----
# Use the validation set for predictions
val_row_ids <- numW$row_id[-idx]

pred_all <- model %>% predict(
  c(
    list(input_seq = Xnum_val),
    Xcat_hist_va,
    Xcat_cur_va
  )
)

preds_gk <- tibble(
  row_id                         = val_row_ids,
  predicted_total_points_scaled  = as.vector(pred_all),
  actual_total_points_scaled     = as.vector(y_val)
) %>%
  mutate(
    predicted_total_points = predicted_total_points_scaled * sigma + mu,
    actual_total_points    = actual_total_points_scaled    * sigma + mu
  ) %>%
  left_join(unscaled_gk, by = "row_id")

glimpse(preds_gk)

# Build a predictions table using the validation row IDs
preds_gk <- tibble(
  row_id = val_row_ids,  # row IDs corresponding to the validation set
  predicted_total_points_scaled = as.vector(pred_all),
  actual_total_points_scaled = as.vector(y_val)
) %>% 
  mutate(
    predicted_total_points = predicted_total_points_scaled * sigma + mu,
    actual_total_points = actual_total_points_scaled * sigma + mu
  )


preds_gk <- preds_gk %>%
  left_join(unscaled_gk, by = "row_id") # Use unscaled_gk

# View the predictions table
glimpse(preds_gk)

## GK 1.10 Save model and validation set predictions ----
# Store GK model and scaling factors 
model_list$gk <- model
cat("GK model stored in model_list$gk.\n")

# Clean up validation predictions (select essential columns)
preds_gk_clean <- preds_gk %>%
  select(row_id, GW, player_id, name, position, team,
         actual_total_points, predicted_total_points, value)
cat("Validation predictions stored in preds_gk_clean.\n")

## GK 1.11: Plots for Evaluation ----

### GK 1.11.1 Save training history ----
history_gk <- history # class() "keras_training_history"
history_df_gk <- data.frame(history_gk)

### GK 1.11.2 Training History Plot ----
plot_history_gk <- plot(history_gk)
plot_history_gk;str(history_df_gk)

### GK 1.11.3 Actual vs Predicted Points Plot ----
plot_actual_predicted_gk <- ggplot(preds_gk, aes(x = actual_total_points, y = predicted_total_points)) +
  geom_point(alpha = 0.5) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Actual vs. Predicted Total Points (Validation Set)",
       x = "Actual Total Points",
       y = "Predicted Total Points") +
  facet_wrap(~ position) +
  theme_grey() +
  coord_cartesian(xlim = range(preds_gk$actual_total_points, na.rm = TRUE),
                  ylim = range(preds_gk$predicted_total_points, na.rm = TRUE))
plot_actual_predicted_gk                  

### GK 1.11.4: Residuals vs Predicted ----
preds_gk_residuals <- preds_gk %>%
  mutate(residual = actual_total_points - predicted_total_points)

plot_residuals_predicted_gk <- ggplot(preds_gk_residuals, aes(x = predicted_total_points, y = residual)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs. Predicted Total Points (Validation Set)",
       x = "Predicted Total Points",
       y = "Residual (Actual - Predicted)") +
  facet_wrap(~ position) +
  theme_grey()  
plot_residuals_predicted_gk

### GK 1.11.5: Distribution of Residuals ----
plot_distribution_residuals_gk <- ggplot(preds_gk_residuals, aes(x = residual)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "blue", alpha = 0.7) +
  geom_density(color = "red") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Distribution of Residuals (Validation Set)",
       x = "Residual (Actual - Predicted)",
       y = "Density") +
  facet_wrap(~ position) +
  theme_grey()
plot_distribution_residuals_gk

### GK 1.11.6 Return list containing loss value and metrics ----
# Evaluate model on validation data using consistent variable names
model_evaluation <- model |> evaluate(
  c(
    list(input_seq = Xnum_val),
    Xcat_hist_va,
    Xcat_cur_va
  ),
  y_val,
  verbose = 1
)
# Print the evaluation results
print(model_evaluation)

# Store validation metrics
validation_metrics <- list()
validation_metrics$gk <- model_evaluation
cat("Validation metrics stored in validation_metrics$gk.\n")

# 2: Defenders (DEF) ----

## DEF 2.1 Define Features, Target & Scale ----
#numF = numerical features, tar = target
numF <- c("assists", "creativity", "minutes", "goals_conceded",
          "bonus", "bps", "expected_assists", "expected_goal_involvements",
          "expected_goals", "ict_index", "own_goals", "red_cards",
          "threat", "transfers_in", "transfers_out", "yellow_cards",
          "expected_goals_conceded", "value",
          "selected", "transfers_balance", "starts", "influence",
          "clean_sheets"
)
tar <- "total_points"
initital_numF <- length(numF)

cat("Numeriske features:", numF, "\n")
cat("Target variabel:", tar, "\n")
cat("Ferdig\n")

def <- def %>%
  arrange(pID_idx, GW) %>%
  group_by(pID_idx) %>%
  ungroup()

# Compute and store the mean and standard deviation for the target variable

mu <- mean(def[[tar]], na.rm = TRUE)
sigma <- sd(def[[tar]], na.rm = TRUE)

# Scale numerical features and target
def <- def %>%
  mutate(across(all_of(numF), 
                ~ (.x - mean(.x, na.rm = TRUE)) / sd(.x, na.rm = TRUE)),
         !!tar := (.data[[tar]] - mu) / sigma)
cat("Chunk done")

## DEF 2.2: Build rolling numerical windows

# Define window size (number of past gameweeks to use)
ws <- vindu

# Build numerical rolling windows (numW)
numW <- def %>%
  group_by(pID_idx) %>%
  filter(n() > ws) %>%
  group_modify(~{
    W   <- slide(.x[, numF], .f=~as.matrix(.x),
                 .before=ws, .after=-1, .complete=TRUE)
    valid <- W[(ws+1):length(W)]
    tgt   <- .x[[tar]][(ws+1):nrow(.x)]
    rid   <- .x$row_id[(ws+1):nrow(.x)]
    tibble(window=valid, target=tgt, row_id=rid)
  }) %>% ungroup() %>%
  left_join(select(def, row_id, GW), by="row_id")

cat("Chunk done\n")

## DEF 2.3: Lasso Regression ----

# 1. Aggregate each rolling window by column means (for each sample)
Xl <- do.call(rbind, lapply(numW$window, colMeans, na.rm = TRUE))
length(Xl)
sum(is.na(Xl))

# 2. Extract the target variable for each window
yl <- numW$target

# 3. Perform cross-validated Lasso regression to select lambda
cvfit <- cv.glmnet(Xl, yl, alpha = 1)

# 4. Get the lambda value that minimizes cross-validated error
minlambda <- cvfit$lambda.min

# 5. Fit Lasso regression with the selected lambda
lassofit <- glmnet(Xl, yl, alpha = 1, lambda = minlambda)

# 6. Extract the names of features with non-zero coefficients (selected by Lasso)
keepF <- rownames(as.matrix(coef(lassofit)))[coef(lassofit)[, 1] != 0] %>%
  setdiff("(Intercept)")

# 7. Update numF to only include selected features
numF <- keepF

cat("Lasso selected features:", numF,"Initial number of features:", initital_numF, "\n",
    "Selected number of features:",length(numF), "\n")

## DEF 2.4.1: Rebuild Numeric Windows (numW) with Lasso selected variables ----
ws <- vindu
# Rebuild rolling numerical windows using only Lasso-selected features (numF)
# Why: After feature selection, 
# we want to ensure that only the most relevant variables are used for modeling, 
# which can improve model performance and reduce overfitting.
numW <- def %>%
  group_by(pID_idx) %>%
  filter(n() > ws) %>%
  group_modify(~{
    W     <- slide(.x[, numF], .f=~as.matrix(.x),
                   .before=ws, .after=-1, .complete=TRUE)
    valid <- W[(ws+1):length(W)]
    tgt   <- .x[[tar]][(ws+1):nrow(.x)]
    rid   <- .x$row_id[(ws+1):nrow(.x)]
    tibble(window=valid, target=tgt, row_id=rid)
  }) %>% ungroup() %>%
  left_join(select(def, row_id, GW), by="row_id")
cat("Chunk done\n")

### DEF 2.4.2: Build & align categorical windows (by player_id!) ----

# Build rolling windows for categorical variables (pID_idx, tID_idx, oID_idx, hID) for each player
catW_all <- def %>%
  group_by(player_id) %>%
  filter(n() > ws) %>%
  group_modify(~{
    # For each player, create rolling windows of categorical indices
    W <- slide(
      .x %>% select(pID_idx, tID_idx, oID_idx, hID),
      .f = ~as.matrix(.x), .before = ws, .after = -1, .complete = TRUE
    )
    # Keep only valid windows (those with enough history)
    valid <- W[(ws + 1):length(W)]
    # Get corresponding row_ids for alignment
    rid <- .x$row_id[(ws + 1):nrow(.x)]
    tibble(window = valid, row_id = rid)
  }) %>% ungroup()

# Align categorical windows to numeric windows using row_id
catW <- numW %>%
  select(row_id) %>%
  left_join(catW_all, by = "row_id")

### DEF 2.4.3: To Arrays
# Number of samples (number of rolling windows)
nSamp <- nrow(numW)

# Convert list of rolling numerical windows to a 3D array: [samples, window size, num features]
num_array <- array(unlist(numW$window), dim = c(nSamp, ws, length(numF)))

# Extract targets (scaled total_points) as a column matrix
targets <- matrix(numW$target, ncol = 1)

# Extract historical categorical indices for each window (for possible use in sequence models)
pID_hist_array <- t(sapply(catW$window, function(m) m[, "pID_idx"]))
tID_hist_array <- t(sapply(catW$window, function(m) m[, "tID_idx"]))
oID_hist_array <- t(sapply(catW$window, function(m) m[, "oID_idx"]))
hID_hist_array <- t(sapply(catW$window, function(m) m[, "hID"]))

# Extract current (last in window) categorical values for each sample
cat_cur_mat <- do.call(rbind, lapply(catW$window, function(m) m[nrow(m), ]))
pID_cur_array <- matrix(cat_cur_mat[, "pID_idx"], ncol = 1)
tID_cur_array <- matrix(cat_cur_mat[, "tID_idx"], ncol = 1)
oID_cur_array <- matrix(cat_cur_mat[, "oID_idx"], ncol = 1)
hID_cur_array <- matrix(cat_cur_mat[, "hID"], ncol = 1)


## DEF 2.5: Split Data into Training and Validation Sets ----
idx <- which(numW$GW <= split_gw)
Xnum_train <- num_array[idx,,,drop=FALSE]
Xnum_val <- num_array[-idx,,,drop=FALSE]
y_train <- targets[idx,,drop=FALSE]
y_val <- targets[-idx,,drop=FALSE]

Xcat_hist_tr <- list(
  pID_hist = pID_hist_array[idx,,drop=FALSE],
  tID_hist = tID_hist_array[idx,,drop=FALSE],
  oID_hist = oID_hist_array[idx,,drop=FALSE],
  hID_hist = hID_hist_array[idx,,drop=FALSE]
)
Xcat_hist_va <- list(
  pID_hist = pID_hist_array[-idx,,drop=FALSE],
  tID_hist = tID_hist_array[-idx,,drop=FALSE],
  oID_hist = oID_hist_array[-idx,,drop=FALSE],
  hID_hist = hID_hist_array[-idx,,drop=FALSE]
)

Xcat_cur_tr <- list(
  pID_cur = pID_cur_array[idx,,drop=FALSE],
  tID_cur = tID_cur_array[idx,,drop=FALSE],
  oID_cur = oID_cur_array[idx,,drop=FALSE],
  hID_cur = hID_cur_array[idx,,drop=FALSE]
)
Xcat_cur_va <- list(
  pID_cur = pID_cur_array[-idx,,drop=FALSE],
  tID_cur = tID_cur_array[-idx,,drop=FALSE],
  oID_cur = oID_cur_array[-idx,,drop=FALSE],
  hID_cur = hID_cur_array[-idx,,drop=FALSE]
)

cat("Chunk Done\n")

## DEF 2.6: Building the Keras Neural Network Model ----
### DEF Dimensions of input layers ----
# nP == Number of players, nT == Number of teams, nO == Number of opponent teams
nP <- max(def$pID_idx, na.rm=TRUE)
nT <- max(def$tID_idx, na.rm=TRUE)
nO <- max(def$oID_idx, na.rm=TRUE)

### DEF Input Layers ----
inp_num <- layer_input(shape=c(ws,length(numF)), name="input_seq")
inp_ph  <- layer_input(shape=c(ws), dtype="int32", name="pID_hist")
inp_th  <- layer_input(shape=c(ws), dtype="int32", name="tID_hist")
inp_oh  <- layer_input(shape=c(ws), dtype="int32", name="oID_hist")
inp_hh  <- layer_input(shape=c(ws), dtype="int32", name="hID_hist")
inp_pc  <- layer_input(shape=c(1),   dtype="int32", name="pID_cur")
inp_tc  <- layer_input(shape=c(1),   dtype="int32", name="tID_cur")
inp_oc  <- layer_input(shape=c(1),   dtype="int32", name="oID_cur")
inp_hc  <- layer_input(shape=c(1),   dtype="int32", name="hID_cur")

### DEF Embedding Layers ----
emb_ph <- inp_ph %>%
  layer_embedding(input_dim = nP + 1, output_dim = emb_dim, mask_zero = TRUE)
emb_th <- inp_th %>%
  layer_embedding(input_dim = nT + 1, output_dim = emb_dim, mask_zero = TRUE)
emb_oh <- inp_oh %>%
  layer_embedding(input_dim = nO + 1, output_dim = emb_dim, mask_zero = TRUE)
emb_hh <- inp_hh %>%
  layer_lambda(function(x){
    x_f <- tf$cast(x, tf$float32)
    tf$expand_dims(x_f, axis = as.integer(-1))
  })

### DEF Concatenate Historical Embeddings for LSTM Sequence ----
cat_hist_seq <- layer_concatenate(list(emb_ph,emb_th,emb_oh,emb_hh), axis=-1)

### DEF LSTM Layer for Sequential Modeling ----
lstm_in  <- layer_concatenate(list(inp_num, cat_hist_seq), axis=-1)
lstm_out <- lstm_in %>%
  layer_lstm(units=64, return_sequences=FALSE) %>%
  layer_dropout(rate=0.2)

### DEF Embedding Layers for Current Categorical Inputs ----
emb_pc <- inp_pc %>%
  layer_embedding(input_dim = nP + 1, output_dim = emb_dim, mask_zero = TRUE) %>%
  layer_flatten()
emb_tc <- inp_tc %>%
  layer_embedding(input_dim = nT + 1, output_dim = emb_dim, mask_zero = TRUE) %>%
  layer_flatten()
emb_oc <- inp_oc %>%
  layer_embedding(input_dim = nO + 1, output_dim = emb_dim, mask_zero = TRUE) %>%
  layer_flatten()
emb_hc <- inp_hc %>%
  layer_lambda(function(x) tf$cast(x, tf$float32))

### DEF Concatenate Current Categorical Embeddings ----
cat_cur_vec <- layer_concatenate(list(emb_pc,emb_tc,emb_oc,emb_hc), axis=-1)

### DEF Merge LSTM Output and Current Categorical Features ----
merged <- layer_concatenate(list(lstm_out, cat_cur_vec), axis=-1)
head   <- merged %>% layer_dense(units=32, activation="relu")
out    <- head   %>% layer_dense(units=1,  activation="linear")

### DEF Build and Compile Model ----
model <- keras_model(
  inputs  = list(inp_num, inp_ph, inp_th, inp_oh, inp_hh,
                 inp_pc, inp_tc, inp_oc, inp_hc),
  outputs = out,
  name = "def_lstm_model"
)

model %>% compile(
  optimizer = optimizer_adam(),
  loss      = "mse",
  metrics   = metric_root_mean_squared_error()
)

### DEF Model Summary and Visualization ----
summary(model)
plot(model)

### DEF Save Model Plot to File ----
keras_python <- import("keras")
keras_python$utils$plot_model(
  model,
  to_file = "keras-model-def.png",
  show_shapes = TRUE,
  show_dtype = FALSE,
  show_layer_names = TRUE,
  rankdir = "TB",
  expand_nested = FALSE,
  dpi = 200,
  show_layer_activations = FALSE
)

### DEF Display Model Plot in R ----
library(png)
img <- readPNG("keras-model-def.png")
grid::grid.raster(img)

## DEF 2.7: Callback Functions and Scaling Factors ----

# Filepath for saving the BEST weights (unique for each position)
weights_filepath_def <- "R Forecast/best_def.hdf5"
cat("Will save best DEF weights to:", weights_filepath_def, "\n")

# 1. Tidlig stopp funksjon, laster inn siste beste loss value.
# patience: how many epochs to wait after last improvement before stopping
# restore_best_weights: load the weights from the epoch with the best val_loss
early_stopping <- callback_early_stopping(
  monitor = "val_loss",
  patience = num_patience, 
  restore_best_weights = TRUE
)

# 2. Model Checkpoint Callback (to save best weights)
model_checkpoint <- callback_model_checkpoint(
  filepath = weights_filepath_def, # Path defined above
  monitor = "val_loss",           # Monitor the same metric as early stopping
  save_best_only = TRUE,          # IMPORTANT: Only save when val_loss improves
  save_weights_only = TRUE,       # IMPORTANT: Save ONLY the weights, not the whole model
  mode = "min",                   # We want to minimize loss
  verbose = 1                     # Optional: print message when weights are saved
)

# 3. Reduce Learning Rate on Plateau Callback
reduce_lr <- callback_reduce_lr_on_plateau(
  monitor = "val_loss",  # Monitor validation loss (same as early stopping)
  factor = 0.2,          # Reduce LR to 20% of its current value (lr * factor)
  patience = num_patience,           # Reduce LR if val_loss doesn't improve for 5 epochs
  min_lr = 1e-6,         # Don't reduce the LR below this value
  verbose = 1            # Print a message when LR is reduced
)

scaling_factors$def <- list(mu = mu,
                            sigma = sigma,
                            numF = numF) # numF here is the one selected by Lasso for DEF
cat("DEF scaling factors stored.\n")

cat("--- Checking DEF data before fit() ---\n")
cat("Xnum_train (DEF) - NAs:", sum(is.na(Xnum_train)), " Infs:", sum(is.infinite(Xnum_train)), " Dims:", dim(Xnum_train), "\n")
cat("y_train (DEF) - NAs:", sum(is.na(y_train)), " Infs:", sum(is.infinite(y_train)), " Dims:", dim(y_train), "\n")
cat("Xnum_val (DEF) - NAs:", sum(is.na(Xnum_val)), " Infs:", sum(is.infinite(Xnum_val)), " Dims:", dim(Xnum_val), "\n")
cat("y_val (DEF) - NAs:", sum(is.na(y_val)), " Infs:", sum(is.infinite(y_val)), " Dims:", dim(y_val), "\n")
# Optional: Check categorical arrays too if you suspect issues there
# cat("Xcat_hist_tr$pID_hist (DEF) - NAs:", sum(is.na(Xcat_hist_tr$pID_hist)), "\n")
# cat("Xcat_cur_tr$pID_cur (DEF) - NAs:", sum(is.na(Xcat_cur_tr$pID_cur)), "\n")
cat("DEF mu:", scaling_factors$def$mu, " sigma:", scaling_factors$def$sigma, "\n") # Check scaling factors used
cat("--------------------------------------\n")

# Add this check too, inside section 2.5 after creating y_val
cat("DEF y_val summary (scaled):", summary(y_val), "\n")
cat("DEF y_train summary (scaled):", summary(y_train), "\n")
## DEF 2.8: Train Defender LSTM Model ----

cat("Starting DEF model training...\n")

history <- model %>% fit(
  x = c(
    list(input_seq = Xnum_train),
    Xcat_hist_tr,
    Xcat_cur_tr
  ),
  y = y_train,
  epochs = epoker,
  batch_size = 16,
  validation_data = list(
    c( # Ensure validation inputs are correctly listed here
      list(input_seq = Xnum_val),
      Xcat_hist_va,
      Xcat_cur_va
    ),
    y_val # Keep the validation target here
  ),
  callbacks = list(early_stopping, model_checkpoint, reduce_lr)
)
cat("DEF model training finished.\n")


## DEF 2.9: Generate Predictions & Invert Scaling on Validation Set ----
# Use the validation set for predictions
val_row_ids <- numW$row_id[-idx]

pred_all <- model %>% predict(
  c(
    list(input_seq = Xnum_val),
    Xcat_hist_va,
    Xcat_cur_va
  )
)

preds_def <- tibble(
  row_id                         = val_row_ids,
  predicted_total_points_scaled  = as.vector(pred_all),
  actual_total_points_scaled     = as.vector(y_val)
) %>%
  mutate(
    predicted_total_points = predicted_total_points_scaled * sigma + mu,
    actual_total_points    = actual_total_points_scaled    * sigma + mu
  ) %>%
  left_join(unscaled_def, by = "row_id")

glimpse(preds_def)
str(preds_def)

# Build a predictions table using the validation row IDs
preds_def <- tibble(
  row_id = val_row_ids,  # row IDs corresponding to the validation set
  predicted_total_points_scaled = as.vector(pred_all),
  actual_total_points_scaled = as.vector(y_val)
) %>% 
  mutate(
    predicted_total_points = predicted_total_points_scaled * sigma + mu,
    actual_total_points = actual_total_points_scaled * sigma + mu
  )


preds_def <- preds_def %>%
  left_join(unscaled_def, by = "row_id") # Use unscaled_def

# View the predictions table
glimpse(preds_def)

## DEF 2.10 Save model and validation set predictions ----
# Store DEF model and scaling factors 
model_list$def <- model
cat("DEF model stored in model_list$def.\n")

# Clean up validation predictions (select essential columns)
preds_def_clean <- preds_def %>%
  select(row_id, GW, player_id, name, position, team,
         actual_total_points, predicted_total_points, value)
cat("Validation predictions stored in preds_def_clean.\n")

## DEF 2.11: Plots for Evaluation ----

### DEF 2.11.1 Save training history ----
history_def <- history # class() "keras_training_history"
history_df_def <- data.frame(history_def)

### DEF 2.11.2 Training History Plot ----
plot_history_def <- plot(history_def)
plot_history_def;str(history_df_def)

### DEF 2.11.3 Actual vs Predicted Points Plot ----
plot_actual_predicted_def <- ggplot(preds_def, aes(x = actual_total_points, y = predicted_total_points)) +
  geom_point(alpha = 0.5) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Actual vs. Predicted Total Points (Validation Set)",
       x = "Actual Total Points",
       y = "Predicted Total Points") +
  facet_wrap(~ position) +
  theme_grey() +
  coord_cartesian(xlim = range(preds_def$actual_total_points, na.rm = TRUE),
                  ylim = range(preds_def$predicted_total_points, na.rm = TRUE))
plot_actual_predicted_def                  

### DEF 2.11.4: Residuals vs Predicted ----
preds_def_residuals <- preds_def %>%
  mutate(residual = actual_total_points - predicted_total_points)

plot_residuals_predicted_def <- ggplot(preds_def_residuals, aes(x = predicted_total_points, y = residual)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs. Predicted Total Points (Validation Set)",
       x = "Predicted Total Points",
       y = "Residual (Actual - Predicted)") +
  facet_wrap(~ position) +
  theme_grey()  
plot_residuals_predicted_def

### DEF 2.11.5: Distribution of Residuals ----
plot_distribution_residuals_def <- ggplot(preds_def_residuals, aes(x = residual)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "blue", alpha = 0.7) +
  geom_density(color = "red") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Distribution of Residuals (Validation Set)",
       x = "Residual (Actual - Predicted)",
       y = "Density") +
  facet_wrap(~ position) +
  theme_grey()
plot_distribution_residuals_def

### DEF 2.11.6 Return list containing loss value and metrics ----
# Evaluate model on validation data using consistent variable names
model_evaluation <- model |> evaluate(
  c(
    list(input_seq = Xnum_val),
    Xcat_hist_va,
    Xcat_cur_va
  ),
  y_val,
  verbose = 1
)
# Print the evaluation results
print(model_evaluation)

# Store validation metrics
validation_metrics$def <- model_evaluation
cat("Validation metrics stored in validation_metrics$def.\n")

# 3: Midfielders (MID) ----

## MID 3.1 Define Features, Target & Scale ----
#numF = numerical features, tar = target
numF <- c("assists", "creativity", "minutes", "goals_conceded",
          "bonus", "bps", "expected_assists", "expected_goal_involvements",
          "expected_goals", "ict_index", "own_goals", "red_cards",
          "threat", "transfers_in", "transfers_out", "yellow_cards",
          "expected_goals_conceded", "value",
          "selected", "transfers_balance", "starts", "influence",
          "clean_sheets"
)
tar <- "total_points"

cat("Numeriske features:", numF, "\n")
cat("Target variabel:", tar, "\n")
cat("Ferdig\n")

mid <- mid %>%
  arrange(pID_idx, GW) %>%
  group_by(pID_idx) %>%
  ungroup()
# Compute and store the mean and standard deviation for the target variable

mu <- mean(mid[[tar]], na.rm = TRUE)
sigma <- sd(mid[[tar]], na.rm = TRUE)

# Scale numerical features and target
mid <- mid %>%
  mutate(across(all_of(numF), 
                ~ (.x - mean(.x, na.rm = TRUE)) / sd(.x, na.rm = TRUE)),
         !!tar := (.data[[tar]] - mu) / sigma)
cat("Chunk done")

## MID 3.2: Build rolling numerical windows ----

# Define window size (number of past gameweeks to use)
ws <- vindu

# Build numerical rolling windows (numW)
numW <- mid %>%
  group_by(pID_idx) %>%
  filter(n() > ws) %>%
  group_modify(~{
    W   <- slide(.x[, numF], .f=~as.matrix(.x),
                 .before=ws, .after=-1, .complete=TRUE)
    valid <- W[(ws+1):length(W)]
    tgt   <- .x[[tar]][(ws+1):nrow(.x)]
    rid   <- .x$row_id[(ws+1):nrow(.x)]
    tibble(window=valid, target=tgt, row_id=rid)
  }) %>% ungroup() %>%
  left_join(select(mid, row_id, GW), by="row_id")

cat("Chunk done\n")

## MID 3.3: Lasso Regression ----

# 1. Aggregate each rolling window by column means (for each sample)
Xl <- do.call(rbind, lapply(numW$window, colMeans, na.rm = TRUE))

# 2. Extract the target variable for each window
yl <- numW$target

# 3. Perform cross-validated Lasso regression to select lambda
cvfit <- cv.glmnet(Xl, yl, alpha = 1)

# 4. Get the lambda value that minimizes cross-validated error
minlambda <- cvfit$lambda.min

# 5. Fit Lasso regression with the selected lambda
lassofit <- glmnet(Xl, yl, alpha = 1, lambda = minlambda)

# 6. Extract the names of features with non-zero coefficients (selected by Lasso)
keepF <- rownames(as.matrix(coef(lassofit)))[coef(lassofit)[, 1] != 0] %>%
  setdiff("(Intercept)")

# 7. Update numF to only include selected features
numF <- keepF

## MID 3.4.1: Rebuild Numeric Windows (numW) with Lasso selected variables ----
ws <- vindu
# Rebuild rolling numerical windows using only Lasso-selected features (numF)
# Why: After feature selection, 
# we want to ensure that only the most relevant variables are used for modeling, 
# which can improve model performance and reduce overfitting.
numW <- mid %>%
  group_by(pID_idx) %>%
  filter(n() > ws) %>%
  group_modify(~{
    W     <- slide(.x[, numF], .f=~as.matrix(.x),
                   .before=ws, .after=-1, .complete=TRUE)
    valid <- W[(ws+1):length(W)]
    tgt   <- .x[[tar]][(ws+1):nrow(.x)]
    rid   <- .x$row_id[(ws+1):nrow(.x)]
    tibble(window=valid, target=tgt, row_id=rid)
  }) %>% ungroup() %>%
  left_join(select(mid, row_id, GW), by="row_id")

cat("Chunk done\n")

### MID 3.4.2: Build & align categorical windows (by player_id!) ----

# Build rolling windows for categorical variables (pID_idx, tID_idx, oID_idx, hID) for each player
catW_all <- mid %>%
  group_by(player_id) %>%
  filter(n() > ws) %>%
  group_modify(~{
    # For each player, create rolling windows of categorical indices
    W <- slide(
      .x %>% select(pID_idx, tID_idx, oID_idx, hID),
      .f = ~as.matrix(.x), .before = ws, .after = -1, .complete = TRUE
    )
    # Keep only valid windows (those with enough history)
    valid <- W[(ws + 1):length(W)]
    # Get corresponding row_ids for alignment
    rid <- .x$row_id[(ws + 1):nrow(.x)]
    tibble(window = valid, row_id = rid)
  }) %>% ungroup()

# Align categorical windows to numeric windows using row_id
catW <- numW %>%
  select(row_id) %>%
  left_join(catW_all, by = "row_id")

### MID 3.4.3: To Arrays
# Number of samples (number of rolling windows)
nSamp <- nrow(numW)

# Convert list of rolling numerical windows to a 3D array: [samples, window size, num features]
num_array <- array(unlist(numW$window), dim = c(nSamp, ws, length(numF)))

# Extract targets (scaled total_points) as a column matrix
targets <- matrix(numW$target, ncol = 1)

# Extract historical categorical indices for each window (for possible use in sequence models)
pID_hist_array <- t(sapply(catW$window, function(m) m[, "pID_idx"]))
tID_hist_array <- t(sapply(catW$window, function(m) m[, "tID_idx"]))
oID_hist_array <- t(sapply(catW$window, function(m) m[, "oID_idx"]))
hID_hist_array <- t(sapply(catW$window, function(m) m[, "hID"]))

# Extract current (last in window) categorical values for each sample
cat_cur_mat <- do.call(rbind, lapply(catW$window, function(m) m[nrow(m), ]))
pID_cur_array <- matrix(cat_cur_mat[, "pID_idx"], ncol = 1)
tID_cur_array <- matrix(cat_cur_mat[, "tID_idx"], ncol = 1)
oID_cur_array <- matrix(cat_cur_mat[, "oID_idx"], ncol = 1)
hID_cur_array <- matrix(cat_cur_mat[, "hID"], ncol = 1)


## MID 3.5: Split Data into Training and Validation Sets ----
idx <- which(numW$GW <= split_gw)
Xnum_train <- num_array[idx,,,drop=FALSE]
Xnum_val <- num_array[-idx,,,drop=FALSE]
y_train <- targets[idx,,drop=FALSE]
y_val <- targets[-idx,,drop=FALSE]

Xcat_hist_tr <- list(
  pID_hist = pID_hist_array[idx,,drop=FALSE],
  tID_hist = tID_hist_array[idx,,drop=FALSE],
  oID_hist = oID_hist_array[idx,,drop=FALSE],
  hID_hist = hID_hist_array[idx,,drop=FALSE]
)
Xcat_hist_va <- list(
  pID_hist = pID_hist_array[-idx,,drop=FALSE],
  tID_hist = tID_hist_array[-idx,,drop=FALSE],
  oID_hist = oID_hist_array[-idx,,drop=FALSE],
  hID_hist = hID_hist_array[-idx,,drop=FALSE]
)

Xcat_cur_tr <- list(
  pID_cur = pID_cur_array[idx,,drop=FALSE],
  tID_cur = tID_cur_array[idx,,drop=FALSE],
  oID_cur = oID_cur_array[idx,,drop=FALSE],
  hID_cur = hID_cur_array[idx,,drop=FALSE]
)
Xcat_cur_va <- list(
  pID_cur = pID_cur_array[-idx,,drop=FALSE],
  tID_cur = tID_cur_array[-idx,,drop=FALSE],
  oID_cur = oID_cur_array[-idx,,drop=FALSE],
  hID_cur = hID_cur_array[-idx,,drop=FALSE]
)

cat("Chunk Done\n")

## MID 3.6: Building the Keras Neural Network Model ----
### MID Dimensions of input layers ----
# nP == Number of players, nT == Number of teams, nO == Number of opponent teams
nP <- max(mid$pID_idx, na.rm=TRUE)
nT <- max(mid$tID_idx, na.rm=TRUE)
nO <- max(mid$oID_idx, na.rm=TRUE)

### MID Input Layers ----
inp_num <- layer_input(shape=c(ws,length(numF)), name="input_seq")
inp_ph  <- layer_input(shape=c(ws), dtype="int32", name="pID_hist")
inp_th  <- layer_input(shape=c(ws), dtype="int32", name="tID_hist")
inp_oh  <- layer_input(shape=c(ws), dtype="int32", name="oID_hist")
inp_hh  <- layer_input(shape=c(ws), dtype="int32", name="hID_hist")
inp_pc  <- layer_input(shape=c(1),   dtype="int32", name="pID_cur")
inp_tc  <- layer_input(shape=c(1),   dtype="int32", name="tID_cur")
inp_oc  <- layer_input(shape=c(1),   dtype="int32", name="oID_cur")
inp_hc  <- layer_input(shape=c(1),   dtype="int32", name="hID_cur")

### MID Embedding Layers ----
emb_ph <- inp_ph %>%
  layer_embedding(input_dim = nP + 1, output_dim = emb_dim, mask_zero = TRUE)
emb_th <- inp_th %>%
  layer_embedding(input_dim = nT + 1, output_dim = emb_dim, mask_zero = TRUE)
emb_oh <- inp_oh %>%
  layer_embedding(input_dim = nO + 1, output_dim = emb_dim, mask_zero = TRUE)
emb_hh <- inp_hh %>%
  layer_lambda(function(x){
    x_f <- tf$cast(x, tf$float32)
    tf$expand_dims(x_f, axis = as.integer(-1))
  })

### MID Concatenate Historical Embeddings for LSTM Sequence ----
cat_hist_seq <- layer_concatenate(list(emb_ph,emb_th,emb_oh,emb_hh), axis=-1)

### MID LSTM Layer for Sequential Modeling ----
lstm_in  <- layer_concatenate(list(inp_num, cat_hist_seq), axis=-1)
lstm_out <- lstm_in %>%
  layer_lstm(units=64, return_sequences=FALSE) %>%
  layer_dropout(rate=0.2)

### MID Embedding Layers for Current Categorical Inputs ----
emb_pc <- inp_pc %>%
  layer_embedding(input_dim = nP + 1, output_dim = emb_dim, mask_zero = TRUE) %>%
  layer_flatten()
emb_tc <- inp_tc %>%
  layer_embedding(input_dim = nT + 1, output_dim = emb_dim, mask_zero = TRUE) %>%
  layer_flatten()
emb_oc <- inp_oc %>%
  layer_embedding(input_dim = nO + 1, output_dim = emb_dim, mask_zero = TRUE) %>%
  layer_flatten()
emb_hc <- inp_hc %>%
  layer_lambda(function(x) tf$cast(x, tf$float32))

### MID Concatenate Current Categorical Embeddings ----
cat_cur_vec <- layer_concatenate(list(emb_pc,emb_tc,emb_oc,emb_hc), axis=-1)

### MID Merge LSTM Output and Current Categorical Features ----
merged <- layer_concatenate(list(lstm_out, cat_cur_vec), axis=-1)
head   <- merged %>% layer_dense(units=32, activation="relu")
out    <- head   %>% layer_dense(units=1,  activation="linear")

### MID Build and Compile Model ----
model <- keras_model(
  inputs  = list(inp_num, inp_ph, inp_th, inp_oh, inp_hh,
                 inp_pc, inp_tc, inp_oc, inp_hc),
  outputs = out,
  name = "mid_lstm_model"
)

model %>% compile(
  optimizer = optimizer_adam(),
  loss      = "mse",
  metrics   = metric_root_mean_squared_error()
)

### MID Model Summary and Visualization ----
summary(model)
plot(model)

### MID Save Model Plot to File ----
keras_python <- import("keras")
keras_python$utils$plot_model(
  model,
  to_file = "keras-model-mid.png",
  show_shapes = TRUE,
  show_dtype = FALSE,
  show_layer_names = TRUE,
  rankdir = "TB",
  expand_nested = FALSE,
  dpi = 200,
  show_layer_activations = FALSE
)

### MID Display Model Plot in R ----
library(png)
img <- readPNG("keras-model-mid.png")
grid::grid.raster(img)

## MID 3.7: Callback Functions and Scaling Factors ----

# Filepath for saving the BEST weights (unique for each position)
weights_filepath_mid <- "R Forecast/best_mid.hdf5"
cat("Will save best MID weights to:", weights_filepath_mid, "\n")

# 1. Tidlig stopp funksjon, laster inn siste beste loss value.
# patience: how many epochs to wait after last improvement before stopping
# restore_best_weights: load the weights from the epoch with the best val_loss
early_stopping <- callback_early_stopping(
  monitor = "val_loss",
  patience = num_patience, 
  restore_best_weights = TRUE
)

# 2. Model Checkpoint Callback (to save best weights)
model_checkpoint <- callback_model_checkpoint(
  filepath = weights_filepath_mid, # Path midined above
  monitor = "val_loss",           # Monitor the same metric as early stopping
  save_best_only = TRUE,          # IMPORTANT: Only save when val_loss improves
  save_weights_only = TRUE,       # IMPORTANT: Save ONLY the weights, not the whole model
  mode = "min",                   # We want to minimize loss
  verbose = 1                     # Optional: print message when weights are saved
)

# 3. Reduce Learning Rate on Plateau Callback
reduce_lr <- callback_reduce_lr_on_plateau(
  monitor = "val_loss",  # Monitor validation loss (same as early stopping)
  factor = 0.2,          # Reduce LR to 20% of its current value (lr * factor)
  patience = num_patience,           # Reduce LR if val_loss doesn't improve for 5 epochs
  min_lr = 1e-6,         # Don't reduce the LR below this value
  verbose = 1            # Print a message when LR is reduced
)

scaling_factors$mid <- list(mu = mu,
                            sigma = sigma,
                            numF = numF) # numF here is the one selected by Lasso for MID
cat("MID scaling factors stored.\n")

## MID 3.8: Train Midfielders LSTM Model ----

cat("Starting MID model training...\n")

history <- model %>% fit(
  x = c(
    list(input_seq = Xnum_train),
    Xcat_hist_tr,
    Xcat_cur_tr
  ),
  y              = y_train,
  epochs         = epoker,
  batch_size     = 16,
  validation_data= list(
    c(
      list(input_seq = Xnum_val),
      Xcat_hist_va,
      Xcat_cur_va
    ),
    y_val
  ),
  callbacks = list(early_stopping, model_checkpoint, reduce_lr)
)
cat("MID model training finished.\n")


## MID 3.9: Generate Predictions & Invert Scaling on Validation Set ----
# Use the validation set for predictions
val_row_ids <- numW$row_id[-idx]

pred_all <- model %>% predict(
  c(
    list(input_seq = Xnum_val),
    Xcat_hist_va,
    Xcat_cur_va
  )
)

preds_mid <- tibble(
  row_id                         = val_row_ids,
  predicted_total_points_scaled  = as.vector(pred_all),
  actual_total_points_scaled     = as.vector(y_val)
) %>%
  mutate(
    predicted_total_points = predicted_total_points_scaled * sigma + mu,
    actual_total_points    = actual_total_points_scaled    * sigma + mu
  ) %>%
  left_join(unscaled_mid, by = "row_id")

glimpse(preds_mid)

# Build a predictions table using the validation row IDs
preds_mid <- tibble(
  row_id = val_row_ids,  # row IDs corresponding to the validation set
  predicted_total_points_scaled = as.vector(pred_all),
  actual_total_points_scaled = as.vector(y_val)
) %>% 
  mutate(
    predicted_total_points = predicted_total_points_scaled * sigma + mu,
    actual_total_points = actual_total_points_scaled * sigma + mu
  )


preds_mid <- preds_mid %>%
  left_join(unscaled_mid, by = "row_id") # Use unscaled_mid

# View the predictions table
glimpse(preds_mid)

## MID 3.10 Save model and validation set predictions ----
# Store MID model and scaling factors 
model_list$mid <- model
cat("MID model stored in model_list$mid.\n")

# Clean up validation predictions (select essential columns)
preds_mid_clean <- preds_mid %>%
  select(row_id, GW, player_id, name, position, team,
         actual_total_points, predicted_total_points, value)
cat("Validation predictions stored in preds_mid_clean.\n")

## MID 3.11: Plots for Evaluation ----

### MID 3.11.1 Save training history ----
history_mid <- history # class() "keras_training_history"
history_df_mid <- data.frame(history_mid)

### MID 3.11.2 Training History Plot ----
plot_history_mid <- plot(history_mid)
plot_history_mid;str(history_df_mid)

### MID 3.11.3 Actual vs Predicted Points Plot ----
plot_actual_predicted_mid <- ggplot(preds_mid, aes(x = actual_total_points, y = predicted_total_points)) +
  geom_point(alpha = 0.5) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Actual vs. Predicted Total Points (Validation Set)",
       x = "Actual Total Points",
       y = "Predicted Total Points") +
  facet_wrap(~ position) +
  theme_grey() +
  coord_cartesian(xlim = range(preds_mid$actual_total_points, na.rm = TRUE),
                  ylim = range(preds_mid$predicted_total_points, na.rm = TRUE))
plot_actual_predicted_mid                  

### MID 3.11.4: Residuals vs Predicted ----
preds_mid_residuals <- preds_mid %>%
  mutate(residual = actual_total_points - predicted_total_points)

plot_residuals_predicted_mid <- ggplot(preds_mid_residuals, aes(x = predicted_total_points, y = residual)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs. Predicted Total Points (Validation Set)",
       x = "Predicted Total Points",
       y = "Residual (Actual - Predicted)") +
  facet_wrap(~ position) +
  theme_grey()  
plot_residuals_predicted_mid

### MID 3.11.5: Distribution of Residuals ----
plot_distribution_residuals_mid <- ggplot(preds_mid_residuals, aes(x = residual)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "blue", alpha = 0.7) +
  geom_density(color = "red") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Distribution of Residuals (Validation Set)",
       x = "Residual (Actual - Predicted)",
       y = "Density") +
  facet_wrap(~ position) +
  theme_grey()
plot_distribution_residuals_mid

### MID 3.11.6 Return list containing loss value and metrics ----
# Evaluate model on validation data using consistent variable names
model_evaluation <- model |> evaluate(
  c(
    list(input_seq = Xnum_val),
    Xcat_hist_va,
    Xcat_cur_va
  ),
  y_val,
  verbose = 1
)
# Print the evaluation results
print(model_evaluation)

# Store validation metrics
validation_metrics$mid <- model_evaluation
cat("Validation metrics stored in validation_metrics$mid.\n")

# 4: Forwards (FWD) ----

## FWD 4.1 Define Features, Target & Scale ----
#numF = numerical features, tar = target
numF <- c("assists", "creativity", "minutes", "goals_conceded",
                  "bonus", "bps", "expected_assists", "expected_goal_involvements",
                  "expected_goals", "ict_index", "own_goals", "red_cards",
                  "threat", "transfers_in", "transfers_out", "yellow_cards",
                  "expected_goals_conceded", "value",
                  "selected", "transfers_balance", "starts", "influence",
                  "clean_sheets"
)
tar <- "total_points"

cat("Numeriske features:", numF, "\n")
cat("Target variabel:", tar, "\n")
cat("Ferdig\n")

fwd <- fwd %>%
  arrange(pID_idx, GW) %>%
  group_by(pID_idx) %>%
  ungroup()
# Compute and store the mean and standard deviation for the target variable

mu <- mean(fwd[[tar]], na.rm = TRUE)
sigma <- sd(fwd[[tar]], na.rm = TRUE)

# Scale numerical features and target
fwd <- fwd %>%
  mutate(across(all_of(numF), 
                ~ (.x - mean(.x, na.rm = TRUE)) / sd(.x, na.rm = TRUE)),
         !!tar := (.data[[tar]] - mu) / sigma)
cat("Chunk done")

## FWD 4.2: Build rolling numerical windows ----

# Define window size (number of past gameweeks to use)
ws <- vindu

# Build numerical rolling windows (numW)
numW <- fwd %>%
  group_by(pID_idx) %>%
  filter(n() > ws) %>%
  group_modify(~{
    W   <- slide(.x[, numF], .f=~as.matrix(.x),
                 .before=ws, .after=-1, .complete=TRUE)
    valid <- W[(ws+1):length(W)]
    tgt   <- .x[[tar]][(ws+1):nrow(.x)]
    rid   <- .x$row_id[(ws+1):nrow(.x)]
    tibble(window=valid, target=tgt, row_id=rid)
  }) %>% ungroup() %>%
  left_join(select(fwd, row_id, GW), by="row_id")

cat("Chunk done\n")

## FWD 4.3: Lasso Regression ----

# 1. Aggregate each rolling window by column means (for each sample)
Xl <- do.call(rbind, lapply(numW$window, colMeans, na.rm = TRUE))

# 2. Extract the target variable for each window
yl <- numW$target

# 3. Perform cross-validated Lasso regression to select lambda
cvfit <- cv.glmnet(Xl, yl, alpha = 1)

# 4. Get the lambda value that minimizes cross-validated error
minlambda <- cvfit$lambda.min

# 5. Fit Lasso regression with the selected lambda
lassofit <- glmnet(Xl, yl, alpha = 1, lambda = minlambda)

# 6. Extract the names of features with non-zero coefficients (selected by Lasso)
keepF <- rownames(as.matrix(coef(lassofit)))[coef(lassofit)[, 1] != 0] %>%
  setdiff("(Intercept)")

# 7. Update numF to only include selected features
numF <- keepF

## FWD 4.4.1: Rebuild Numeric Windows (numW) with Lasso selected variables ----
ws <- vindu
# Rebuild rolling numerical windows using only Lasso-selected features (numF)
# Why: After feature selection, 
# we want to ensure that only the most relevant variables are used for modeling, 
# which can improve model performance and reduce overfitting.
numW <- fwd %>%
  group_by(pID_idx) %>%
  filter(n() > ws) %>%
  group_modify(~{
    W     <- slide(.x[, numF], .f=~as.matrix(.x),
                   .before=ws, .after=-1, .complete=TRUE)
    valid <- W[(ws+1):length(W)]
    tgt   <- .x[[tar]][(ws+1):nrow(.x)]
    rid   <- .x$row_id[(ws+1):nrow(.x)]
    tibble(window=valid, target=tgt, row_id=rid)
  }) %>% ungroup() %>%
  left_join(select(fwd, row_id, GW), by="row_id")

cat("Chunk done\n")

### FWD 4.4.2: Build & align categorical windows (by player_id!) ----

# Build rolling windows for categorical variables (pID_idx, tID_idx, oID_idx, hID) for each player
catW_all <- fwd %>%
  group_by(player_id) %>%
  filter(n() > ws) %>%
  group_modify(~{
    # For each player, create rolling windows of categorical indices
    W <- slide(
      .x %>% select(pID_idx, tID_idx, oID_idx, hID),
      .f = ~as.matrix(.x), .before = ws, .after = -1, .complete = TRUE
    )
    # Keep only valid windows (those with enough history)
    valid <- W[(ws + 1):length(W)]
    # Get corresponding row_ids for alignment
    rid <- .x$row_id[(ws + 1):nrow(.x)]
    tibble(window = valid, row_id = rid)
  }) %>% ungroup()

# Align categorical windows to numeric windows using row_id
catW <- numW %>%
  select(row_id) %>%
  left_join(catW_all, by = "row_id")

### FWD 4.4.3: To Arrays
# Number of samples (number of rolling windows)
nSamp <- nrow(numW)

# Convert list of rolling numerical windows to a 3D array: [samples, window size, num features]
num_array <- array(unlist(numW$window), dim = c(nSamp, ws, length(numF)))

# Extract targets (scaled total_points) as a column matrix
targets <- matrix(numW$target, ncol = 1)

# Extract historical categorical indices for each window (for possible use in sequence models)
pID_hist_array <- t(sapply(catW$window, function(m) m[, "pID_idx"]))
tID_hist_array <- t(sapply(catW$window, function(m) m[, "tID_idx"]))
oID_hist_array <- t(sapply(catW$window, function(m) m[, "oID_idx"]))
hID_hist_array <- t(sapply(catW$window, function(m) m[, "hID"]))

# Extract current (last in window) categorical values for each sample
cat_cur_mat <- do.call(rbind, lapply(catW$window, function(m) m[nrow(m), ]))
pID_cur_array <- matrix(cat_cur_mat[, "pID_idx"], ncol = 1)
tID_cur_array <- matrix(cat_cur_mat[, "tID_idx"], ncol = 1)
oID_cur_array <- matrix(cat_cur_mat[, "oID_idx"], ncol = 1)
hID_cur_array <- matrix(cat_cur_mat[, "hID"], ncol = 1)


## FWD 4.5: Split Data into Training and Validation Sets ----
idx <- which(numW$GW <= split_gw)
Xnum_train <- num_array[idx,,,drop=FALSE]
Xnum_val <- num_array[-idx,,,drop=FALSE]
y_train <- targets[idx,,drop=FALSE]
y_val <- targets[-idx,,drop=FALSE]

Xcat_hist_tr <- list(
  pID_hist = pID_hist_array[idx,,drop=FALSE],
  tID_hist = tID_hist_array[idx,,drop=FALSE],
  oID_hist = oID_hist_array[idx,,drop=FALSE],
  hID_hist = hID_hist_array[idx,,drop=FALSE]
)
Xcat_hist_va <- list(
  pID_hist = pID_hist_array[-idx,,drop=FALSE],
  tID_hist = tID_hist_array[-idx,,drop=FALSE],
  oID_hist = oID_hist_array[-idx,,drop=FALSE],
  hID_hist = hID_hist_array[-idx,,drop=FALSE]
)

Xcat_cur_tr <- list(
  pID_cur = pID_cur_array[idx,,drop=FALSE],
  tID_cur = tID_cur_array[idx,,drop=FALSE],
  oID_cur = oID_cur_array[idx,,drop=FALSE],
  hID_cur = hID_cur_array[idx,,drop=FALSE]
)
Xcat_cur_va <- list(
  pID_cur = pID_cur_array[-idx,,drop=FALSE],
  tID_cur = tID_cur_array[-idx,,drop=FALSE],
  oID_cur = oID_cur_array[-idx,,drop=FALSE],
  hID_cur = hID_cur_array[-idx,,drop=FALSE]
)

cat("Chunk Done\n")

## FWD 4.6: Building the Keras Neural Network Model ----
### FWD Dimensions of input layers ----
# nP == Number of players, nT == Number of teams, nO == Number of opponent teams
nP <- max(fwd$pID_idx, na.rm=TRUE)
nT <- max(fwd$tID_idx, na.rm=TRUE)
nO <- max(fwd$oID_idx, na.rm=TRUE)

### FWD Input Layers ----
inp_num <- layer_input(shape=c(ws,length(numF)), name="input_seq")
inp_ph  <- layer_input(shape=c(ws), dtype="int32", name="pID_hist")
inp_th  <- layer_input(shape=c(ws), dtype="int32", name="tID_hist")
inp_oh  <- layer_input(shape=c(ws), dtype="int32", name="oID_hist")
inp_hh  <- layer_input(shape=c(ws), dtype="int32", name="hID_hist")
inp_pc  <- layer_input(shape=c(1),   dtype="int32", name="pID_cur")
inp_tc  <- layer_input(shape=c(1),   dtype="int32", name="tID_cur")
inp_oc  <- layer_input(shape=c(1),   dtype="int32", name="oID_cur")
inp_hc  <- layer_input(shape=c(1),   dtype="int32", name="hID_cur")

### FWD Embedding Layers ----
emb_ph <- inp_ph %>%
  layer_embedding(input_dim = nP + 1, output_dim = emb_dim, mask_zero = TRUE)
emb_th <- inp_th %>%
  layer_embedding(input_dim = nT + 1, output_dim = emb_dim, mask_zero = TRUE)
emb_oh <- inp_oh %>%
  layer_embedding(input_dim = nO + 1, output_dim = emb_dim, mask_zero = TRUE)
emb_hh <- inp_hh %>%
  layer_lambda(function(x){
    x_f <- tf$cast(x, tf$float32)
    tf$expand_dims(x_f, axis = as.integer(-1))
  })

### FWD Concatenate Historical Embeddings for LSTM Sequence ----
cat_hist_seq <- layer_concatenate(list(emb_ph,emb_th,emb_oh,emb_hh), axis=-1)

### FWD LSTM Layer for Sequential Modeling ----
lstm_in  <- layer_concatenate(list(inp_num, cat_hist_seq), axis=-1)
lstm_out <- lstm_in %>%
  layer_lstm(units=64, return_sequences=FALSE) %>%
  layer_dropout(rate=0.2)

### FWD Embedding Layers for Current Categorical Inputs ----
emb_pc <- inp_pc %>%
  layer_embedding(input_dim = nP + 1, output_dim = emb_dim, mask_zero = TRUE) %>%
  layer_flatten()
emb_tc <- inp_tc %>%
  layer_embedding(input_dim = nT + 1, output_dim = emb_dim, mask_zero = TRUE) %>%
  layer_flatten()
emb_oc <- inp_oc %>%
  layer_embedding(input_dim = nO + 1, output_dim = emb_dim, mask_zero = TRUE) %>%
  layer_flatten()
emb_hc <- inp_hc %>%
  layer_lambda(function(x) tf$cast(x, tf$float32))

### FWD Concatenate Current Categorical Embeddings ----
cat_cur_vec <- layer_concatenate(list(emb_pc,emb_tc,emb_oc,emb_hc), axis=-1)

### FWD Merge LSTM Output and Current Categorical Features ----
merged <- layer_concatenate(list(lstm_out, cat_cur_vec), axis=-1)
head   <- merged %>% layer_dense(units=32, activation="relu")
out    <- head   %>% layer_dense(units=1,  activation="linear")

### FWD Build and Compile Model ----
model <- keras_model(
  inputs  = list(inp_num, inp_ph, inp_th, inp_oh, inp_hh,
                 inp_pc, inp_tc, inp_oc, inp_hc),
  outputs = out,
  name = "fwd_lstm_model"
)

model %>% compile(
  optimizer = optimizer_adam(),
  loss      = "mse",
  metrics   = metric_root_mean_squared_error()
)

### FWD Model Summary and Visualization ----
summary(model)
plot(model)

### FWD Save Model Plot to File ----
keras_python <- import("keras")
keras_python$utils$plot_model(
  model,
  to_file = "keras-model-fwd.png",
  show_shapes = TRUE,
  show_dtype = FALSE,
  show_layer_names = TRUE,
  rankdir = "TB",
  expand_nested = FALSE,
  dpi = 200,
  show_layer_activations = FALSE
)

### FWD Display Model Plot in R ----
library(png)
img <- readPNG("keras-model-fwd.png")
grid::grid.raster(img)

## FWD 4.7: Callback Functions and Scaling Factors ----

# Filepath for saving the BEST weights (unique for each position)
weights_filepath_fwd <- "R Forecast/best_fwd.hdf5"
cat("Will save best FWD weights to:", weights_filepath_fwd, "\n")

# 1. Tidlig stopp funksjon, laster inn siste beste loss value.
# patience: how many epochs to wait after last improvement before stopping
# restore_best_weights: load the weights from the epoch with the best val_loss
early_stopping <- callback_early_stopping(
  monitor = "val_loss",
  patience = num_patience, 
  restore_best_weights = TRUE
)

# 2. Model Checkpoint Callback (to save best weights)
model_checkpoint <- callback_model_checkpoint(
  filepath = weights_filepath_fwd, # Path fwdined above
  monitor = "val_loss",           # Monitor the same metric as early stopping
  save_best_only = TRUE,          # IMPORTANT: Only save when val_loss improves
  save_weights_only = TRUE,       # IMPORTANT: Save ONLY the weights, not the whole model
  mode = "min",                   # We want to minimize loss
  verbose = 1                     # Optional: print message when weights are saved
)

# 3. Reduce Learning Rate on Plateau Callback
reduce_lr <- callback_reduce_lr_on_plateau(
  monitor = "val_loss",  # Monitor validation loss (same as early stopping)
  factor = 0.2,          # Reduce LR to 20% of its current value (lr * factor)
  patience = num_patience,           # Reduce LR if val_loss doesn't improve for 5 epochs
  min_lr = 1e-6,         # Don't reduce the LR below this value
  verbose = 1            # Print a message when LR is reduced
)

scaling_factors$fwd <- list(mu = mu,
                            sigma = sigma,
                            numF = numF) # numF here is the one selected by Lasso for FWD
cat("FWD scaling factors stored.\n")

## FWD 4.8: Train Forwards LSTM Model ----

cat("Starting FWD model training...\n")

history <- model %>% fit(
  x = c(
    list(input_seq = Xnum_train),
    Xcat_hist_tr,
    Xcat_cur_tr
  ),
  y              = y_train,
  epochs         = epoker,
  batch_size     = 16,
  validation_data= list(
    c(
      list(input_seq = Xnum_val),
      Xcat_hist_va,
      Xcat_cur_va
    ),
    y_val
  ),
  callbacks = list(early_stopping, model_checkpoint, reduce_lr)
)
cat("FWD model training finished.\n")


## FWD 4.9: Generate Predictions & Invert Scaling on Validation Set ----
# Use the validation set for predictions
val_row_ids <- numW$row_id[-idx]

pred_all <- model %>% predict(
  c(
    list(input_seq = Xnum_val),
    Xcat_hist_va,
    Xcat_cur_va
  )
)

preds_fwd <- tibble(
  row_id                         = val_row_ids,
  predicted_total_points_scaled  = as.vector(pred_all),
  actual_total_points_scaled     = as.vector(y_val)
) %>%
  mutate(
    predicted_total_points = predicted_total_points_scaled * sigma + mu,
    actual_total_points    = actual_total_points_scaled    * sigma + mu
  ) %>%
  left_join(unscaled_fwd, by = "row_id")

glimpse(preds_fwd)

# Build a predictions table using the validation row IDs
preds_fwd <- tibble(
  row_id = val_row_ids,  # row IDs corresponding to the validation set
  predicted_total_points_scaled = as.vector(pred_all),
  actual_total_points_scaled = as.vector(y_val)
) %>% 
  mutate(
    predicted_total_points = predicted_total_points_scaled * sigma + mu,
    actual_total_points = actual_total_points_scaled * sigma + mu
  )


preds_fwd <- preds_fwd %>%
  left_join(unscaled_fwd, by = "row_id") # Use unscaled_fwd

# View the predictions table
glimpse(preds_fwd)

## FWD 4.10 Save model and validation set predictions ----
# Store FWD model and scaling factors 
model_list$fwd <- model
cat("FWD model stored in model_list$fwd.\n")

# Clean up validation predictions (select essential columns)
preds_fwd_clean <- preds_fwd %>%
  select(row_id, GW, player_id, name, position, team,
         actual_total_points, predicted_total_points, value)
cat("Validation predictions stored in preds_fwd_clean.\n")

## FWD 4.11: Plots for Evaluation ----

### FWD 4.11.1 Save training history ----
history_fwd <- history # class() "keras_training_history"
history_df_fwd <- data.frame(history_fwd)

### FWD 4.11.2 Training History Plot ----
plot_history_fwd <- plot(history_fwd)
plot_history_fwd;str(history_df_fwd)

### FWD 4.11.3 Actual vs Predicted Points Plot ----
plot_actual_predicted_fwd <- ggplot(preds_fwd, aes(x = actual_total_points, y = predicted_total_points)) +
  geom_point(alpha = 0.5) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Actual vs. Predicted Total Points (Validation Set)",
       x = "Actual Total Points",
       y = "Predicted Total Points") +
  facet_wrap(~ position) +
  theme_grey() +
  coord_cartesian(xlim = range(preds_fwd$actual_total_points, na.rm = TRUE),
                  ylim = range(preds_fwd$predicted_total_points, na.rm = TRUE))
plot_actual_predicted_fwd                  

### FWD 4.11.4: Residuals vs Predicted ----
preds_fwd_residuals <- preds_fwd %>%
  mutate(residual = actual_total_points - predicted_total_points)

plot_residuals_predicted_fwd <- ggplot(preds_fwd_residuals, aes(x = predicted_total_points, y = residual)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs. Predicted Total Points (Validation Set)",
       x = "Predicted Total Points",
       y = "Residual (Actual - Predicted)") +
  facet_wrap(~ position) +
  theme_grey()  
plot_residuals_predicted_fwd

### FWD 4.11.5: Distribution of Residuals ----
plot_distribution_residuals_fwd <- ggplot(preds_fwd_residuals, aes(x = residual)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "blue", alpha = 0.7) +
  geom_density(color = "red") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Distribution of Residuals (Validation Set)",
       x = "Residual (Actual - Predicted)",
       y = "Density") +
  facet_wrap(~ position) +
  theme_grey()
plot_distribution_residuals_fwd

### FWD 4.11.6 Return list containing loss value and metrics ----
# Evaluate model on validation data using consistent variable names
model_evaluation <- model |> evaluate(
  c(
    list(input_seq = Xnum_val),
    Xcat_hist_va,
    Xcat_cur_va
  ),
  y_val,
  verbose = 1
)
# Print the evaluation results
print(model_evaluation)

# Store validation metrics
validation_metrics$fwd <- model_evaluation
cat("Validation metrics stored in validation_metrics$fwd.\n")

# Merging Datasets ----
glimpse(preds_gk)
glimpse(preds_def)
glimpse(preds_mid)
glimpse(preds_fwd)

# filepath: c:\Users\peram\Documents\test\R Forecast\prognose-modell.ipynb
# Combine the CLEANED validation predictions
validation_results_df <- bind_rows(
  preds_gk_clean,
  preds_def_clean,
  preds_mid_clean,
  preds_fwd_clean
) %>%
  arrange(player_id, GW)

glimpse(validation_results_df)

# Keep the original detailed forecast df as well (optional)
forecastdf_detailed <- bind_rows(preds_gk, preds_def, preds_mid, preds_fwd)
cat("Clean and detailed validation results combined.\n")

## Evaluation Metrics Dataframe ----
# Create metrics comparison dataframe
metrics_df <- data.frame(
  metric = names(validation_metrics$gk),
  GK = unlist(validation_metrics$gk),
  DEF = unlist(validation_metrics$def),
  MID = unlist(validation_metrics$mid),
  FWD = unlist(validation_metrics$fwd)
)

metrics_df
# Save plots from 1.11.n, 2.11.n, 3.11.n, 4.11.n to "C:/Users/peram/Documents/test/R Forecast/Plots from LSTM Forecast notebook"

# GK plots
ggsave("C:/Users/peram/Documents/test/R Forecast/Plots from LSTM Forecast notebook/plot_history_gk.png", plot = plot_history_gk, width = 7, height = 5)
ggsave("C:/Users/peram/Documents/test/R Forecast/Plots from LSTM Forecast notebook/plot_actual_predicted_gk.png", plot = plot_actual_predicted_gk, width = 7, height = 5)
ggsave("C:/Users/peram/Documents/test/R Forecast/Plots from LSTM Forecast notebook/plot_residuals_predicted_gk.png", plot = plot_residuals_predicted_gk, width = 7, height = 5)
ggsave("C:/Users/peram/Documents/test/R Forecast/Plots from LSTM Forecast notebook/plot_distribution_residuals_gk.png", plot = plot_distribution_residuals_gk, width = 7, height = 5)

# DEF plots
ggsave("C:/Users/peram/Documents/test/R Forecast/Plots from LSTM Forecast notebook/plot_history_def.png", plot = plot_history_def, width = 7, height = 5)
ggsave("C:/Users/peram/Documents/test/R Forecast/Plots from LSTM Forecast notebook/plot_actual_predicted_def.png", plot = plot_actual_predicted_def, width = 7, height = 5)
ggsave("C:/Users/peram/Documents/test/R Forecast/Plots from LSTM Forecast notebook/plot_residuals_predicted_def.png", plot = plot_residuals_predicted_def, width = 7, height = 5)
ggsave("C:/Users/peram/Documents/test/R Forecast/Plots from LSTM Forecast notebook/plot_distribution_residuals_def.png", plot = plot_distribution_residuals_def, width = 7, height = 5)

# MID plots
ggsave("C:/Users/peram/Documents/test/R Forecast/Plots from LSTM Forecast notebook/plot_history_mid.png", plot = plot_history_mid, width = 7, height = 5)
ggsave("C:/Users/peram/Documents/test/R Forecast/Plots from LSTM Forecast notebook/plot_actual_predicted_mid.png", plot = plot_actual_predicted_mid, width = 7, height = 5)
ggsave("C:/Users/peram/Documents/test/R Forecast/Plots from LSTM Forecast notebook/plot_residuals_predicted_mid.png", plot = plot_residuals_predicted_mid, width = 7, height = 5)
ggsave("C:/Users/peram/Documents/test/R Forecast/Plots from LSTM Forecast notebook/plot_distribution_residuals_mid.png", plot = plot_distribution_residuals_mid, width = 7, height = 5)

# FWD plots
ggsave("C:/Users/peram/Documents/test/R Forecast/Plots from LSTM Forecast notebook/plot_history_fwd.png", plot = plot_history_fwd, width = 7, height = 5)
ggsave("C:/Users/peram/Documents/test/R Forecast/Plots from LSTM Forecast notebook/plot_actual_predicted_fwd.png", plot = plot_actual_predicted_fwd, width = 7, height = 5)
ggsave("C:/Users/peram/Documents/test/R Forecast/Plots from LSTM Forecast notebook/plot_residuals_predicted_fwd.png", plot = plot_residuals_predicted_fwd, width = 7, height = 5)
ggsave("C:/Users/peram/Documents/test/R Forecast/Plots from LSTM Forecast notebook/plot_distribution_residuals_fwd.png", plot = plot_distribution_residuals_fwd, width = 7, height = 5)


## Export Predicted Validation Set as .CSV ----
# Export CLEAN validation results
write_csv(validation_results_df, "Validation_Predictions_Clean.csv")
cat("Clean validation predictions saved to Validation_Predictions_Clean.csv\n")

# Export DETAILED validation results (original behavior, renamed file)
write_csv(forecastdf_detailed, "Validation_Predictions_Detailed.csv")
cat("Detailed validation predictions saved to Validation_Predictions_Detailed.csv\n")

# Make further predictions ----
# To do this we need future fixtures including:
# - GW (gameweek number)
# - team_id (team identifier matching your tID)
# - opponent_id (opponent identifier matching your oID)
# - is_home (1 for home, 0 for away)

## Helper functions: ----

### Get FPL fixtures ----

# Funksjon som henter fra api
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

future_fixtures <- get_fpl_fixtures()
future_fixtures

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

team_mapping <- get_team_mapping()

if (exists("future_fixtures")) {
  print(paste("Max GW in future_fixtures:", max(future_fixtures$GW, na.rm = TRUE)))
  print("Gameweeks present in future_fixtures:")
  print(table(future_fixtures$GW))
  glimpse(future_fixtures)
} else {
  print("future_fixtures data frame does not exist.")
}

num_previous_full_seasons <- 2 # Set this correctly
# --- End User Input ---

# 1. Determine Starting GWs (Historical and API)
#--------------------------------------------
if (!exists("alternativsammensatt") || !("GW" %in% names(alternativsammensatt))) {
  stop("The 'alternativsammensatt' dataframe is missing or does not contain the 'GW' column.")
}
last_historical_gw <- max(alternativsammensatt$GW, na.rm = TRUE)
current_api_gw <- last_historical_gw - (num_previous_full_seasons * 38)
target_api_gws <- (current_api_gw + 1):(current_api_gw + antall_uker)

cat("Last Historical GW in data:", last_historical_gw, "\n")
cat("Corresponding Current API GW:", current_api_gw, "\n")
cat("Forecasting for API GWs:", paste(target_api_gws, collapse = ", "), "\n")

if (any(target_api_gws > 38)) {
  cat("WARNING: Target API GWs exceed 38. Ensure 'future_fixtures' contains data for the next season if applicable.\n")
}
if (any(target_api_gws < 1)) {
  stop("Calculated target API GWs are less than 1. Check 'num_previous_full_seasons' calculation.")
}

# 2. Get Fixtures for Future API Gameweeks
#--------------------------------------------
if (!exists("future_fixtures") || !all(c("GW", "team_id", "opponent_id", "is_home") %in% names(future_fixtures))) {
  stop("The 'future_fixtures' dataframe is missing or does not have the required columns (GW, team_id, opponent_id, is_home). It should use API GW numbering (1-38).")
}
future_fixtures_filtered <- future_fixtures %>%
  filter(GW %in% target_api_gws) %>%
  mutate(across(c(team_id, opponent_id), as.integer))

if (nrow(future_fixtures_filtered) == 0) {
  stop("No API fixture data found for the target API gameweeks: ", paste(target_api_gws, collapse=", "))
}
cat("Fixture data loaded for target API GWs.\n")

# 3. Prepare Base Input Data (Once per position)
#--------------------------------------------
base_input_list <- list()
all_player_metadata <- list()

for (pos in c("gk", "def", "mid", "fwd")) {
  cat("\n--- Preparing base input for position:", toupper(pos), "---\n")
  if (!pos %in% names(model_list)) { cat("   Skipping: Model not found for", pos, "\n"); next }
  if (!pos %in% names(scaling_factors)) { cat("   Skipping: Scaling factors not found for", pos, "\n"); next }
  current_model <- model_list[[pos]]
  current_mu <- scaling_factors[[pos]]$mu
  current_sigma <- scaling_factors[[pos]]$sigma
  current_numF <- scaling_factors[[pos]]$numF
  if (!exists(pos)) { cat("   Skipping: Scaled data frame '", pos, "' not found.\n"); next }
  if (!exists(paste0("unscaled_", pos))) { cat("   Skipping: Unscaled data frame 'unscaled_", pos, "' not found.\n"); next }
  scaled_data <- get(pos)
  unscaled_data_minimal <- get(paste0("unscaled_", pos)) %>%
    distinct(player_id, name, position, team, value)
  all_player_metadata[[pos]] <- unscaled_data_minimal
  active_players_pos <- scaled_data %>%
    filter(GW == last_historical_gw) %>%
    distinct(player_id, tID) %>%
    mutate(across(c(player_id, tID), as.integer))
  if (nrow(active_players_pos) == 0) {
    cat("   No active players found for", toupper(pos), "in Historical GW", last_historical_gw, ". Skipping.\n")
    next
  }
  base_input_data_pos <- scaled_data %>%
    filter(player_id %in% active_players_pos$player_id) %>%
    group_by(player_id) %>%
    arrange(GW) %>%
    slice_tail(n = vindu) %>%
    filter(n() == vindu) %>%
    ungroup()
  players_with_history <- base_input_data_pos %>% distinct(player_id)
  active_players_pos <- active_players_pos %>% filter(player_id %in% players_with_history$player_id)
  n_forecast_pos <- nrow(active_players_pos)
  if (n_forecast_pos == 0) {
    cat("   No players with sufficient history (\", vindu, \"GWs) found for\", toupper(pos), \". Skipping.\n")
    next
  }
  cat("   Base input data prepared for", n_forecast_pos, "players based on data up to Historical GW", last_historical_gw, ".\n")
  num_list <- base_input_data_pos %>%
    select(player_id, all_of(current_numF)) %>%
    arrange(match(player_id, active_players_pos$player_id)) %>%
    group_by(player_id) %>%
    group_split(.keep = FALSE) %>%
    lapply(as.matrix)
  base_num_array_pos <- array(
    unlist(num_list),
    dim = c(n_forecast_pos, vindu, length(current_numF))
  )
  base_cat_player_id_pos <- matrix(as.integer(active_players_pos$player_id), ncol = 1)
  base_cat_tID_pos <- matrix(as.integer(active_players_pos$tID), ncol = 1)
  base_input_list[[pos]] <- list(
    active_players = active_players_pos,
    num_array = base_num_array_pos,
    cat_player_id = base_cat_player_id_pos,
    cat_tID = base_cat_tID_pos,
    mu = current_mu,
    sigma = current_sigma
  )
}
all_player_metadata_df <- bind_rows(all_player_metadata) %>% distinct(player_id, .keep_all = TRUE)

# 4. Forecast Loop (Predicting for each fixture in target GWs)
#--------------------------------------------
all_fixture_forecasts_list <- list() # Store predictions PER FIXTURE

for (gw_api_to_predict in target_api_gws) {
  cat("\n--- Forecasting for API GW:", gw_api_to_predict, "---\n")
  future_fixtures_gw <- future_fixtures_filtered %>% filter(GW == gw_api_to_predict)
  if (nrow(future_fixtures_gw) == 0) {
    cat("   WARNING: No fixture data found for API GW", gw_api_to_predict, ". Skipping this GW.\n")
    next
  }
  
  forecasts_this_gw_list <- list()
  
  for (pos in names(base_input_list)) {
    cat("      Forecasting position:", toupper(pos), "\n")
    base_inputs <- base_input_list[[pos]]
    current_model <- model_list[[pos]]
    
    # Prepare inputs for *each fixture* this GW for active players in this position
    fixture_inputs_gw_pos <- base_inputs$active_players %>%
      inner_join(future_fixtures_gw, by = c("tID" = "team_id"), relationship = "many-to-many") %>% # Join players to ALL their fixtures this GW
      select(player_id, tID, opponent_id = opponent_id, is_home = is_home) %>%
      arrange(player_id) # Arrange by player_id to group fixtures
    
    if (nrow(fixture_inputs_gw_pos) == 0) {
      cat("         No players with fixtures found for this position in GW", gw_api_to_predict, "\n")
      next
    }
    
    # Identify which players have DGWs *within this position's active players*
    player_fixture_counts <- fixture_inputs_gw_pos %>% count(player_id)
    dgw_players_pos <- player_fixture_counts %>% filter(n > 1) %>% pull(player_id)
    sgw_players_pos <- player_fixture_counts %>% filter(n == 1) %>% pull(player_id)
    
    cat("         Found", length(dgw_players_pos), "DGW players and", length(sgw_players_pos), "SGW players.\n")
    
    # Prepare arrays - need to potentially duplicate base arrays for DGW players
    # Find indices of players in the original base arrays
    player_indices <- match(fixture_inputs_gw_pos$player_id, base_inputs$active_players$player_id)
    
    # Replicate base arrays according to the fixture list
    num_array_predict <- base_inputs$num_array[player_indices, , , drop = FALSE]
    cat_player_id_predict <- base_inputs$cat_player_id[player_indices, , drop = FALSE]
    cat_tID_predict <- base_inputs$cat_tID[player_indices, , drop = FALSE]
    
    # Prepare dynamic categorical inputs (already ordered by player_id)
    cat_oID_predict <- matrix(as.integer(fixture_inputs_gw_pos$opponent_id), ncol = 1)
    cat_hID_predict <- matrix(as.integer(fixture_inputs_gw_pos$is_home), ncol = 1)
    
    # Dimension check
    stopifnot(
      nrow(num_array_predict) == nrow(cat_player_id_predict),
      nrow(num_array_predict) == nrow(cat_tID_predict),
      nrow(num_array_predict) == nrow(cat_oID_predict),
      nrow(num_array_predict) == nrow(cat_hID_predict),
      nrow(num_array_predict) == nrow(fixture_inputs_gw_pos) # Ensure all dimensions match the number of fixtures to predict
    )
    
    # Predict for ALL fixtures at once
    cat("         Running prediction for", nrow(num_array_predict) ,"fixtures...\n")
    pred_future_scaled <- current_model %>% predict(
      list(
        input_seq = num_array_predict,
        input_player_id = cat_player_id_predict,
        input_tID = cat_tID_predict,
        input_oID = cat_oID_predict,
        input_hID = cat_hID_predict
      )
    )
    
    # Store raw scaled predictions PER FIXTURE
    future_preds_per_fixture <- tibble(
      player_id = fixture_inputs_gw_pos$player_id,
      GW = gw_api_to_predict, # API GW number
      position = pos,
      opponent_id = fixture_inputs_gw_pos$opponent_id,
      is_home = fixture_inputs_gw_pos$is_home,
      predicted_points_scaled_fixture = as.vector(pred_future_scaled)
    )
    
    forecasts_this_gw_list[[pos]] <- future_preds_per_fixture
    cat("         Raw fixture forecasts stored for", nrow(future_preds_per_fixture), "fixtures.\n")
  }
  # Add all fixture forecasts for this GW to the main list
  all_fixture_forecasts_list[[as.character(gw_api_to_predict)]] <- bind_rows(forecasts_this_gw_list)
}

# 5. Aggregate Predictions per GW, Unscale, and Finalize
#--------------------------------------------
future_forecast_df_raw_fixtures <- bind_rows(all_fixture_forecasts_list)

if (nrow(future_forecast_df_raw_fixtures) == 0) {
  stop("No future fixture forecasts were generated.")
}

cat("\n--- Aggregating, Unscaling and Finalizing Forecasts ---\n")

# Aggregate scaled predictions by summing per player per GW
future_forecast_df_aggregated_scaled <- future_forecast_df_raw_fixtures %>%
  group_by(player_id, GW, position) %>%
  summarise(
    predicted_total_points_scaled = sum(predicted_points_scaled_fixture, na.rm = TRUE),
    num_fixtures = n(), # Keep track of how many fixtures were summed
    # Keep first opponent/home status for display purposes (or create a combined string)
    first_opponent_id = first(opponent_id),
    first_is_home = first(is_home),
    .groups = 'drop'
  )

# Unscale the *aggregated* predictions
future_forecast_df_unscaled <- future_forecast_df_aggregated_scaled %>%
  mutate(
    mu = map_dbl(position, ~ scaling_factors[[.x]]$mu),
    sigma = map_dbl(position, ~ scaling_factors[[.x]]$sigma),
    predicted_total_points = predicted_total_points_scaled * sigma + mu,
    predicted_total_points = round(predicted_total_points, 2)
  ) %>%
  select(-predicted_total_points_scaled, -mu, -sigma)

# 6. Translate IDs to Strings
#--------------------------------------------
if (!exists("team_mapping") || !all(c("team_id", "team_name") %in% names(team_mapping))) {
  stop("The 'team_mapping' dataframe is missing or does not have 'team_id' and 'team_name' columns.")
}
team_mapping <- team_mapping %>% mutate(team_id = as.integer(team_id))

future_forecast_df_final <- future_forecast_df_unscaled %>%
  left_join(all_player_metadata_df, by = "player_id", suffix = c("", ".meta")) %>%
  # Join opponent team name based on the *first* opponent ID
  left_join(team_mapping %>% select(first_opponent_id = team_id, opponent_team_name = team_name),
            by = "first_opponent_id") %>%
  # Handle NO_FIXTURE case (if opponent ID was 0)
  mutate(opponent_display = ifelse(first_opponent_id == 0, "NO_FIXTURE",
                                   ifelse(num_fixtures > 1, paste0(opponent_team_name, " (DGW)"), opponent_team_name)),
         is_home_display = ifelse(first_opponent_id == 0, NA_character_,
                                  ifelse(num_fixtures > 1, paste0(first_is_home, " (DGW)"), as.character(first_is_home)))
  ) %>%
  # Select and arrange final columns
  select(
    GW, # API GW
    player_id,
    name,
    position = position,
    team = team,
    value = value,
    opponent_display, # Shows first opponent, notes DGW
    is_home_display,  # Shows first fixture home/away, notes DGW
    num_fixtures,     # Number of fixtures in this GW
    predicted_points = predicted_total_points
  ) %>%
  arrange(GW, desc(predicted_points))

cat("Final forecast data frame created with team names and DGW handling.\n")
glimpse(future_forecast_df_final)

# 7. Export the Final Forecasts
#--------------------------------------------
future_filename <- paste0("Future_Forecasts_API_GW", min(target_api_gws), "_to_GW", max(target_api_gws), "_Final_DGW_Summed.csv")
write_csv(future_forecast_df_final, future_filename)
cat("Final future forecasts saved to", future_filename, "\n")
