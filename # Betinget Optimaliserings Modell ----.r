# Blandet heltalls modell  ----
# Basert på Kristiansen et al.

# Pakker
library(tidyverse)
library(ompr); library(ompr.roi); library(ROI.plugin.glpk)
citation("keras3");citation("tensorflow");citation("ompr")

# Data
sesong22 <- read_csv("Sesong 22 til 23.csv")

clipr::write_clip(print(glimpse(sesong22)))

# Modell

fplmodell <- MILPModel() 

----
# Load required packages
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)
library(dplyr)

### 1. Prepare Data ------------------------------------------------------------
# Assuming 'sesong22' is your dataframe with columns: name, position, team, GW, xP, value
fpl_data <- sesong22 %>%
  mutate(
    player_id = as.integer(factor(name)),  # Create unique player IDs
    position = factor(position),
    team = factor(team)
  ) %>%
  select(player_id, name, position, team, GW, expected_points = xP, value)

# Split players by position
goalkeepers <- fpl_data %>% filter(position == "GK") %>% distinct(player_id) %>% pull(player_id)
defenders <- fpl_data %>% filter(position == "DEF") %>% distinct(player_id) %>% pull(player_id)
midfielders <- fpl_data %>% filter(position == "MID") %>% distinct(player_id) %>% pull(player_id)
forwards <- fpl_data %>% filter(position == "FWD") %>% distinct(player_id) %>% pull(player_id)

# Gameweeks and players
gameweeks <- unique(fpl_data$GW)
players <- unique(fpl_data$player_id)
n_players <- length(players)
n_gameweeks <- length(gameweeks)

### 2. Define Parameters -------------------------------------------------------
params <- list(
  M.K = 2,    # Max goalkeepers in squad
  M.D = 5,    # Max defenders
  M.M = 5,    # Max midfielders
  M.F = 3,    # Max forwards
  E = 11,     # Starting 11
  E.K = 1,    # Starting GK
  E.D = 3,    # Min starting DEF
  E.M = 2,    # Min starting MID
  E.F = 1,    # Min starting FWD
  R = -4,     # Points per penalized transfer
  B.S = 1000, # Starting budget (in 0.1M units)
  Q = 1,      # Free transfers per GW
  epsilon = 0.1,  # Vice-captain weight
  kappa = c(0.05, 0.03, 0.01)  # Sub priorities (1st, 2nd, 3rd)
)

### 3. Build the Optimization Model --------------------------------------------
model <- MIPModel() %>%
  
  # Decision Variables
  add_variable(x[p, t], p = players, t = gameweeks, type = "binary") %>%  # Squad selection
  add_variable(y[p, t], p = players, t = gameweeks, type = "binary") %>%  # Starting XI
  add_variable(f[p, t], p = players, t = gameweeks, type = "binary") %>%  # Captain
  add_variable(h[p, t], p = players, t = gameweeks, type = "binary") %>%  # Vice-captain
  add_variable(g[p, t, l], p = players, t = gameweeks, l = 1:3, type = "binary") %>%  # Subs
  add_variable(c[p, t], p = players, t = gameweeks, type = "binary") %>%  # Triple Captain
  add_variable(e[p, t], p = players, t = gameweeks, type = "binary") %>%  # Transfer in
  add_variable(u[p, t], p = players, t = gameweeks, type = "binary") %>%  # Transfer out
  add_variable(alpha[t], t = gameweeks, type = "integer", lb = 0) %>%     # Penalized transfers
  add_variable(v[t], t = gameweeks, type = "continuous", lb = 0) %>%      # Budget
  add_variable(q[t], t = gameweeks, type = "integer", lb = params$Q, ub = 2) %>% # Free transfers
  
  # Objective Function (Maximize expected points)
  set_objective(
    sum_expr(
      (y[p, t] + f[p, t] + params$epsilon * h[p, t] + 
       sum_expr(params$kappa[l] * g[p, t, l], l = 1:3) + 
       2 * c[p, t]) * 
      filter(fpl_data, player_id == p, GW == t)$expected_points,
      p = players, t = gameweeks
    ) + 
    params$R * sum_expr(alpha[t], t = gameweeks),
    sense = "max"
  ) %>%
  
  ### Constraints ###
  
  # Squad Composition
  add_constraint(sum_expr(x[p, t], p = goalkeepers) == params$M.K, t = gameweeks) %>%
  add_constraint(sum_expr(x[p, t], p = defenders) == params$M.D, t = gameweeks) %>%
  add_constraint(sum_expr(x[p, t], p = midfielders) == params$M.M, t = gameweeks) %>%
  add_constraint(sum_expr(x[p, t], p = forwards) == params$M.F, t = gameweeks) %>%
  
  # Starting XI Requirements
  add_constraint(sum_expr(y[p, t], p = players) == params$E, t = gameweeks) %>%
  add_constraint(sum_expr(y[p, t], p = goalkeepers) == params$E.K, t = gameweeks) %>%
  add_constraint(sum_expr(y[p, t], p = defenders) >= params$E.D, t = gameweeks) %>%
  add_constraint(sum_expr(y[p, t], p = midfielders) >= params$E.M, t = gameweeks) %>%
  add_constraint(sum_expr(y[p, t], p = forwards) >= params$E.F, t = gameweeks) %>%
  
  # Captain and Vice-captain Logic
  add_constraint(sum_expr(f[p, t], p = players) <= 1, t = gameweeks) %>%
  add_constraint(sum_expr(h[p, t], p = players) <= 1, t = gameweeks) %>%
  add_constraint(f[p, t] <= y[p, t], p = players, t = gameweeks) %>%
  add_constraint(h[p, t] <= y[p, t], p = players, t = gameweeks) %>%
  
  # Budget Constraints
  add_constraint(
    params$B.S - sum_expr(filter(fpl_data, GW == 1)$value[p] * x[p, 1], p = players) == v[1]
  ) %>%
  add_constraint(
    v[t-1] + sum_expr(filter(fpl_data, GW == t)$value[p] * u[p, t], p = players) - 
    sum_expr(filter(fpl_data, GW == t)$value[p] * e[p, t], p = players) == v[t],
    t = gameweeks[-1]  # All gameweeks except first
  )

### 4. Solve the Model --------------------------------------------------------
solution <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))

### 5. Extract Results --------------------------------------------------------
get_optimal_team <- function(solution, gw) {
  solution %>%
    get_solution(x[p, gw]) %>%
    filter(value > 0.9) %>%
    left_join(fpl_data %>% filter(GW == gw), by = "p" = "player_id") %>%
    select(name, position, team, expected_points)
}

gw1_team <- get_optimal_team(solution, 1)
print(gw1_team)

----

## Begrensninger ----

### Set
#T- Setofgameweeks.
#P- Setofplayers.
#C- Setofteams.
#L- Substitutionpriorities,where1isfirstpriority.

### Subset
# P^D- Subset of defenders, PD⇢P
# P^M- Subset of midfielders, PM⇢P
# P^F- Subset of forwards, PF ⇢P
# P^K- Subset of goalkeepers, PK⇢P
# P_c- Subset of footballplayers ina team c, Pc ⇢P, c2C
# T_FH- Subset of thegameweeksinthefirsthalfoftheseason, TFH⇢T
# T_SH- Subset of thegameweeksinthesecondhalfoftheseason. TSH⇢T

### Index

# p - Players
# t - Gameweeks
# l - Substitutionpriorities

### Parameters

params <- list(
  M.K = 2,    # Max goalkeepers in squad
  M.D = 5,    # Max defenders
  M.M = 5,    # Max midfielders
  M.F = 3,    # Max forwards
  E = 11,     # Starting 11
  E.K = 1,    # Starting GK
  E.D = 3,    # Min starting DEF
  E.M = 2,    # Min starting MID
  E.F = 1,    # Min starting FWD
  R = -4,     # Points per penalized transfer
  B.S = 1000, # Starting budget (in 0.1M units)
  Q = 1       # Free transfers per GW
)
#Gamechips



