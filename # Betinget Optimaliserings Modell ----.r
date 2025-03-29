# Blandet heltalls modell  ----
# Basert på Kristiansen et al.
rm(list = ls(all = TRUE))
# Pakker
library(tidyverse)
library(ompr); library(ompr.roi); library(ROI.plugin.highs)
citation("keras3");citation("tensorflow");citation("ompr")

# Forsøker på en sesong først for å se om det fungerer, Den nyeste sesongen er minst
allesesonger <- read_csv("Differensiert gw alle tre sesonger(22-24), heltall.csv")
allesesonger |> colnames()
length(unique(allesesonger$name)) == length(unique(allesesonger$player_id))

# En hel sesong tar veldig lang tid så derfor prøver jeg først med 3 uker

treuker <- allesesonger |> filter(GW <= 3)
view(treuker)

#Bruker data for å forenkle senere endringer i kode

data <- treuker

# Set Table 4.1 Kristiansen et al. 2018

T_setofgameweeks <- unique(data$GW) # Sett med gameweeks
P_setofplayers <- unique(data$player_id) # Sett med spillere
C_setofteams <- unique(data$team) # Sett med lag
L_substitution <- c(1:3) # Ubytter rekkefølge, er bare 3 innbyttere 

# Subset  Table 4.2 Kristiansen et al. 2018
Pdef <- data |> filter(position == "DEF") # Sett Forsvarsspillere
Pmid <- data |> filter(position == "MID") # Sett Midtbanespillere
Pfwd <- data |> filter(position == "FWD") # Sett Angrepsspillere
Pgk <- data |> filter(position == "GK") # Sett Keepere
P_c <- split(data$player_id, data$team)  # Sett med spillere etter lag c

medianavgameweeks <- median(T_setofgameweeks) # Midt i sesongen
T_FH <- T_setofgameweeks[T_setofgameweeks <= medianavgameweeks] # Første halvdel av datasettet
T_SH <- T_setofgameweeks[T_setofgameweeks > medianavgameweeks] # Andre halvdel av datasettet

# Index Table 4.3 Kristiansen et al. 2018

t <- T_setofgameweeks# Gameweek
p <- P_setofplayers # Spillere
l <- L_substitution # Ubytter rekkefølge

# Parametre Table 4.4 Kristiansen et al. 2018
R <- 4              # Straff for hver overgang utover gratis overganger
MK <- 2              # Goalkeepers required in squad
MD <- 5               # Defenders required in squad
MM <- 5              # Midfielders required in squad
MF <- 3               # Forwards required in squad
MC <- 3               # Max players allowed from the same team
E <- 11               # Players required in starting line-up
EK <- 1               # Goalkeepers required in starting line-up
ED <- 3               # Min defenders required in starting line-up
EM <- 3               # Min midfielders required
EF <- 1               # Min forwards required in starting line-up
BS <- 1000.0           # Starting budget (adjust if mid-season)
phi <- (MK+MD+MM+MF) - (E+EK) # Number of substitutes on bench
phi_K <- MK - EK        # Number of GK substitutes
Q_bar <- 4            # Max number of free transfers to accumulate
Q_under_bar <- 1                # Free transfers given per gameweek 
 
# Konstant parametre
epsilon <- 0.01    #  eps << 1
kappa <- setNames(10^-(2:(length(l) + 1)), l) # Weights for subs priorities

# Antall spillere og uker
n_players <- length(P_setofplayers)
n_gameweeks <- length(T_setofgameweeks)

beta <- n_players     # Tilstrekkelig høy konstant??? 
alpha_bar <- n_players # Tilstrekkelig høy konstant???

# Variabler Tabell 4.5 Kristiansen et al. 2018
# Initialiser modellen
model <- MIPModel() %>%
  
  # ---- Binære variabler (Tabell 4.5) ----
  # Squad selection (x_pt)
  add_variable(x[p, t], p = 1:n_players, t = T_setofgameweeks, type = "binary") %>%
  
  # Free Hit squad (x_pt^freehit)
  add_variable(x_freehit[p, t], p = 1:n_players, t = T_setofgameweeks, type = "binary") %>%
  
  # Starting line-up (y_pt)
  add_variable(y[p, t], p = 1:n_players, t = T_setofgameweeks, type = "binary") %>%
  
  # Captain (f_pt)
  add_variable(f[p, t], p = 1:n_players, t = T_setofgameweeks, type = "binary") %>%
  
  # Vice-captain (h_pt)
  add_variable(h[p, t], p = 1:n_players, t = T_setofgameweeks, type = "binary") %>%
  
  # Substitutionsprioritet (g_ptl)
  add_variable(g[p, t, l], p = 1:n_players, t = T_setofgameweeks, l = L_substitution, type = "binary") %>%
  
  # Transfers out (u_pt)
  add_variable(u[p, t], p = 1:n_players, t = T_setofgameweeks, type = "binary") %>%
  
  # Transfers in (e_pt)
  add_variable(e[p, t], p = 1:n_players, t = T_setofgameweeks, type = "binary") %>%
  
  # Wildcard (w_t)
  add_variable(w[t], t = T_setofgameweeks, type = "binary") %>%
  
  # Triple Captain (c_pt)
  add_variable(c[p, t], p = 1:n_players, t = T_setofgameweeks, type = "binary") %>%
  
  # Bench Boost (b_t)
  add_variable(b[t], t = T_setofgameweeks, type = "binary") %>%
  
  # Free Hit (r_t)
  add_variable(r[t], t = T_setofgameweeks, type = "binary") %>%
  
  # ---- Kontinuerlige/heltallsvariabler ----
  # Gjenværende budsjett (v_t)
  add_variable(v[t], t = T_setofgameweeks, type = "continuous", lb = 0) %>%
  
  # Gratis overganger (q_t)
  add_variable(q[t], t = T_setofgameweeks, type = "integer", lb = 0, ub = Q_bar) %>%
  
  # Straffeoverganger (alpha_t)
  add_variable(alpha[t], t = T_setofgameweeks, type = "integer", lb = 0)

# Legg til posisjonsindekser for enklere constraints
goalkeepers <- which(data$position == "GK")
defenders <- which(data$position == "DEF")
midfielders <- which(data$position == "MID")
forwards <- which(data$position == "FWD")


### Gem 2.5

# --- 0. Load Libraries ---
# Make sure these are installed: install.packages(c("dplyr", "tidyr", "ompr", "ompr.roi", "ROI.plugin.glpk"))
library(dplyr)
library(tidyr)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk) # Using GLPK solver (free)

# --- Load Your Data ---
# IMPORTANT: Replace this with code to load YOUR actual data frame
# It MUST have columns: player_id, GW, position, team, xP, value
# Example structure:
# fpl_data_raw <- read.csv("your_fpl_data_with_lstm_xp.csv")
# For demonstration, we create a sample:
set.seed(42)
example_players <- paste("Player", 1:50)
example_teams <- paste("Team", LETTERS[1:10])
example_positions <- c("GKP", "DEF", "MID", "FWD")

fpl_data_raw <- expand.grid(player_name = example_players, GW = 1:5) %>%
  group_by(player_name) %>%
  mutate(
    player_id = cur_group_id(),
    position = sample(example_positions, 1, prob = c(0.1, 0.4, 0.4, 0.1)),
    team = sample(example_teams, 1)
  ) %>%
  ungroup() %>%
  mutate(
    xP = pmax(0, rnorm(n(), mean = 3, sd = 2)), # YOUR LSTM FORECASTS GO HERE
    value = round(runif(n(), 4.0, 13.0) * 2) / 2
  ) %>%
  select(player_id, player_name, GW, position, team, xP, value)

print("Sample of your loaded data structure:")
head(fpl_data_raw)

# REAl DATA

testdata <- read_csv("Differensiert gw alle tre sesonger(22-24), heltall.csv")

testdata <- testdata |> select(player_id,name, GW, position, team, xP, value) 

# --- Define Planning Horizon ---
start_gw <- 1
end_gw <- 2 # How many weeks ahead to plan?
planning_gameweeks <- start_gw:end_gw

# Filter data for the planning horizon
fpl_data <- testdata %>%
  filter(GW %in% planning_gameweeks)

if(nrow(fpl_data) == 0) stop("No data for planning gameweeks.")

print(paste("Planning horizon GWs:", paste(planning_gameweeks, collapse = ", ")))

# --- 1. Define Sets (Table 4.1) ---
# These are the basic lists of items we work with.

# T: Set of Gameweeks in our plan (derived from your GW column)
T_set <- unique(fpl_data$GW)
print(paste("Set T (Gameweeks):", paste(sort(T_set), collapse=", ")))

# P: Set of unique Players considered (derived from your player_id column)
P_set <- unique(fpl_data$player_id)
n_players <- length(P_set)
print(paste("Set P (Players): Count =", n_players))

# C: Set of unique Teams involved (derived from your team column)
C_set <- unique(fpl_data$team)
print(paste("Set C (Teams):", paste(sort(C_set), collapse=", ")))

# L: Set of Substitution Priorities (defined by FPL rules)
L_set <- 1:3 # Bench order 1, 2, 3
print(paste("Set L (Priorities):", paste(L_set, collapse=", ")))

# --- 2. Define Subsets (Table 4.2) ---
# These are specific groups *within* the main sets.

# Player info lookup (useful for defining subsets and constraints)
player_lookup <- fpl_data %>%
  group_by(player_id) %>%
  summarise(
    player_name = first(name),
    position = first(position), # Assumes position is constant
    team = first(team),         # Assumes team is constant in planning horizon
    .groups = 'drop'
  )

# P^K, P^D, P^M, P^F: Subsets of Players by Position (using your position column)
P_K <- player_lookup$player_id[player_lookup$position == "GK"]
P_D <- player_lookup$player_id[player_lookup$position == "DEF"]
P_M <- player_lookup$player_id[player_lookup$position == "MID"]
P_F <- player_lookup$player_id[player_lookup$position == "FWD"]
print(paste("Subset P^K (Goalkeepers):", length(P_K)))
print(paste("Subset P^D (Defenders):", length(P_D)))
print(paste("Subset P^M (Midfielders):", length(P_M)))
print(paste("Subset P^F (Forwards):", length(P_F)))

# P_c: Subset of players in team c. We don't pre-calculate this.
# Instead, we use the 'player_lookup' table inside constraints later.

# T_FH, T_SH: Gameweek halves (relative to the full 38-GW season)
total_season_gws <- 38
mid_season_gw <- floor(total_season_gws / 2)
T_FH <- T_set[T_set <= mid_season_gw] # GWs in first half *that are in our plan*
T_SH <- T_set[T_set > mid_season_gw]  # GWs in second half *that are in our plan*
print(paste("Subset T_FH (First Half GWs in Plan):", paste(T_FH, collapse=", ")))
print(paste("Subset T_SH (Second Half GWs in Plan):", paste(T_SH, collapse=", ")))

# --- 3. Define Parameters (Table 4.4) ---
# These are the known inputs/constants for the model.

# P_pt(wt) -> ExpP_pt (Expected Points parameter)
# This comes DIRECTLY from your 'xP' column.
exp_points_df <- fpl_data %>%
  select(p = player_id, t = GW, ExpP = xP) %>%
  group_by(p, t) %>%
  summarise(ExpP = mean(ExpP, na.rm = TRUE), .groups = 'drop') %>%
  tidyr::complete(p = P_set, t = T_set, fill = list(ExpP = 0)) # Fill missing with 0 points
print("Parameter ExpP_pt (Expected Points): First 6 rows")
head(exp_points_df)

# CB_pt (Buy Price / "Value") and CS_pt (Sell Price) parameters
# Both derived from your 'value' column (deterministic simplification).
prices_df <- fpl_data %>%
  select(p = player_id, t = GW, CB = value) %>%
  mutate(CS = CB) |># Simplification: Sell Price = Buy Price
  group_by(p, t) %>%
  summarise(
    CB = mean(CB, na.rm = TRUE),
    CS = mean(CS, na.rm = TRUE),
    .groups = 'drop'
   ) %>%
  tidyr::complete(p = P_set, t = T_set, fill = list(CB = 1000, CS = 0)) # Fill missing
print("Parameters CB_pt (Buy Price) & CS_pt (Sell Price): First 6 rows")
head(prices_df)

# --- FPL Rule Constants & Model Constants ---
# Stored in a list for easy access
params <- list(
  # From FPL Rules
  R = 4,                # Points deduction per penalized transfer
  MK = 2,               # Goalkeepers required in squad
  MD = 5,               # Defenders required in squad
  MM = 5,               # Midfielders required in squad
  MF = 3,               # Forwards required in squad
  MC = 3,               # Max players allowed from the same team
  E = 11,               # Players required in starting line-up
  EK = 1,               # Goalkeepers required in starting line-up
  ED = 3,               # Min defenders required in starting line-up
  EM = 2,               # Min midfielders required (check rules, might be >=3)
  EF = 1,               # Min forwards required in starting line-up
  BS = 100.0,           # Starting budget (adjust if mid-season)
  phi = (2+5+5+3) - 11, # Number of substitutes on bench
  phi_K = 2 - 1,        # Number of GK substitutes
  Q_bar = 2,            # Max number of free transfers to accumulate
  Q = 1,                # Free transfers given per gameweek
  # For Model Formulation
  epsilon = 0.01,       # Small value for vice-captain boost (objective)
  kappa = setNames(10^-(2:(length(L_set) + 1)), L_set), # Weights for subs priorities
  beta = n_players,     # Sufficiently high 'Big M' for constraints 4.30, 4.31
  alpha_bar = n_players # Sufficiently high 'Big M' for constraint 4.42
)
print("Game Rule & Model Parameters (list 'params'):")
print(params)

# --- Helper functions to easily get parameter values later ---
# These functions look up values from our prepared data frames.
get_param_value <- function(df, player, gameweek, col_name, default_value) {
  val <- df[[col_name]][df$p == player & df$t == gameweek]
  if (length(val) == 0 || is.na(val[1])) return(default_value)
  return(val[1])
}
get_ExpP <- function(player, gameweek) get_param_value(exp_points_df, player, gameweek, "ExpP", 0)
get_CB <- function(player, gameweek) get_param_value(prices_df, player, gameweek, "CB", params$BS * 10)
get_CS <- function(player, gameweek) get_param_value(prices_df, player, gameweek, "CS", 0)
get_Team <- function(player) player_lookup$team[player_lookup$player_id == player][1]
get_players_in_team <- function(team_id) player_lookup$player_id[player_lookup$team == team_id]

print("Helper functions created (get_ExpP, get_CB, get_CS, etc.)")

# --- 4. Define Variables (Table 4.5) ---
# These are the decisions the optimization model needs to make.
# We use ompr's add_variable() function.

model <- MIPModel() %>%

  # x[p,t]: 1 if player p is in the NORMAL squad in GW t, 0 otherwise
  add_variable(x[p, t], p = P_set, t = T_set, type = "binary") %>%

  # x_fh[p,t]: 1 if player p is in the squad during a FREE HIT in GW t, 0 otherwise
  add_variable(x_fh[p, t], p = P_set, t = T_set, type = "binary") %>%

  # y[p,t]: 1 if player p is in the STARTING LINEUP in GW t, 0 otherwise
  add_variable(y[p, t], p = P_set, t = T_set, type = "binary") %>%

  # f[p,t]: 1 if player p is CAPTAIN in GW t, 0 otherwise
  add_variable(f[p, t], p = P_set, t = T_set, type = "binary") %>%

  # h[p,t]: 1 if player p is VICE-CAPTAIN in GW t, 0 otherwise
  add_variable(h[p, t], p = P_set, t = T_set, type = "binary") %>%

  # g[p,t,l]: 1 if player p has SUBSTITUTION priority l on the bench in GW t, 0 otherwise
  add_variable(g[p, t, l], p = P_set, t = T_set, l = L_set, type = "binary") %>%

  # u[p,t]: 1 if player p is TRANSFERRED OUT before GW t, 0 otherwise
  # Note: Transfers happen *between* gameweeks. u[p,t] affects the squad *for* GW t.
  # Defined only for t >= start_gw + 1, as no transfers before the first GW.
  add_variable(u[p, t], p = P_set, t = T_set[T_set >= start_gw + 1], type = "binary") %>%

  # e[p,t]: 1 if player p is TRANSFERRED IN before GW t, 0 otherwise
  add_variable(e[p, t], p = P_set, t = T_set[T_set >= start_gw + 1], type = "binary") %>%

  # w[t]: 1 if WILDCARD chip is used in GW t, 0 otherwise
  add_variable(w[t], t = T_set, type = "binary") %>%

  # c[p,t]: 1 if TRIPLE CAPTAIN chip is used on player p in GW t, 0 otherwise
  add_variable(c[p, t], p = P_set, t = T_set, type = "binary") %>%

  # b[t]: 1 if BENCH BOOST chip is used in GW t, 0 otherwise
  add_variable(b[t], t = T_set, type = "binary") %>%

  # r[t]: 1 if FREE HIT chip is used in GW t, 0 otherwise
  add_variable(r[t], t = T_set, type = "binary") %>%

  # lambda[p,t]: Auxiliary binary variable for linearizing the starting lineup constraint (Eq 4.24-4.26)
  add_variable(lambda[p, t], p = P_set, t = T_set, type = "binary") %>%

  # v[t]: Remaining budget (£m) AT THE END of GW t
  add_variable(v[t], t = T_set, type = "continuous", lb = 0) %>%

  # q[t]: Number of free transfers available AT THE START of GW t (for transfers before GW t+1)
  # Defined only for t >= start_gw + 1
  add_variable(q[t], t = T_set[T_set >= start_gw + 1], type = "integer", lb = 0, ub = params$Q_bar) %>%

  # alpha[t]: Number of penalized transfers made IN GW t (i.e., transfers before GW t+1 costing points)
  # Defined only for t >= start_gw + 1
  add_variable(alpha[t], t = T_set[T_set >= start_gw + 1], type = "integer", lb = 0)

print("Variables defined within the 'ompr' model.")
# You can inspect the model structure (optional)
print(model)

# --- Step 5: Define the Objective Function ---
# This implements the deterministic objective function shown in Chapter 5, Section 5.1
# using our forecasted expected points (p-hat_pt), which come from get_ExpP(p,t) -> your xP column.

# Assuming 'model' is the ompr model object created in the previous step
# (containing add_variable calls)

model <- model %>% # Continue adding to the existing model object
  set_objective(
    # Sum over all players (p in P_set) and all gameweeks (t in T_set)
    sum_expr(
      # Expected points from player p starting in gameweek t
      get_ExpP(p, t) * y[p, t] +

      # ADDED expected points if player p is captain (1 * ExpP)
      get_ExpP(p, t) * f[p, t] +

      # ADDED expected points if player p is vice-captain (epsilon * ExpP - very small boost)
      params$epsilon * get_ExpP(p, t) * h[p, t] +

      # ADDED expected points from substitutions (kappa_l * ExpP for sub priority l)
      sum_expr(params$kappa[l] * get_ExpP(p, t) * g[p, t, l], l = L_set) +

      # ADDED expected points if Triple Captain chip is used (2 * ExpP)
      2 * get_ExpP(p, t) * c[p, t],

      # Specify indices for the outer summation
      p = P_set, t = T_set
    )
    # SUBTRACT total penalty points from penalized transfers
    - sum_expr(params$R * alpha[t], t = T_set[T_set >= start_gw + 1]),

    # Specify the goal: maximize this total value
    sense = "max"
  )

print("Objective function defined in the 'ompr' model.")
# You can inspect the objective function part of the model (optional)
objective_function(model)

