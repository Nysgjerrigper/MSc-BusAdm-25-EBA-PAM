# Enkel Optimalisering
rm(list = ls(all = TRUE))
library(tidyverse)
library(lpSolve)
# Data
# Forsøker på en sesong først for å se om det fungerer, Den nyeste sesongen 23/24
allesesonger <- read_csv("https://raw.githubusercontent.com/Nysgjerrigper/test/refs/heads/main/Differensiert%20gw%20alle%20tre%20sesonger(22-24)%2C%20heltall.csv", show_col_types = FALSE)

# En hel sesong tar veldig lang tid så derfor prøver jeg først med 3 uker
df <- allesesonger |> filter(GW <= 3)


# --- 0. Load Libraries ---
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk) # Or ROI.plugin.symphony
library(dplyr)
library(readr)
library(tidyr)

# --- 1. User Inputs & Constants ---

# --- Core Settings ---
CURRENT_GW <- 1                # Set the Gameweek (Ω) you want to optimize FOR.
PLANNING_HORIZON <- 1        # How many GWs to look ahead (including CURRENT_GW). Λ = CURRENT_GW + PLANNING_HORIZON - 1
DATA_URL <- "https://raw.githubusercontent.com/Nysgjerrigper/test/refs/heads/main/Differensiert%20gw%20alle%20tre%20sesonger(22-24)%2C%20heltall.csv"

# --- FPL Rules ---
TOTAL_BUDGET <- 1000           # Budget (e.g., 100.0m = 1000)
SQUAD_SIZE <- 15
MAX_PER_TEAM <- 3
ROLE_REQUIREMENTS <- list(GKP = 2, DEF = 5, MID = 5, FWD = 3) # r_d
TRANSFER_PENALTY <- 4          # Points penalty for each transfer over the free limit

# --- Settings for Ω > 1 (Ignored if CURRENT_GW = 1) ---
# *** MANUALLY UPDATE THESE IF CURRENT_GW > 1 ***
PREVIOUS_SQUAD_PLAYERS <- c() # Vector of player_name strings from GW Ω-1 squad
PREVIOUS_BUDGET <- TOTAL_BUDGET # Remaining budget *after* GW Ω-1 finalized (e.g., 1003 if 0.3m in the bank)
FREE_TRANSFERS_GW_OMEGA <- 1 # How many free transfers available *entering* GW Ω (usually 1 or 2)

# --- Settings for Future GWs in Horizon (t > Ω) ---
DEFAULT_FUTURE_FREE_TRANSFERS <- 1 # δ_t for t > Ω (simplification)

# --- Solver Choice ---
SOLVER <- "glpk" # Or "symphony"

# --- 2. Data Loading and Preparation ---

cat("Loading data...\n")
# Using read_csv for better type handling, adjust if needed
# Specify column types to handle potential variations and ensure correct types
# Check your actual column names in the CSV! These are guesses.
# Assuming 'value' is cost*10, 'xP' is expected points.
raw_data <- read_csv(DATA_URL,
                     col_types = cols(
                       GW = col_double(),
                       player_id = col_double(), # Assuming this is a player ID
                       name = col_character(),
                       position = col_character(),
                       team = col_character(),
                       value = col_double(), # Cost * 10
                       xP = col_double(),    # Expected points
                       # Add other columns as needed, or use cols(.default = col_guess())
                       .default = col_guess()
                     ))

cat("Data loaded. Preparing data...\n")

# --- Define Gameweek Sets ---
# T: Set of all GWs in the FPL season (determine from data or set manually if needed)
# We only need GWs relevant to our planning horizon
last_gw_in_horizon <- CURRENT_GW + PLANNING_HORIZON - 1
relevant_gws <- CURRENT_GW:last_gw_in_horizon

# T_omega: Set of GWs for which decisions are made (Ω, ..., Λ)
T_omega <- relevant_gws

# T_omega_plus_1: Set for transfer variables y, z (Ω+1, ..., Λ+1) - Careful with index
# Let's adjust indexing: y_ct, z_ct represent transfers *for* GW t (affecting squad state *at* t)
# This matches constraints 6 & 7 linking t and t-1.
# The paper's T_Ω+1 {Ω+1, ..., Λ} seems inconsistent with vars y_ct, z_ct in constraints
# Let's assume y_ct, z_ct are defined for t ∈ T_omega (actions taken *before* GW t starts)

# T_0: Set of past GWs (not explicitly needed if we have PREVIOUS_SQUAD_*)
# T_2: Set {2, ..., Λ} (used in some constraints if Ω=1)
T_2 <- if (CURRENT_GW == 1 && length(relevant_gws) > 1) relevant_gws[-1] else integer(0) # Empty if horizon=1


# --- Filter Data for Relevant Gameweeks and Players ---
fpl_data <- raw_data %>%
  filter(GW %in% relevant_gws) %>%
  # Ensure necessary columns exist
  select(GW, name, position, team, value, xP) %>%
  # Ensure only one entry per player per GW in the horizon
  distinct(GW, name, .keep_all = TRUE) %>%
  # Handle missing values if necessary (e.g., replace NA xP with 0)
  mutate(xP = ifelse(is.na(xP), 0, xP),
         value = ifelse(is.na(value), TOTAL_BUDGET * 2, value)) # Assign very high cost if NA

# --- Define Sets based on Filtered Data ---
# C_omega: Set of unique player names available in the planning horizon
player_pool <- fpl_data %>% distinct(name) %>% pull(name)
C_omega <- seq_along(player_pool) # Index {1, ..., C_omega_size}
player_map <- setNames(C_omega, player_pool)
player_map_rev <- setNames(player_pool, C_omega)

# D: Set of roles {1=GKP, 2=DEF, 3=MID, 4=FWD}
role_map <- c("GK" = 1, "DEF" = 2, "MID" = 3, "FWD" = 4)
D <- 1:4

# S_omega: Set of unique teams
team_pool <- fpl_data %>% distinct(name) %>% pull(name)
S_omega <- seq_along(team_pool) # Index {1, ..., S_omega_size}
team_map <- setNames(S_omega, team_pool)

# --- Create Parameter Arrays ---
# Get unique players and their info (role, team) - assuming constant
player_info <- fpl_data %>%
  filter(GW == CURRENT_GW) %>% # Use current GW for role/team info
  distinct(name, .keep_all = TRUE) %>%
  mutate(
    c_idx = player_map[name],
    d_idx = role_map[position],
    s_idx = team_map[team]
  ) %>%
  filter(!is.na(c_idx) & !is.na(d_idx) & !is.na(s_idx)) # Filter out players with missing info

# p_c,t: Projected score of player c in GW t
p <- array(0, dim = c(length(C_omega), length(T_omega)), dimnames = list(player_map_rev[C_omega], T_omega))
# k_c,t: Cost of player c in GW t
k <- array(TOTAL_BUDGET * 2, dim = c(length(C_omega), length(T_omega)), dimnames = list(player_map_rev[C_omega], T_omega)) # Default high cost

for (t in T_omega) {
  gw_data <- fpl_data %>% filter(GW == t) %>% mutate(c_idx = player_map[name])
  # Ensure c_idx is valid before using it for indexing
  valid_gw_data <- gw_data %>% filter(!is.na(c_idx) & c_idx %in% C_omega)

  p[valid_gw_data$c_idx, as.character(t)] <- valid_gw_data$xP
  k[valid_gw_data$c_idx, as.character(t)] <- valid_gw_data$value
}


# a_c,d: 1 if player c has role d, 0 otherwise
a <- array(0, dim = c(length(C_omega), length(D)), dimnames = list(player_map_rev[C_omega], names(role_map)))
for (i in 1:nrow(player_info)) {
   a[player_info$c_idx[i], player_info$d_idx[i]] <- 1
}


# beta_c,s: 1 if player c is in team s, 0 otherwise
beta <- array(0, dim = c(length(C_omega), length(S_omega)), dimnames = list(player_map_rev[C_omega], names(team_map)))
for (i in 1:nrow(player_info)) {
   beta[player_info$c_idx[i], player_info$s_idx[i]] <- 1
}

# r_d: Required number for role d
r <- setNames(unlist(ROLE_REQUIREMENTS), 1:4) # Ensure names are 1, 2, 3, 4

# delta_t: Free transfers for GW t
delta <- setNames(rep(DEFAULT_FUTURE_FREE_TRANSFERS, length(T_omega)), T_omega)
if (CURRENT_GW %in% T_omega) {
    delta[as.character(CURRENT_GW)] <- FREE_TRANSFERS_GW_OMEGA
}

# X_c, omega-1: Historical squad indicator for the previous GW (needed if Ω > 1)
X_prev <- setNames(rep(0, length(C_omega)), C_omega)
if (CURRENT_GW > 1) {
  prev_squad_indices <- player_map[PREVIOUS_SQUAD_PLAYERS]
  prev_squad_indices <- prev_squad_indices[!is.na(prev_squad_indices)] # Filter invalid names
  if(length(prev_squad_indices) != SQUAD_SIZE){
      warning(paste("Previous squad size is not 15. Found:", length(prev_squad_indices)))
      # Potentially stop execution or handle error appropriately
  }
   if(length(prev_squad_indices) > 0) {
        X_prev[prev_squad_indices] <- 1
   } else {
       warning("No valid players found from PREVIOUS_SQUAD_PLAYERS list.")
   }
}

cat("Data preparation complete. Building model...\n")

# --- 3. Build the Optimization Model ---

# Helper function for safe indexing (returns 0 if index out of bounds)
safe_k <- function(c, t) {
  t_char = as.character(t)
  if (c %in% rownames(k) && t_char %in% colnames(k)) {
    return(k[c, t_char])
  } else {
    # warning(paste("Cost data missing for player", c, "GW", t))
    return(TOTAL_BUDGET * 2) # Return prohibitive cost if data missing
  }
}

model <- MIPModel() %>%

  # --- Decision Variables ---
  # x_c,t: 1 if player c is in the squad in GW t, 0 otherwise
  add_variable(x[c, t], c = C_omega, t = T_omega, type = "binary") %>%

  # y_c,t: 1 if player c is brought *into* the squad *for* GW t (transfer in)
  add_variable(y[c, t], c = C_omega, t = T_omega, type = "binary") %>%

  # z_c,t: 1 if player c is removed *from* the squad *for* GW t (transfer out)
  add_variable(z[c, t], c = C_omega, t = T_omega, type = "binary") %>%

  # sigma_t: Number of *paid* transfers in GW t
  add_variable(sigma[t], t = T_omega, type = "continuous", lb = 0) %>%

  # b_t: Budget remaining *at the end* of GW t (after transfers and purchases for t)
  # We only really need b_omega for constraint checking, but define for all t for consistency
  # Let's redefine b_t slightly: budget available *at the start* of GW t's decisions
  add_variable(b[t], t = T_omega, type = "continuous", lb = 0) %>%

  # --- Objective Function (Eq 18) ---
  # Maximize total projected points over horizon, minus transfer penalties
  # Note: Penalty is applied for t >= CURRENT_GW (all GWs in T_omega)
  set_objective(sum_expr(p[c, as.character(t)] * x[c, t], c = C_omega, t = T_omega) -
                  sum_expr(TRANSFER_PENALTY * sigma[t], t = T_omega),
                sense = "max") %>%


  # --- Constraints ---

  # Linking constraints for x, y, z (Eq 6 & 7 adjusted for t=Ω)
  # If player is added (x[c,t]=1, prev_x=0), then y[c,t]=1
  # If player is removed (x[c,t]=0, prev_x=1), then z[c,t]=1
  add_constraint(x[c, t] - (if (t == CURRENT_GW) X_prev[c] else x[c, t-1]) <= y[c, t], c = C_omega, t = T_omega) %>% # Eq 6 logic
  add_constraint((if (t == CURRENT_GW) X_prev[c] else x[c, t-1]) - x[c, t] <= z[c, t], c = C_omega, t = T_omega) %>% # Eq 7 logic

  # Squad Size constraint (Eq 8 modified - applies to *all* GWs in horizon)
  # The paper's Eq 8 is only for GW Ω, but the logic requires size 15 always.
  add_constraint(sum_expr(x[c, t], c = C_omega) == SQUAD_SIZE, t = T_omega) %>%

  # Transfer balance constraint (Eq 9 alternative: y and z ensure size stays constant)
  # Total players transferred IN must equal total players transferred OUT for each GW
  # This ensures the squad size remains SQUAD_SIZE if it started at SQUAD_SIZE
  # This should be implicitly handled by Eq 6, 7, and 8, but can be added for robustness
  # add_constraint(sum_expr(y[c, t], c = C_omega) == sum_expr(z[c, t], c = C_omega), t = T_omega) %>% # Seems redundant given others

  # Transfer limit constraint (Eq 10)
  # Total transfers (in + out)/2 must be <= free transfers + paid transfers (sigma)
  # Note: sum(y) = sum(z) = number of transfers pairs. So sum(y) = total transfers / 2? No, sum(y) = players in, sum(z) = players out.
  # The paper uses sum(y) + sum(z) <= 2*(delta_t + sigma_t). Let's use sum(z) as the count of transfers.
  add_constraint(sum_expr(z[c, t], c = C_omega) <= delta[as.character(t)] + sigma[t], t = T_omega) %>%

  # Budget / Cost Constraint (Eq 11 adapted)
  # Cost of squad in GW t + budget remaining at *end* of t = Previous budget + cash from sales - cash for purchases
  # Let b[t] = budget *before* decisions for GW t.
  # b[t+1] = b[t] + sum(k[c,t]*z[c,t]) - sum(k[c,t]*y[c,t]) -- this links budgets across GWs
  # Simpler form from paper: Total cost of squad in GW t <= B (initial budget for Ω=1) or <= budget available entering GW t
  # sum(k[c,t] * x[c,t]) <= b[t] -- This seems more direct. Where b[omega] = PREVIOUS_BUDGET
  # Let's use the paper's conservation form:
  # Sum(k_ct * x_ct) + b_t = Sum(k_c,t-1 * x_c,t-1) + b_t-1
  # This tracks remaining budget explicitly.
  add_constraint(sum_expr(k[c, as.character(t)] * x[c, t], c = C_omega) + b[t] ==
                   (if (t == CURRENT_GW) sum_expr(safe_k(c, CURRENT_GW - 1) * X_prev[c], c = C_omega) + PREVIOUS_BUDGET
                    else sum_expr(k[c, as.character(t-1)] * x[c, t-1], c = C_omega) + b[t-1]),
                 t = T_omega) %>%
   # Ensure budget never exceeds initial total budget (or a reasonable upper bound)
   add_constraint(b[t] <= TOTAL_BUDGET * 1.1, t = T_omega) %>% # Add some slack for bank
   # Ensure budget doesn't drop below 0
   add_constraint(b[t] >= 0, t = T_omega) %>%


  # Role constraints (Eq 12)
  add_constraint(sum_expr(a[c, d] * x[c, t], c = C_omega) == r[as.character(d)], d = D, t = T_omega) %>%

  # Team constraints (Eq 13)
  add_constraint(sum_expr(beta[c, s] * x[c, t], c = C_omega) <= MAX_PER_TEAM, s = S_omega, t = T_omega)


# --- Sanity check variable counts ---
num_vars_x <- length(C_omega) * length(T_omega)
num_vars_y <- length(C_omega) * length(T_omega)
num_vars_z <- length(C_omega) * length(T_omega)
num_vars_sigma <- length(T_omega)
num_vars_b <- length(T_omega)
cat(sprintf("Model built. Number of variables:\n  x: %d\n  y: %d\n  z: %d\n  sigma: %d\n  b: %d\nTotal: %d\n",
            num_vars_x, num_vars_y, num_vars_z, num_vars_sigma, num_vars_b,
            num_vars_x+num_vars_y+num_vars_z+num_vars_sigma+num_vars_b))
cat("Number of players in pool:", length(C_omega), "\n")
cat("Number of GWs in horizon:", length(T_omega), "\n")

# --- 4. Solve the Model ---
cat("Solving model...\n")
# Increase solver timeout if needed, especially for larger horizons/player pools
result <- solve_model(model, with_ROI(solver = SOLVER)) # Set verbosity higher for more logs
result <- solve_model(model, with_ROI(solver = SOLVER, verbose = TRUE)) # Try 'verbose = TRUE'

# --- 5. Extract and Display Results ---

if (result$status == "optimal" || result$status == "feasible") { # Check for optimal or feasible status
  cat("\n--- Optimal Squad Found ---\n")

  # --- Get Solution ---
  sol_x <- get_solution(result, x[c, t])
  sol_y <- get_solution(result, y[c, t])
  sol_z <- get_solution(result, z[c, t])
  sol_sigma <- get_solution(result, sigma[t])
  sol_b <- get_solution(result, b[t])

  # --- Recommended Squad for CURRENT_GW (Ω) ---
  squad_gw_omega <- sol_x %>%
    filter(t == CURRENT_GW, value > 0.9) %>% # Filter for selected players in the target GW
    left_join(player_info %>% select(c_idx, player_name, pos, name), by = c("c" = "c_idx")) %>%
    mutate(cost = k[cbind(c, as.character(t))] / 10, # Show cost in millions
           xP = p[cbind(c, as.character(t))])

  cat(paste("\nRecommended Squad for GW", CURRENT_GW, ":\n"))
  print(squad_gw_omega %>% select(player_name, pos, name, cost, xP), n = SQUAD_SIZE)

  # --- Calculate Squad Cost and Points for GW Ω ---
  squad_cost_omega <- sum(squad_gw_omega$cost)
  squad_points_omega <- sum(squad_gw_omega$xP)
  cat(sprintf("\nTotal Squad Cost GW%d: %.1fm\n", CURRENT_GW, squad_cost_omega))
  cat(sprintf("Projected Points GW%d (Squad): %.1f\n", CURRENT_GW, squad_points_omega))

  # --- Transfers for CURRENT_GW (Ω) ---
  transfers_in <- sol_y %>%
    filter(t == CURRENT_GW, value > 0.9) %>%
    left_join(player_info %>% select(c_idx, player_name), by = c("c" = "c_idx"))
  transfers_out <- sol_z %>%
    filter(t == CURRENT_GW, value > 0.9) %>%
     left_join(player_info %>% select(c_idx, player_name), by = c("c" = "c_idx")) # Join based on player_info for consistency

  num_transfers <- nrow(transfers_out) # Number of transfers = number of players out
  paid_transfers <- sol_sigma %>% filter(t == CURRENT_GW) %>% pull(value)
  points_hit <- paid_transfers * TRANSFER_PENALTY

  cat(paste("\nTransfers for GW", CURRENT_GW, ":\n"))
  cat(sprintf("  >> Number of transfers: %d (Free: %d, Paid: %.0f, Hit: -%.0f pts)\n",
              num_transfers, delta[as.character(CURRENT_GW)], paid_transfers, points_hit))

  if (nrow(transfers_out) > 0) {
    cat("  Players OUT:\n")
    print(transfers_out %>% select(player_name), n = Inf)
  } else {
    cat("  Players OUT: None\n")
  }

  if (nrow(transfers_in) > 0) {
    cat("  Players IN:\n")
    print(transfers_in %>% select(player_name), n = Inf)
  } else {
     cat("  Players IN: None\n")
  }

  # --- Budget ---
  budget_remaining <- sol_b %>% filter(t == CURRENT_GW) %>% pull(value)
  cat(sprintf("\nBudget Remaining (End of GW%d actions): %.1fm\n", CURRENT_GW, budget_remaining / 10))

  # --- Future Plans (Optional) ---
  # You could print the planned squads or transfers for t > CURRENT_GW as well
   cat("\n--- Future Plans (Tentative) ---\n")
   for (fut_t in T_omega[T_omega > CURRENT_GW]) {
     fut_squad <- sol_x %>% filter(t == fut_t, value > 0.9) %>% nrow()
     fut_transfers_out <- sol_z %>% filter(t == fut_t, value > 0.9) %>% nrow()
     fut_paid <- sol_sigma %>% filter(t==fut_t) %>% pull(value)
     fut_hit <- fut_paid * TRANSFER_PENALTY
     cat(sprintf("GW %d: Squad size %d. Transfers planned: %d (Hit: -%.0f pts)\n",
                 fut_t, fut_squad, fut_transfers_out, fut_hit))
   }


} else {
  cat("\n--- Solver did not find an optimal solution ---\n")
  cat(paste("Solver status:", result$status, "\n"))
  # You might want to inspect the model or solver logs further.
  # Potential issues: Infeasible model (conflicting constraints), data issues, solver limitations/bugs.
}
