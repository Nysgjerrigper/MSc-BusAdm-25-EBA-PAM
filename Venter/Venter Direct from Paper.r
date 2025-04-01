# --- 0. Load Libraries ---
# install.packages("lpSolveAPI") # Run this line once if needed
# install.packages("dplyr")
# install.packages("readr")
# install.packages("tidyr")
# install.packages("slam") # Often needed for sparse matrix operations with solvers

library(lpSolveAPI)
library(dplyr)
library(readr)
library(tidyr)
library(slam) # Keep for now, potentially removable but harmless

# --- 1. User Inputs & FPL Constants (Matching Paper Notation) ---
CURRENT_GW <- 2              # Ω: The starting gameweek of your horizon
PLANNING_HORIZON <- 2      # Number of GWs to look ahead (Λ - Ω + 1)
# Example: CURRENT_GW = 5, PLANNING_HORIZON = 4 => Horizon T_Ω = {5, 6, 7, 8}

TOTAL_BUDGET <- 1000           # B: Initial Budget (used only if CURRENT_GW = 1)
SQUAD_SIZE <- 15               # Squad size requirement (implicit 15 in paper's Eq 8)
MAX_PER_TEAM <- 3              # Team limit (implicit 3 in paper's Eq 13)
ROLE_REQUIREMENTS <- list(GK = 2, DEF = 5, MID = 5, FWD = 3) # r_d: Role requirements
TRANSFER_PENALTY <- 4          # R: Points penalty (implicit 4 in paper's Eq 18)

# --- Settings for Rolling Horizon (Ignored if CURRENT_GW = 1) ---
# *** MANUALLY UPDATE THESE IF CURRENT_GW > 1 ***
PREVIOUS_SQUAD_PLAYERS <- c("Fabian Schar", "Rodrigo Moreno", "Erling Haaland", "Kevin De Bruyne", "Joao Cancelo", "Ivan Toney", "Thiago Alcantara do Nascimento","Kieran Trippier",
"Granit Xhaka","Gabriel Martinelli Silva","Reece James","Jose Malheiro de Sa","Gabriel Fernando de Jesus","Kalidou Koulibaly","Nick Pope") # Corresponds to X_c,Ω-1 = 1
PREVIOUS_BUDGET <- 10 # b_Ω-1: Budget remaining *before* transfers for CURRENT_GW
FREE_TRANSFERS_GW_OMEGA <- 1 # δ_Ω: Free transfers available *entering* CURRENT_GW

# --- Settings for Future GWs in Horizon (t > CURRENT_GW) ---
DEFAULT_FUTURE_FREE_TRANSFERS <- 1 # δ_t for t > Ω (simplification mentioned in paper context)

DATA_URL <- "https://raw.githubusercontent.com/Nysgjerrigper/test/refs/heads/main/Differensiert%20gw%20alle%20tre%20sesonger(22-24)%2C%20heltall.csv"

# --- 2. Data Loading and Preparation (Multi-GW) ---

cat("Loading data from:", DATA_URL, "\n")
raw_data <- read_csv(DATA_URL,
                     col_types = cols(
                       name = col_character(),
                       position = col_character(),
                       team = col_character(),
                       xP = col_double(),
                       value = col_double(),
                       GW = col_double(),
                       .default = col_guess()
                     ))

# --- Define Gameweek Sets ---
last_gw_in_horizon <- CURRENT_GW + PLANNING_HORIZON - 1
T_omega <- CURRENT_GW:last_gw_in_horizon # Set T_Ω = {Ω, ..., Λ}
num_gws_horizon <- length(T_omega)

cat("Preparing data for GWs:", paste(T_omega, collapse=", "), "\n")

# --- Filter Data for Relevant Gameweeks ---
fpl_data_horizon <- raw_data %>%
    filter(GW %in% T_omega) %>%
    # Practical Necessity: Handle missing values. Paper implicitly assumes complete data.
    # Setting xP=0 is reasonable. Setting value high effectively removes player.
    mutate(
        xP = ifelse(is.na(xP), 0, xP),
        value = ifelse(is.na(value), TOTAL_BUDGET * 10, value) # Use a very high cost
    ) %>%
    # Ensure one row per player per GW in the horizon
    distinct(GW, name, .keep_all = TRUE)

# --- Determine the full player pool across the horizon ---
# Start with players in the horizon data
player_pool_horizon <- fpl_data_horizon %>% distinct(name) %>% pull(name)
# Add players from the previous squad if not already included
player_pool <- unique(c(player_pool_horizon, PREVIOUS_SQUAD_PLAYERS))
num_players <- length(player_pool)
player_indices <- 1:num_players
player_map <- setNames(player_indices, player_pool)
player_map_rev <- setNames(player_pool, player_indices)

cat("Total unique players considered (Horizon + Prev Squad):", num_players, "\n")

# --- Role and Team mapping (assuming constant as per paper) ---
# Get info primarily from CURRENT_GW data within the horizon
player_info <- fpl_data_horizon %>%
    filter(GW == CURRENT_GW) %>%
    distinct(name, .keep_all = TRUE) %>%
    select(name, position, team)

# Add info for players ONLY in previous squad (if possible from raw_data)
if (CURRENT_GW > 1) {
    missing_info_players <- setdiff(PREVIOUS_SQUAD_PLAYERS, player_info$name)
    if(length(missing_info_players) > 0) {
        # Attempt to get their most recent info from raw_data
        missing_info <- raw_data %>%
            filter(name %in% missing_info_players) %>%
            arrange(desc(GW)) %>% # Get most recent info
            distinct(name, .keep_all = TRUE) %>%
            select(name, position, team)
        if(nrow(missing_info) > 0) {
            player_info <- bind_rows(player_info, missing_info)
        } else {
             warning("Could not find role/team info for some players listed in PREVIOUS_SQUAD_PLAYERS: ", paste(missing_info_players, collapse=", "))
        }
    }
}
# Final check for players in pool but without info
players_without_info <- setdiff(player_pool, player_info$name)
if(length(players_without_info) > 0) {
    warning("Some players in the pool lack role/team info: ", paste(players_without_info, collapse=", "))
    # These players will likely cause issues or be unusable in constraints
}


role_map <- c("GK" = 1, "DEF" = 2, "MID" = 3, "FWD" = 4) # Set D = {1, 2, 3, 4}
roles <- names(role_map)

# Team mapping (assuming constant)
team_pool <- player_info %>% distinct(team) %>% pull(team) %>% na.omit() # Set S_Ω
num_teams <- length(team_pool)
team_map <- setNames(1:num_teams, team_pool)

# --- Create Parameter Arrays (Multi-GW) ---
# p_c,t: Projected score [player_idx, gw_idx_in_horizon]
p <- matrix(0, nrow = num_players, ncol = num_gws_horizon, dimnames = list(player_map_rev, T_omega))
# k_c,t: Cost [player_idx, gw_idx_in_horizon]
k <- matrix(TOTAL_BUDGET * 10, nrow = num_players, ncol = num_gws_horizon, dimnames = list(player_map_rev, T_omega)) # Default high cost

for (t_idx in 1:num_gws_horizon) {
    gw_actual <- T_omega[t_idx]
    gw_data <- fpl_data_horizon %>% filter(GW == gw_actual)
    present_players_in_map <- intersect(gw_data$name, names(player_map))
    present_players_idx <- player_map[present_players_in_map]
    gw_data_filtered <- gw_data %>% filter(name %in% present_players_in_map)
    valid_idx <- !is.na(present_players_idx)
    if(any(valid_idx)){
        p[present_players_idx[valid_idx], t_idx] <- gw_data_filtered$xP[valid_idx]
        k[present_players_idx[valid_idx], t_idx] <- gw_data_filtered$value[valid_idx]
    }
}


# a_c,d: 1 if player c has role d [player_idx, role_idx]
a <- matrix(0, nrow = num_players, ncol = length(role_map), dimnames = list(player_map_rev, roles))
for (i in 1:nrow(player_info)) {
   player_idx <- player_map[player_info$name[i]]
   role_name <- player_info$position[i]
   role_idx <- which(roles == role_name)
   if(!is.na(player_idx) && length(role_idx) > 0) {
       a[player_idx, role_idx] <- 1
   }
}

# β_c,s: 1 if player c is in team s [player_idx, team_idx]
beta <- matrix(0, nrow = num_players, ncol = num_teams, dimnames = list(player_map_rev, team_pool))
for (i in 1:nrow(player_info)) {
    player_idx <- player_map[player_info$name[i]]
    team_name_player <- player_info$team[i]
    team_idx <- team_map[team_name_player]
    if(!is.na(player_idx) && !is.na(team_idx)){
        beta[player_idx, team_idx] <- 1
    }
}

# δ_t: Free transfers for GW t [gw_idx_in_horizon]
delta <- setNames(rep(DEFAULT_FUTURE_FREE_TRANSFERS, num_gws_horizon), T_omega)
delta[1] <- FREE_TRANSFERS_GW_OMEGA # δ_Ω

# X_c,Ω-1: Historical squad indicator for GW (CURRENT_GW - 1)
X_prev <- rep(0, num_players)
names(X_prev) <- player_map_rev
valid_prev_players <- character(0)
if (CURRENT_GW > 1) {
    valid_prev_players <- intersect(PREVIOUS_SQUAD_PLAYERS, names(player_map))
    invalid_prev_players <- setdiff(PREVIOUS_SQUAD_PLAYERS, valid_prev_players)
    if(length(invalid_prev_players) > 0) {
        warning("These players from PREVIOUS_SQUAD_PLAYERS are not in the player pool and will be ignored for X_prev: ", paste(invalid_prev_players, collapse=", "))
    }
    prev_squad_indices <- player_map[valid_prev_players]
    if(length(prev_squad_indices) > 0) {
        X_prev[prev_squad_indices] <- 1
    }
     if(sum(X_prev) != SQUAD_SIZE){
      warning(paste("Previous squad size based on valid players in PREVIOUS_SQUAD_PLAYERS is not 15. Found:", sum(X_prev)))
    }
} else {
    # If GW1 (Ω=1), X_prev is not used in constraints, b_0 = B (TOTAL_BUDGET)
    PREVIOUS_BUDGET <- TOTAL_BUDGET
}

# k_c,Ω-1: Cost of players in previous squad (Strictly requires cost from GW Ω-1)
k_omega_minus_1 <- setNames(rep(TOTAL_BUDGET * 10, num_players), player_map_rev) # Default high cost
k_prev_squad_cost <- 0 # Initialize Sum_c(k_c,Ω-1 * X_c,Ω-1)
if (CURRENT_GW > 1) {
    gw_omega_minus_1 <- CURRENT_GW - 1
    prev_gw_data <- raw_data %>% filter(GW == gw_omega_minus_1)

    if(nrow(prev_gw_data) > 0) {
        cat("Found data for previous GW", gw_omega_minus_1, "to get k_c,Ω-1 costs.\n")
        # Update costs for players present in that GW's data
        players_in_prev_gw <- intersect(prev_gw_data$name, names(player_map))
        indices_in_prev_gw <- player_map[players_in_prev_gw]
        prev_gw_data_filtered <- prev_gw_data %>% filter(name %in% players_in_prev_gw)

        # Handle missing values in previous GW cost data
        prev_gw_data_filtered <- prev_gw_data_filtered %>%
             mutate(value = ifelse(is.na(value), TOTAL_BUDGET * 10, value))

        k_omega_minus_1[indices_in_prev_gw] <- prev_gw_data_filtered$value

        # Calculate the cost using these k_omega_minus_1 values and X_prev
        k_prev_squad_cost <- sum(k_omega_minus_1 * X_prev) # Sum cost only for players where X_prev is 1

         # Check if any player needed for the sum still has the default high cost
         players_in_prev_squad_indices <- which(X_prev > 0.5)
         if(any(k_omega_minus_1[players_in_prev_squad_indices] >= TOTAL_BUDGET * 5)) {
             missing_cost_players <- names(k_omega_minus_1[players_in_prev_squad_indices])[k_omega_minus_1[players_in_prev_squad_indices] >= TOTAL_BUDGET * 5]
             warning("Could not find k_c,Ω-1 cost for some players in previous squad: ", paste(missing_cost_players, collapse=", "), ". Using default high cost.")
         }

    } else {
        warning("Could not find data for previous GW ", gw_omega_minus_1, " in raw_data. Cannot determine k_c,Ω-1. Budget constraint for GW Ω will be incorrect.")
        # Model might become infeasible or give wrong results if k_prev_squad_cost remains 0
        k_prev_squad_cost <- sum(X_prev) * 50 # Assign an arbitrary average cost as a fallback? Risky.
        warning("Assigning arbitrary previous squad cost. Results may be inaccurate.")
    }
}


# --- 3. Setup lpSolveAPI Model ---

# --- Calculate Variable Indices ---
# Order: x[c,t], y[c,t], z[c,t], sigma[t], b[t] (t ranges over T_Ω = {Ω..Λ})
n_x <- num_players * num_gws_horizon
n_y <- num_players * num_gws_horizon
n_z <- num_players * num_gws_horizon
n_sigma <- num_gws_horizon
n_b <- num_gws_horizon
num_variables <- n_x + n_y + n_z + n_sigma + n_b

# Helper functions to get column index for a variable
# t_idx is the index within the horizon (1 to num_gws_horizon)
get_x_col <- function(c, t_idx) { (t_idx - 1) * num_players + c }
get_y_col <- function(c, t_idx) { n_x + (t_idx - 1) * num_players + c }
get_z_col <- function(c, t_idx) { n_x + n_y + (t_idx - 1) * num_players + c }
get_sigma_col <- function(t_idx) { n_x + n_y + n_z + t_idx }
get_b_col <- function(t_idx) { n_x + n_y + n_z + n_sigma + t_idx }

# Create the lp model object
lprec <- make.lp(0, num_variables)

# Set variable types (Eq 14, 15, 16, 17)
set.type(lprec, columns = 1:n_x, type = "binary")           # x_c,t ∈ {0,1}
set.type(lprec, columns = (n_x + 1):(n_x + n_y), type = "binary") # y_c,t ∈ {0,1}
set.type(lprec, columns = (n_x + n_y + 1):(n_x + n_y + n_z), type = "binary") # z_c,t ∈ {0,1}
set.bounds(lprec, lower = rep(0, n_sigma), columns = get_sigma_col(1):get_sigma_col(num_gws_horizon)) # σ_t >= 0
set.bounds(lprec, lower = rep(0, n_b), columns = get_b_col(1):get_b_col(num_gws_horizon))       # b_t >= 0


# --- Objective Function (Eq 18) ---
# Maximize Sum_{c∈CΩ, t∈TΩ}(p_ct * x_ct) - R * Sum_{t∈TΩ}(sigma_t)
obj_coeffs <- numeric(num_variables)
for (t_idx in 1:num_gws_horizon) {
    for (c in 1:num_players) {
        point_value <- p[c, t_idx]
        # Basic check for validity, though earlier steps aim to prevent NAs
        if (!is.na(point_value) && is.numeric(point_value)) {
             obj_coeffs[get_x_col(c, t_idx)] <- point_value
        } else {
             obj_coeffs[get_x_col(c, t_idx)] <- 0 # Assign 0 if points are invalid/NA
        }
    }
    obj_coeffs[get_sigma_col(t_idx)] <- -TRANSFER_PENALTY # Use R = TRANSFER_PENALTY
}
set.objfn(lprec, obj_coeffs)
lp.control(lprec, sense = "max")


# --- Add Constraints ---

# Eq 6: x_c,t - x_c,t-1 <= y_c,t  (for t ∈ T_Ω)
# Eq 7: x_c,t-1 - x_c,t <= z_c,t  (for t ∈ T_Ω)
cat("Adding transfer linking constraints (Eq 6, 7)...\n")
for (t_idx in 1:num_gws_horizon) {
    for (c in 1:num_players) {
        x_ct_col <- get_x_col(c, t_idx)
        y_ct_col <- get_y_col(c, t_idx)
        z_ct_col <- get_z_col(c, t_idx)

        if (t_idx == 1) { # Special case for t = Ω, use X_c,Ω-1
            # Eq 6: x_c,Ω - X_c,Ω-1 <= y_c,Ω  =>  x_c,Ω - y_c,Ω <= X_c,Ω-1
            add.constraint(lprec, xt = c(1, -1), type = "<=", rhs = X_prev[c], indices = c(x_ct_col, y_ct_col))
            # Eq 7: X_c,Ω-1 - x_c,Ω <= z_c,Ω  => -x_c,Ω - z_c,Ω <= -X_c,Ω-1
            add.constraint(lprec, xt = c(-1, -1), type = "<=", rhs = -X_prev[c], indices = c(x_ct_col, z_ct_col))
        } else { # Case for t > Ω, use x_c,t-1
            x_ctm1_col <- get_x_col(c, t_idx - 1)
            # Eq 6: x_c,t - x_c,t-1 <= y_c,t  =>  x_c,t - x_c,t-1 - y_c,t <= 0
            add.constraint(lprec, xt = c(1, -1, -1), type = "<=", rhs = 0, indices = c(x_ct_col, x_ctm1_col, y_ct_col))
            # Eq 7: x_c,t-1 - x_c,t <= z_c,t  =>  x_c,t-1 - x_c,t - z_c,t <= 0
            add.constraint(lprec, xt = c(1, -1, -1), type = "<=", rhs = 0, indices = c(x_ctm1_col, x_ct_col, z_ct_col))
        }
    }
}

# Eq 8: Sum_c(x_c,t) == 15 (for t ∈ T_Ω)
cat("Adding squad size constraints (Eq 8)...\n")
for (t_idx in 1:num_gws_horizon) {
    cols <- sapply(1:num_players, get_x_col, t_idx = t_idx)
    add.constraint(lprec, xt = rep(1, num_players), type = "=", rhs = SQUAD_SIZE, indices = cols)
}

# Eq 9: Sum_c(y_c,t) - Sum_c(z_c,t) = 0 (for t ∈ T_Ω)
cat("Adding transfer balance constraints (Eq 9)...\n")
for (t_idx in 1:num_gws_horizon) {
    y_cols <- sapply(1:num_players, get_y_col, t_idx = t_idx)
    z_cols <- sapply(1:num_players, get_z_col, t_idx = t_idx)
    add.constraint(lprec, xt = c(rep(1, num_players), rep(-1, num_players)), type = "=", rhs = 0, indices = c(y_cols, z_cols))
}


# Eq 10: Sum_c(y_c,t + z_c,t) <= 2 * (δ_t + σ_t)  (for t ∈ T_Ω)
# Rearranged: Sum_c(y_c,t) + Sum_c(z_c,t) - 2*σ_t <= 2*δ_t
cat("Adding transfer limit constraints (Eq 10)...\n")
for (t_idx in 1:num_gws_horizon) {
    y_cols <- sapply(1:num_players, get_y_col, t_idx = t_idx)
    z_cols <- sapply(1:num_players, get_z_col, t_idx = t_idx)
    sigma_col <- get_sigma_col(t_idx)
    add.constraint(lprec, xt = c(rep(1, num_players), rep(1, num_players), -2), type = "<=", rhs = 2 * delta[t_idx], indices = c(y_cols, z_cols, sigma_col))
}

# Eq 11: Budget Flow: Sum_c(k_ct * x_ct) + b_t == Sum_c(k_c,t-1 * x_c,t-1) + b_t-1 (for t ∈ T_Ω)
cat("Adding budget flow constraints (Eq 11)...\n")
for (t_idx in 1:num_gws_horizon) {
    x_ct_cols <- sapply(1:num_players, get_x_col, t_idx = t_idx)
    b_t_col <- get_b_col(t_idx)
    coeffs_t <- k[, t_idx] # k_c,t

    if (t_idx == 1) { # Special case for t = Ω
        # Sum_c(k_cΩ * x_cΩ) + b_Ω == Sum_c(k_c,Ω-1 * X_c,Ω-1) + b_Ω-1
        # RHS = k_prev_squad_cost + PREVIOUS_BUDGET
        rhs_budget1 <- k_prev_squad_cost + PREVIOUS_BUDGET
        # Constraint: Sum_c(k_cΩ * x_cΩ) + b_Ω = RHS
        add.constraint(lprec, xt = c(coeffs_t, 1), type = "=", rhs = rhs_budget1, indices = c(x_ct_cols, b_t_col))
    } else { # Case for t > Ω
        # Sum_c(k_ct * x_ct) + b_t == Sum_c(k_c,t-1 * x_c,t-1) + b_t-1
        # Rearranged: Sum_c(k_ct * x_ct) - Sum_c(k_c,t-1 * x_c,t-1) + b_t - b_t-1 == 0
        x_ctm1_cols <- sapply(1:num_players, get_x_col, t_idx = t_idx - 1)
        b_tm1_col <- get_b_col(t_idx - 1)
        coeffs_tm1 <- -k[, t_idx - 1] # -k_c,t-1

        all_cols <- c(x_ct_cols, x_ctm1_cols, b_t_col, b_tm1_col)
        all_coeffs <- c(coeffs_t, coeffs_tm1, 1, -1)

        # NO FILTERING FOR STRICT EQUIVALENCE
        add.constraint(lprec, xt = all_coeffs, type = "=", rhs = 0, indices = all_cols)
    }
}
# NO OVERALL BUDGET UPPER BOUND FOR STRICT EQUIVALENCE

# Eq 12: Role Requirements: Sum_c(a_cd * x_ct) == r_d (for t ∈ T_Ω, d ∈ D)
cat("Adding role constraints (Eq 12)...\n")
for (t_idx in 1:num_gws_horizon) {
    for (role_idx in 1:length(roles)) {
        role_name <- roles[role_idx]
        player_cols <- sapply(1:num_players, get_x_col, t_idx = t_idx)
        coeffs_role <- a[, role_idx] # a_c,d for fixed d
        # Include all players in the sum, even if coefficient is 0
        add.constraint(lprec, xt = coeffs_role, type = "=", rhs = ROLE_REQUIREMENTS[[role_name]], indices = player_cols)
    }
}

# Eq 13: Team Limit: Sum_c(β_cs * x_ct) <= 3 (for t ∈ T_Ω, s ∈ S_Ω)
cat("Adding team constraints (Eq 13)...\n")
for (t_idx in 1:num_gws_horizon) {
    for (team_idx in 1:num_teams) {
        player_cols <- sapply(1:num_players, get_x_col, t_idx = t_idx)
        coeffs_team <- beta[, team_idx] # β_c,s for fixed s
        # Include all players in the sum
        add.constraint(lprec, xt = coeffs_team, type = "<=", rhs = MAX_PER_TEAM, indices = player_cols)
    }
}

# --- 4. Solve the MILP ---
cat("Solving MILP model (Strict Equivalence)...\n")

# Optional: Write model to file for debugging
# write.lp(lprec, "fpl_model_strict.lp")

# Use default solver settings unless specific ones are needed for convergence
# lp.control(lprec, presolve = c("rows", "cols", "lindep"), scaling = c("geometric", "equilibrate", "integers")) # Removed for strictness, add back if needed

status <- solve(lprec, timeout = 120) # 2 minute timeout

# --- 5. Process and Display Results ---

if (status == 0) {
    cat("\n--- Optimal Solution Found (lpSolveAPI status 0) ---\n")
    objective_value <- get.objective(lprec)
    cat(sprintf("Optimal Objective Value (Total Points - Penalties): %.2f\n", objective_value))
    solution_vector <- get.variables(lprec)

    cat("\n--- Results for Current Gameweek:", CURRENT_GW, "---\n")
    squad_indices_gw1 <- which(solution_vector[get_x_col(1, 1):get_x_col(num_players, 1)] > 0.9)
    squad_names_gw1 <- player_map_rev[squad_indices_gw1]
    squad_gw1_details <- player_info %>% filter(name %in% squad_names_gw1) %>%
                 left_join(fpl_data_horizon %>% filter(GW==CURRENT_GW) %>% select(name, value, xP), by="name")

    cat("\nRecommended Squad (GW ", CURRENT_GW, "):\n")
    position_order <- c("GK", "DEF", "MID", "FWD")
    squad_gw1_details <- squad_gw1_details %>%
        mutate(position = factor(position, levels = position_order)) %>%
        arrange(position)
    print(squad_gw1_details %>% select(name, position, team, value, xP))

    squad_cost_gw1 <- sum(squad_gw1_details$value, na.rm = TRUE)
    squad_points_gw1 <- sum(squad_gw1_details$xP, na.rm = TRUE)
    cat(sprintf("\nSquad Cost: %.1fm (raw: %d)\n", squad_cost_gw1 / 10, squad_cost_gw1))
    cat(sprintf("Projected Points (Squad Only): %.2f\n", squad_points_gw1))

    transfers_in_indices <- which(solution_vector[get_y_col(1, 1):get_y_col(num_players, 1)] > 0.9)
    transfers_out_indices <- which(solution_vector[get_z_col(1, 1):get_z_col(num_players, 1)] > 0.9)
    transfers_in_names <- player_map_rev[transfers_in_indices]
    transfers_out_names <- player_map_rev[transfers_out_indices]
    paid_transfers <- solution_vector[get_sigma_col(1)]
    paid_transfers <- max(0, round(paid_transfers, 2)) # Keep rounding for display
    points_hit <- paid_transfers * TRANSFER_PENALTY

    cat("\nTransfers (GW ", CURRENT_GW, "):\n")
    # Note: Eq 9 ensures len(transfers_in) == len(transfers_out)
    cat(sprintf("  >> Number of transfers (in/out): %d (Free: %d, Paid: %.1f, Hit: -%.1f pts)\n",
               length(transfers_out_names), delta[1], paid_transfers, points_hit))
     cat("  Players IN:", if(length(transfers_in_names)>0) paste(transfers_in_names, collapse=", ") else "None", "\n")
     cat("  Players OUT:", if(length(transfers_out_names)>0) paste(transfers_out_names, collapse=", ") else "None", "\n")

    budget_remaining <- solution_vector[get_b_col(1)]
    cat(sprintf("\nBudget Remaining (End of GW %d actions): %.1fm (raw: %.1f)\n", CURRENT_GW, budget_remaining / 10, budget_remaining))

    if (num_gws_horizon > 1) {
        cat("\n--- Future Plan Summary (Tentative) ---\n")
         for (t_idx in 2:num_gws_horizon) {
             gw_actual = T_omega[t_idx]
             num_transfers_fut_out <- sum(solution_vector[get_z_col(1, t_idx):get_z_col(num_players, t_idx)] > 0.9)
             paid_transfers_fut <- solution_vector[get_sigma_col(t_idx)]
             paid_transfers_fut <- max(0, round(paid_transfers_fut, 2))
             points_hit_fut <- paid_transfers_fut * TRANSFER_PENALTY
             budget_fut <- solution_vector[get_b_col(t_idx)]
             cat(sprintf("GW %d: Planned Transfers (In/Out): %d (Hit: -%.1f pts), End Budget: %.1fm\n",
                         gw_actual, num_transfers_fut_out, points_hit_fut, budget_fut/10 ))
         }
    }

} else {
    cat("\n--- Solver did not find an optimal solution ---\n")
    cat("lpSolveAPI status code:", status, "\n")
    if (status == 2) {
        cat("Model is infeasible. Check constraints and data, especially the strict k_c,Ω-1 cost calculation and budget.\n")
        # write.lp(lprec, "fpl_infeasible_strict.lp")
        # cat("Model written to fpl_infeasible_strict.lp for debugging.\n")
    } else if (status == 3) {
        cat("Model is unbounded. Check objective function and constraints. Removing stability features might cause this.\n")
    } else if (status == 5) {
        cat("Solver timed out. The strict model might be harder to solve.\n")
    }
}

# Clean up
# rm(lprec)