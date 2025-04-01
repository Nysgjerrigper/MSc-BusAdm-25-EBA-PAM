rm(list = ls(all = TRUE))
# --- 0. Load Libraries ---
library(lpSolveAPI)
library(dplyr)
library(readr)
library(tidyr)
library(slam)

# --- 1. User Inputs & FPL Constants ---
START_GW <- 1      # <<< SET THE STARTING GAMEWEEK >>>
END_GW <- 10             # <<< SET THE END GAMEWEEK (for automated rolling horizon) >>>
PLANNING_HORIZON <- 0       # <<< PLANNING HORIZON FOR EACH GW >>>

TOTAL_BUDGET <- 1000
SQUAD_SIZE <- 15
MAX_PER_TEAM <- 3
ROLE_REQUIREMENTS <- list(GK = 2, DEF = 5, MID = 5, FWD = 3)
TRANSFER_PENALTY <- 4
DEFAULT_FUTURE_FREE_TRANSFERS <- 1
FREE_TRANSFERS_GW_OMEGA <- 1 # Initial free transfers for START_GW

DATA_URL <- "https://raw.githubusercontent.com/Nysgjerrigper/test/refs/heads/main/Differensiert%20gw%20alle%20tre%20sesonger(22-24)%2C%20heltall.csv"

# --- Automated Rolling Horizon Loop ---
for (current_gw in START_GW:END_GW) {

    cat(paste("\n--- Optimizing for Gameweek:", current_gw, "---\n"))

    # --- Settings for Rolling Horizon (updated each GW) ---
    CURRENT_GW <- current_gw
    PLANNING_HORIZON <- PLANNING_HORIZON # Keep planning horizon constant

    # Initialize PREVIOUS_SQUAD_PLAYERS and PREVIOUS_BUDGET for the very first GW
    if (CURRENT_GW == START_GW) {
        PREVIOUS_SQUAD_PLAYERS <- c()
        PREVIOUS_BUDGET <- TOTAL_BUDGET
        FREE_TRANSFERS_GW_OMEGA <- FREE_TRANSFERS_GW_OMEGA # Use initial free transfers for the first GW
    } else {
        # Load previous squad and budget from the *previous iteration's output*
        # These variables (last_gw_squad_names, last_gw_budget_raw) are set at the end of each loop iteration
        PREVIOUS_SQUAD_PLAYERS <- last_gw_squad_names
        PREVIOUS_BUDGET <- last_gw_budget_raw
        FREE_TRANSFERS_GW_OMEGA <- DEFAULT_FUTURE_FREE_TRANSFERS # For subsequent GWs, default free transfers
    }


    # --- 2. Data Loading and Preparation (Multi-GW) ---
    cat("Loading data from:", DATA_URL, "\n")
    raw_data <- read_csv(DATA_URL, col_types = cols(name = col_character(), position = col_character(), team = col_character(), xP = col_double(), value = col_double(), GW = col_double(), .default = col_guess()))
    last_gw_in_horizon <- CURRENT_GW + PLANNING_HORIZON - 1
    T_omega <- CURRENT_GW:last_gw_in_horizon
    num_gws_horizon <- length(T_omega)

    cat("Preparing data for GWs:", paste(T_omega, collapse=", "), "\n")
    fpl_data_horizon <- raw_data %>% filter(GW %in% T_omega) %>% mutate(xP = ifelse(is.na(xP), 0, xP), value = ifelse(is.na(value), TOTAL_BUDGET * 5, value)) %>% distinct(GW, name, .keep_all = TRUE)
    player_pool <- fpl_data_horizon %>% distinct(name) %>% pull(name)
    num_players <- length(player_pool)
    player_indices <- 1:num_players
    player_map <- setNames(player_indices, player_pool)
    player_map_rev <- setNames(player_pool, player_indices)

    player_info <- fpl_data_horizon %>% filter(GW == CURRENT_GW) %>% distinct(name, .keep_all = TRUE) %>% select(name, position, team)
    role_map <- c("GK" = 1, "DEF" = 2, "MID" = 3, "FWD" = 4)
    roles <- names(role_map)
    team_pool <- player_info %>% distinct(team) %>% pull(team)
    num_teams <- length(team_pool)
    team_map <- setNames(1:num_teams, team_pool)

    p <- matrix(0, nrow = num_players, ncol = num_gws_horizon, dimnames = list(player_map_rev, T_omega))
    k <- matrix(TOTAL_BUDGET * 5, nrow = num_players, ncol = num_gws_horizon, dimnames = list(player_map_rev, T_omega))
    for (t_idx in 1:num_gws_horizon) { gw_actual <- T_omega[t_idx]; gw_data <- fpl_data_horizon %>% filter(GW == gw_actual); present_players_idx <- player_map[gw_data$name]; valid_idx <- !is.na(present_players_idx); p[present_players_idx[valid_idx], t_idx] <- gw_data$xP[valid_idx]; k[present_players_idx[valid_idx], t_idx] <- gw_data$value[valid_idx] }
    a <- matrix(0, nrow = num_players, ncol = length(role_map), dimnames = list(player_map_rev, roles))
    for (i in 1:nrow(player_info)) { player_idx <- player_map[player_info$name[i]]; role_name <- player_info$position[i]; role_idx <- which(roles == role_name); if(!is.na(player_idx) && length(role_idx) > 0) { a[player_idx, role_idx] <- 1 } }
    beta <- matrix(0, nrow = num_players, ncol = num_teams, dimnames = list(player_map_rev, team_pool))
    for (i in 1:nrow(player_info)) { player_idx <- player_map[player_info$name[i]]; team_name_player <- player_info$team[i]; team_idx <- team_map[team_name_player]; if(!is.na(player_idx) && !is.na(team_idx)){ beta[player_idx, team_idx] <- 1 } }

    delta <- setNames(rep(DEFAULT_FUTURE_FREE_TRANSFERS, num_gws_horizon), T_omega); delta[1] <- FREE_TRANSFERS_GW_OMEGA
        X_prev <- rep(0, num_players)
    names(X_prev) <- player_map_rev

    if (CURRENT_GW > 1) { 
        prev_squad_indices <- player_map[PREVIOUS_SQUAD_PLAYERS]
        prev_squad_indices <- prev_squad_indices[!is.na(prev_squad_indices)]
        if(length(prev_squad_indices) > 0) {
            X_prev[prev_squad_indices] <- 1
        }
        if(sum(X_prev) != SQUAD_SIZE){
            warning(paste("Previous squad size based on PREVIOUS_SQUAD_PLAYERS is not 15. Found:", sum(X_prev)))
        }
    } else {
        PREVIOUS_BUDGET <- TOTAL_BUDGET
    }
    k_prev <- setNames(rep(TOTAL_BUDGET*5, num_players), player_map_rev)
    if(CURRENT_GW > 1 && sum(X_prev)>0) { prev_player_names <- names(X_prev[X_prev > 0.5]); costs_now <- fpl_data_horizon %>% filter(GW == CURRENT_GW, name %in% prev_player_names) %>% select(name, value); if(nrow(costs_now)>0){ k_prev[player_map[costs_now$name]] <- costs_now$value } }
    k_prev_squad_cost <- sum(k_prev * X_prev)


    # --- 3. Setup lpSolveAPI Model ---
    n_x <- num_players * num_gws_horizon; n_y <- num_players * num_gws_horizon; n_z <- num_players * num_gws_horizon; n_sigma <- num_gws_horizon; n_b <- num_gws_horizon; num_variables <- n_x + n_y + n_z + n_sigma + n_b
    get_x_col <- function(c, t_idx) { (t_idx - 1) * num_players + c }; get_y_col <- function(c, t_idx) { n_x + (t_idx - 1) * num_players + c }; get_z_col <- function(c, t_idx) { n_x + n_y + (t_idx - 1) * num_players + c }; get_sigma_col <- function(t_idx) { n_x + n_y + n_z + t_idx }; get_b_col <- function(t_idx) { n_x + n_y + n_z + n_sigma + t_idx }
    lprec <- make.lp(0, num_variables); set.type(lprec, columns = 1:n_x, type = "binary"); set.type(lprec, columns = (n_x + 1):(n_x + n_y), type = "binary"); set.type(lprec, columns = (n_x + n_y + 1):(n_x + n_y + n_z), type = "binary")

    obj_coeffs <- numeric(num_variables)
    for (t_idx in 1:num_gws_horizon) { for (c in 1:num_players) { obj_coeffs[get_x_col(c, t_idx)] <- p[c, t_idx] }; obj_coeffs[get_sigma_col(t_idx)] <- -TRANSFER_PENALTY }
    set.objfn(lprec, obj_coeffs); lp.control(lprec, sense = "max")

    constraint_row_index <- 0
    cat("Adding transfer linking constraints (Eq 6, 7)...\n")
    for (t_idx in 1:num_gws_horizon) { for (c in 1:num_players) { constraint_row_index <- constraint_row_index + 2; x_ct_col <- get_x_col(c, t_idx); y_ct_col <- get_y_col(c, t_idx); z_ct_col <- get_z_col(c, t_idx); if (t_idx == 1) { add.constraint(lprec, xt = c(1, -1), type = "<=", rhs = X_prev[c], indices = c(x_ct_col, y_ct_col)); add.constraint(lprec, xt = c(-1, -1), type = "<=", rhs = -X_prev[c], indices = c(x_ct_col, z_ct_col)) } else { x_ctm1_col <- get_x_col(c, t_idx - 1); add.constraint(lprec, xt = c(1, -1, -1), type = "<=", rhs = 0, indices = c(x_ct_col, x_ctm1_col, y_ct_col)); add.constraint(lprec, xt = c(-1, 1, -1), type = "<=", rhs = 0, indices = c(x_ct_col, x_ctm1_col, z_ct_col)) } } }
    cat("Adding squad size constraints (Eq 8)...\n")
    for (t_idx in 1:num_gws_horizon) { constraint_row_index <- constraint_row_index + 1; cols <- sapply(1:num_players, get_x_col, t_idx = t_idx); add.constraint(lprec, xt = rep(1, num_players), type = "=", rhs = SQUAD_SIZE, indices = cols) }
    cat("Adding transfer limit constraints (Eq 10)...\n")
    for (t_idx in 1:num_gws_horizon) { constraint_row_index <- constraint_row_index + 1; z_cols <- sapply(1:num_players, get_z_col, t_idx = t_idx); sigma_col <- get_sigma_col(t_idx); add.constraint(lprec, xt = c(rep(1, num_players), -1), type = "<=", rhs = delta[t_idx], indices = c(z_cols, sigma_col)) }
    cat("Adding budget flow constraints (Eq 11)...\n")
    for (t_idx in 1:num_gws_horizon) { constraint_row_index <- constraint_row_index + 1; x_ct_cols <- sapply(1:num_players, get_x_col, t_idx = t_idx); b_t_col <- get_b_col(t_idx); coeffs_t <- k[, t_idx]; if (t_idx == 1) { rhs_budget1 <- k_prev_squad_cost + PREVIOUS_BUDGET; add.constraint(lprec, xt = c(coeffs_t, 1), type = "=", rhs = rhs_budget1, indices = c(x_ct_cols, b_t_col)) } else { x_ctm1_cols <- sapply(1:num_players, get_x_col, t_idx - 1); b_tm1_col <- get_b_col(t_idx - 1); coeffs_tm1 <- -k[, t_idx - 1]; all_cols <- c(x_ct_cols, x_ctm1_cols, b_t_col, b_tm1_col); all_coeffs <- c(coeffs_t, coeffs_tm1, 1, -1); valid_terms <- abs(all_coeffs) < (TOTAL_BUDGET * 2); add.constraint(lprec, xt = all_coeffs[valid_terms], type = "=", rhs = 0, indices = all_cols[valid_terms]) } }
    add.constraint(lprec, xt = rep(1, n_b), type = "<=", rhs = TOTAL_BUDGET * 1.5, indices = (get_b_col(1)):(get_b_col(num_gws_horizon)))
    cat("Adding role constraints (Eq 12)...\n")
    for (t_idx in 1:num_gws_horizon) { for (role_idx in 1:length(roles)) { constraint_row_index <- constraint_row_index + 1; role_name <- roles[role_idx]; player_cols <- sapply(1:num_players, get_x_col, t_idx = t_idx); coeffs_role <- a[, role_idx]; add.constraint(lprec, xt = coeffs_role, type = "=", rhs = ROLE_REQUIREMENTS[[role_name]], indices = player_cols) } }
    cat("Adding team constraints (Eq 13)...\n")
    for (t_idx in 1:num_gws_horizon) { for (team_idx in 1:num_teams) { constraint_row_index <- constraint_row_index + 1; player_cols <- sapply(1:num_players, get_x_col, t_idx = t_idx); coeffs_team <- beta[, team_idx]; add.constraint(lprec, xt = coeffs_team, type = "<=", rhs = MAX_PER_TEAM, indices = player_cols) } }

    # --- 4. Solve the MILP ---
    cat("Solving MILP model...\n")
    status <- solve(lprec, timeout = 120)

    # --- 5. Process and Display Results ---
    if (status == 0) {
        cat("\n--- Optimal Solution Found (lpSolveAPI status 0) ---\n")
        objective_value <- get.objective(lprec); cat(sprintf("Optimal Objective Value (Total Points - Penalties): %.2f\n", objective_value))
        solution_vector <- get.variables(lprec)

        cat("\n--- Results for Current Gameweek:", CURRENT_GW, "---\n")
        squad_indices_gw1 <- which(solution_vector[get_x_col(1, 1):get_x_col(num_players, 1)] > 0.9); squad_gw1 <- player_info %>% filter(name %in% player_map_rev[squad_indices_gw1]) %>% left_join(fpl_data_horizon %>% filter(GW==CURRENT_GW) %>% select(name, value, xP), by="name")
        cat("\nRecommended Squad:\n"); print(squad_gw1 %>% select(name, position, team, value, xP) %>% arrange(match(position, c("GK","DEF","MID","FWD"))))
        squad_cost_gw1 <- sum(squad_gw1$value); squad_points_gw1 <- sum(squad_gw1$xP); cat(sprintf("\nSquad Cost: %.1fm (raw: %d)\n", squad_cost_gw1 / 10, squad_cost_gw1)); cat(sprintf("Projected Points (Squad Only): %.2f\n", squad_points_gw1))

        transfers_in_indices <- which(solution_vector[get_y_col(1, 1):get_y_col(num_players, 1)] > 0.9); transfers_out_indices <- which(solution_vector[get_z_col(1, 1):get_z_col(num_players, 1)] > 0.9); transfers_in_names <- player_map_rev[transfers_in_indices]; transfers_out_names <- player_map_rev[transfers_out_indices]; paid_transfers <- solution_vector[get_sigma_col(1)]; points_hit <- paid_transfers * TRANSFER_PENALTY
        cat("\nTransfers:\n"); cat(sprintf("  >> Number of transfers: %d (Free: %d, Paid: %.1f, Hit: -%.1f pts)\n", length(transfers_out_names), delta[1], paid_transfers, points_hit)); cat("  Players IN:", if(length(transfers_in_names)>0) paste(transfers_in_names, collapse=", ") else "None", "\n"); cat("  Players OUT:", if(length(transfers_out_names)>0) paste(transfers_out_names, collapse=", ") else "None", "\n")

        budget_remaining <- solution_vector[get_b_col(1)]; cat(sprintf("\nBudget Remaining (End of GW %d actions): %.1fm (raw: %.1f)\n", CURRENT_GW, budget_remaining / 10, budget_remaining))
        cat("\n--- Future Plan Summary (Tentative) ---\n"); for (t_idx in 2:num_gws_horizon) { gw_actual = T_omega[t_idx]; num_transfers_fut <- sum(solution_vector[get_z_col(1, t_idx):get_z_col(num_players, t_idx)] > 0.9); paid_transfers_fut <- solution_vector[get_sigma_col(t_idx)]; points_hit_fut <- paid_transfers_fut * TRANSFER_PENALTY; budget_fut <- solution_vector[get_b_col(t_idx)]; cat(sprintf("GW %d: Planned Transfers: %d (Hit: -%.1f pts), End Budget: %.1fm\n", gw_actual, num_transfers_fut, points_hit_fut, budget_fut/10 )) }


        # --- Store squad and budget for next GW ---
        last_gw_squad_names <- squad_gw1$name # Save player names for next GW
        last_gw_budget_raw <- budget_remaining # Save raw budget value

    } else {
        cat("\n--- Solver did not find an optimal solution ---\n")
        cat("lpSolveAPI status code:", status, "\n")
    }
    # delete.lp(lprec) # Clean up (optional)
} # --- End of Gameweek Loop ---

cat("\n--- Rolling Horizon Optimization Complete ---\n")
