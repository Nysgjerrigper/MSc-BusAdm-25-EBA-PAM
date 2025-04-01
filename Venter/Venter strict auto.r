#Venter strict auto
rm(list = ls(all = TRUE)) # Clear environment at the start

# --- 0. Load Libraries ---
library(lpSolveAPI)
library(dplyr)
library(readr)
library(tidyr)
library(slam) # Keep for now, potentially removable but harmless

# --- 1. User Inputs & FPL Constants ---
START_GW <- 2              # <<< SET THE STARTING GAMEWEEK OF THE RUN >>>
END_GW <- 3                # <<< SET THE END GAMEWEEK OF THE RUN >>>
# Note: The loop runs from START_GW up to and including END_GW.

# PLANNING_HORIZON defines how many GWs (including current) are considered *within* each optimization step.
# Set to 1 for a myopic (single-week) optimization, >1 for multi-week planning.
PLANNING_HORIZON <- 2      # <<< PLANNING HORIZON FOR EACH GW's OPTIMIZATION >>>

TOTAL_BUDGET <- 1000           # B: Initial Budget (used only if START_GW = 1, or first run)
SQUAD_SIZE <- 15               # Required squad size
MAX_PER_TEAM <- 3              # Max players per EPL team
ROLE_REQUIREMENTS <- list(GK = 2, DEF = 5, MID = 5, FWD = 3) # r_d
TRANSFER_PENALTY <- 4          # R: Penalty per paid transfer
DEFAULT_FUTURE_FREE_TRANSFERS <- 1 # δ_t for t > Ω (within horizon)
INITIAL_FREE_TRANSFERS <- 1    # Free transfers available for the very first GW (START_GW)

DATA_URL <- "https://raw.githubusercontent.com/Nysgjerrigper/test/refs/heads/main/Differensiert%20gw%20alle%20tre%20sesonger(22-24)%2C%20heltall.csv"

# --- Variables to store state between iterations ---
# Initialize before the loop
last_gw_squad_names <- c()   # Stores the optimal squad from the previous GW run
last_gw_budget_raw <- TOTAL_BUDGET # Stores the remaining budget from the previous GW run

# --- Automated Rolling Horizon Loop ---
for (current_gw_iter in START_GW:END_GW) {

    cat(paste("\n=====================================================\n"))
    cat(paste("--- Optimizing for Gameweek:", current_gw_iter, "---"))
    cat(paste("\n--- Planning Horizon:", PLANNING_HORIZON, "GW(s) ---"))
    cat(paste("\n=====================================================\n"))

    # --- Settings for Current Iteration ---
    CURRENT_GW <- current_gw_iter # Ω for this iteration

    # Determine free transfers for *this* optimization's start (GW Ω)
    if (CURRENT_GW == START_GW) {
        # Use initial settings for the very first GW of the entire run
        PREVIOUS_SQUAD_PLAYERS <- c() # Assume no squad before START_GW unless specified otherwise
        PREVIOUS_BUDGET <- TOTAL_BUDGET
        FREE_TRANSFERS_GW_OMEGA <- INITIAL_FREE_TRANSFERS
        cat(paste("Initializing run: Using TOTAL_BUDGET =", TOTAL_BUDGET, "and INITIAL_FREE_TRANSFERS =", INITIAL_FREE_TRANSFERS, "\n"))
        # If START_GW > 1, you might want to manually set the actual squad/budget
        # from GW (START_GW - 1) here instead of using defaults.
        # Example: if (START_GW == 10) { PREVIOUS_SQUAD_PLAYERS <- ...; PREVIOUS_BUDGET <- ... }

    } else {
        # Use results from the *previous iteration*
        PREVIOUS_SQUAD_PLAYERS <- last_gw_squad_names
        PREVIOUS_BUDGET <- last_gw_budget_raw
        # Subsequent GWs typically start with 1 free transfer (or 2 if saved - simplified here)
        FREE_TRANSFERS_GW_OMEGA <- DEFAULT_FUTURE_FREE_TRANSFERS # δ_Ω for this iteration
        cat(paste("Using previous squad (", length(PREVIOUS_SQUAD_PLAYERS), " players) and budget =", PREVIOUS_BUDGET, "\n"))
        cat(paste("Setting free transfers for this GW to:", FREE_TRANSFERS_GW_OMEGA, "\n"))
    }


    # --- 2. Data Loading and Preparation (for this iteration's horizon) ---
    cat("Loading data...\n")
    # Load fresh data each time to ensure costs/points for the horizon are correct
    raw_data <- read_csv(DATA_URL, show_col_types = FALSE)

    # --- Define Gameweek Sets for this iteration ---
    last_gw_in_horizon <- CURRENT_GW + PLANNING_HORIZON - 1
    T_omega_actual_gws <- CURRENT_GW:last_gw_in_horizon # Actual GW numbers {Ω, ..., Λ}
    num_gws_horizon <- length(T_omega_actual_gws)
    T_idx <- 1:num_gws_horizon # Indices for the horizon [1, ..., num_gws_horizon]

    # Check if horizon extends beyond available data (simple check)
    max_gw_in_data <- max(raw_data$GW, na.rm = TRUE)
    if (last_gw_in_horizon > max_gw_in_data) {
        warning(paste("Planning horizon extends beyond max GW in data (", max_gw_in_data, "). Truncating horizon."))
        last_gw_in_horizon <- max_gw_in_data
        T_omega_actual_gws <- CURRENT_GW:last_gw_in_horizon
        num_gws_horizon <- length(T_omega_actual_gws)
        T_idx <- 1:num_gws_horizon
        if (num_gws_horizon < 1) {
            cat("Cannot run optimization, current GW is beyond data.\n")
            break # Stop the loop if no valid horizon exists
        }
    }

    cat("Preparing data for GWs:", paste(T_omega_actual_gws, collapse=", "), "\n")

    # --- Filter Data for Horizon ---
    fpl_data_horizon <- raw_data %>%
        filter(GW %in% T_omega_actual_gws) %>%
        mutate(
            xP = ifelse(is.na(xP), 0, xP),
            value = ifelse(is.na(value), TOTAL_BUDGET * 10, value) # High cost for missing values
        ) %>%
        distinct(GW, name, .keep_all = TRUE)

    # --- Define Player Set (C) ---
    players_in_horizon <- unique(fpl_data_horizon$name)
    # Include players from previous squad even if not in horizon data
    all_player_names <- unique(c(players_in_horizon, PREVIOUS_SQUAD_PLAYERS))
    num_players <- length(all_player_names)
    player_indices <- 1:num_players
    player_map <- setNames(player_indices, all_player_names)
    player_map_rev <- setNames(all_player_names, player_indices)

    cat("Total unique players considered for GW", CURRENT_GW, ":", num_players, "\n")

    # --- Get Player Info (Role D, Team S) ---
    # (Same logic as before, ensures info is gathered based on current data + previous squad)
    player_info_current <- fpl_data_horizon %>%
        filter(GW == CURRENT_GW) %>%
        distinct(name, .keep_all = TRUE) %>%
        select(name, position, team)

    player_info_prev_only <- character(0)
    # Only look for previous info if not the very first GW *of the script run*
    if (CURRENT_GW > START_GW && length(PREVIOUS_SQUAD_PLAYERS) > 0) {
        player_info_prev_only <- setdiff(PREVIOUS_SQUAD_PLAYERS, player_info_current$name)
    }

    player_info_missing <- data.frame()
    if(length(player_info_prev_only) > 0) {
        info_for_missing <- raw_data %>%
            filter(name %in% player_info_prev_only) %>%
            arrange(desc(GW)) %>% distinct(name, .keep_all = TRUE) %>% select(name, position, team)
        player_info_missing <- info_for_missing
    }
    player_info <- bind_rows(player_info_current, player_info_missing)

    # --- Define Role Set (D) & Team Set (S) ---
    role_names <- names(ROLE_REQUIREMENTS)
    num_roles <- length(role_names)
    D_idx <- 1:num_roles
    ROLE_REQUIREMENTS_vec <- ROLE_REQUIREMENTS[role_names] %>% unlist()

    team_pool_names <- player_info %>% distinct(team) %>% pull(team) %>% na.omit()
    num_teams <- length(team_pool_names)
    S_idx <- 1:num_teams
    team_map <- setNames(S_idx, team_pool_names)

    # --- Create Parameter Arrays/Vectors (Strictly inside loop) ---
    p <- matrix(0, nrow = num_players, ncol = num_gws_horizon)
    k <- matrix(TOTAL_BUDGET * 10, nrow = num_players, ncol = num_gws_horizon)

    for (t_idx in T_idx) {
        gw_actual <- T_omega_actual_gws[t_idx]
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

    a <- matrix(0, nrow = num_players, ncol = num_roles)
    for (i in 1:nrow(player_info)) {
       player_idx <- player_map[player_info$name[i]]
       role_name <- player_info$position[i]
       role_idx <- which(role_names == role_name)
       if(!is.na(player_idx) && length(role_idx) > 0) { a[player_idx, role_idx] <- 1 }
    }

    beta <- matrix(0, nrow = num_players, ncol = num_teams)
    for (i in 1:nrow(player_info)) {
        player_idx <- player_map[player_info$name[i]]
        team_name_player <- player_info$team[i]
        team_idx <- team_map[team_name_player]
        if(!is.na(player_idx) && !is.na(team_idx)){ beta[player_idx, team_idx] <- 1 }
    }

    delta <- setNames(rep(DEFAULT_FUTURE_FREE_TRANSFERS, num_gws_horizon), T_idx)
    delta[1] <- FREE_TRANSFERS_GW_OMEGA # δ_Ω for this iteration

    # X_c,Ω-1: Historical squad indicator using PREVIOUS_SQUAD_PLAYERS
    X_prev <- rep(0, num_players)
    names(X_prev) <- player_map_rev # Use names for matching
    if (length(PREVIOUS_SQUAD_PLAYERS) > 0) {
        # Ensure previous players are in the current player map
        valid_prev_players <- intersect(PREVIOUS_SQUAD_PLAYERS, names(player_map))
        invalid_prev_players <- setdiff(PREVIOUS_SQUAD_PLAYERS, valid_prev_players)
        if(length(invalid_prev_players) > 0) {
            warning("GW ", CURRENT_GW, ": These players from previous squad are not in current pool: ", paste(invalid_prev_players, collapse=", "))
        }
        prev_squad_indices <- player_map[valid_prev_players] # Get numeric indices
        if(length(prev_squad_indices) > 0) {
            X_prev[prev_squad_indices] <- 1
        }
         if(sum(X_prev) != SQUAD_SIZE && length(PREVIOUS_SQUAD_PLAYERS) > 0){ # Check only if prev squad existed
          warning(paste("GW ", CURRENT_GW, ": Previous squad size based on valid players is not 15. Found:", sum(X_prev)))
        }
    }

    # k_c,Ω-1: Cost of players in previous squad (Strict calculation)
    k_omega_minus_1 <- setNames(rep(TOTAL_BUDGET * 10, num_players), player_map_rev) # Default high cost
    k_prev_squad_cost <- 0 # Initialize Sum_c(k_c,Ω-1 * X_c,Ω-1)

    # Only calculate if not the very first GW of the script run
    if (CURRENT_GW > START_GW) {
        gw_omega_minus_1 <- CURRENT_GW - 1
        prev_gw_data <- raw_data %>% filter(GW == gw_omega_minus_1)

        if(nrow(prev_gw_data) > 0) {
            cat("Getting player costs from actual previous GW:", gw_omega_minus_1, "\n")
            # Update costs for players present in that GW's data
            players_in_prev_gw <- intersect(prev_gw_data$name, names(player_map))
            indices_in_prev_gw <- player_map[players_in_prev_gw]
            prev_gw_data_filtered <- prev_gw_data %>%
                filter(name %in% players_in_prev_gw) %>%
                mutate(value = ifelse(is.na(value), TOTAL_BUDGET * 10, value)) # Handle NA cost in prev GW

            k_omega_minus_1[indices_in_prev_gw] <- prev_gw_data_filtered$value

            # Calculate the cost using these k_omega_minus_1 values and X_prev
            k_prev_squad_cost <- sum(k_omega_minus_1 * X_prev) # Sum cost only for players where X_prev is 1

             players_in_prev_squad_indices <- which(X_prev > 0.5)
             if(any(k_omega_minus_1[players_in_prev_squad_indices] >= TOTAL_BUDGET * 5)) {
                 missing_cost_players <- names(k_omega_minus_1[players_in_prev_squad_indices])[k_omega_minus_1[players_in_prev_squad_indices] >= TOTAL_BUDGET * 5]
                 warning("GW ", CURRENT_GW, ": Could not find k_c,Ω-1 cost for some players in previous squad: ", paste(missing_cost_players, collapse=", "), ". Using default high cost.")
             }
             cat("Calculated previous squad cost (k_prev_squad_cost):", k_prev_squad_cost, "\n")

        } else {
            warning("GW ", CURRENT_GW, ": Could not find data for previous GW ", gw_omega_minus_1, ". Budget constraint may be incorrect.")
            # Fallback: Estimate cost? Risky. Using 0 might be safer if budget is tight.
            k_prev_squad_cost <- 0 # Or sum(X_prev) * average_cost
            warning("Setting previous squad cost to 0. Results may be inaccurate.")
        }
    } else {
        # For the START_GW, the budget constraint uses PREVIOUS_BUDGET directly (which is TOTAL_BUDGET)
        # and k_prev_squad_cost is effectively 0 as there's no previous squad value to consider.
        k_prev_squad_cost <- 0
    }


    # --- 3. Setup lpSolveAPI Model (Strictly inside loop) ---
    n_x <- num_players * num_gws_horizon; n_y <- num_players * num_gws_horizon; n_z <- num_players * num_gws_horizon; n_sigma <- num_gws_horizon; n_b <- num_gws_horizon; num_variables <- n_x + n_y + n_z + n_sigma + n_b
    get_x_col <- function(c, t_idx) { (t_idx - 1) * num_players + c }; get_y_col <- function(c, t_idx) { n_x + (t_idx - 1) * num_players + c }; get_z_col <- function(c, t_idx) { n_x + n_y + (t_idx - 1) * num_players + c }; get_sigma_col <- function(t_idx) { n_x + n_y + n_z + t_idx }; get_b_col <- function(t_idx) { n_x + n_y + n_z + n_sigma + t_idx }

    lprec <- make.lp(0, num_variables)
    set.type(lprec, columns = 1:n_x, type = "binary")
    set.type(lprec, columns = (n_x + 1):(n_x + n_y), type = "binary")
    set.type(lprec, columns = (n_x + n_y + 1):(n_x + n_y + n_z), type = "binary")
    set.bounds(lprec, lower = rep(0, n_sigma), columns = get_sigma_col(1):get_sigma_col(num_gws_horizon))
    set.bounds(lprec, lower = rep(0, n_b), columns = get_b_col(1):get_b_col(num_gws_horizon))

    # --- Objective Function (Eq 18) ---
    obj_coeffs <- numeric(num_variables)
    for (t_idx in T_idx) {
        for (c in 1:num_players) {
            point_value <- p[c, t_idx]
            obj_coeffs[get_x_col(c, t_idx)] <- ifelse(!is.na(point_value) && is.numeric(point_value), point_value, 0)
        }
        obj_coeffs[get_sigma_col(t_idx)] <- -TRANSFER_PENALTY
    }
    set.objfn(lprec, obj_coeffs)
    lp.control(lprec, sense = "max")

    # --- Add Constraints (Strict Equivalence) ---
    # Eq 6 & 7: Transfer Linking
    cat("Adding transfer linking constraints...\n")
    for (t_idx in T_idx) {
        for (c in 1:num_players) {
            x_ct_col <- get_x_col(c, t_idx); y_ct_col <- get_y_col(c, t_idx); z_ct_col <- get_z_col(c, t_idx)
            if (t_idx == 1) {
                add.constraint(lprec, xt = c(1, -1), type = "<=", rhs = X_prev[c], indices = c(x_ct_col, y_ct_col))
                add.constraint(lprec, xt = c(-1, -1), type = "<=", rhs = -X_prev[c], indices = c(x_ct_col, z_ct_col))
            } else {
                x_ctm1_col <- get_x_col(c, t_idx - 1)
                add.constraint(lprec, xt = c(1, -1, -1), type = "<=", rhs = 0, indices = c(x_ct_col, x_ctm1_col, y_ct_col))
                add.constraint(lprec, xt = c(1, -1, -1), type = "<=", rhs = 0, indices = c(x_ctm1_col, x_ct_col, z_ct_col))
            }
        }
    }
    # Eq 8: Squad Size
    cat("Adding squad size constraints...\n")
    for (t_idx in T_idx) {
        cols <- sapply(1:num_players, get_x_col, t_idx = t_idx)
        add.constraint(lprec, xt = rep(1, num_players), type = "=", rhs = SQUAD_SIZE, indices = cols)
    }
    # Eq 9: Transfer Balance
    cat("Adding transfer balance constraints...\n")
    for (t_idx in T_idx) {
        y_cols <- sapply(1:num_players, get_y_col, t_idx = t_idx)
        z_cols <- sapply(1:num_players, get_z_col, t_idx = t_idx)
        add.constraint(lprec, xt = c(rep(1, num_players), rep(-1, num_players)), type = "=", rhs = 0, indices = c(y_cols, z_cols))
    }
    # Eq 10: Transfer Limit
    cat("Adding transfer limit constraints...\n")
    for (t_idx in T_idx) {
        y_cols <- sapply(1:num_players, get_y_col, t_idx = t_idx)
        z_cols <- sapply(1:num_players, get_z_col, t_idx = t_idx)
        sigma_col <- get_sigma_col(t_idx)
        add.constraint(lprec, xt = c(rep(1, num_players), rep(1, num_players), -2), type = "<=", rhs = 2 * delta[t_idx], indices = c(y_cols, z_cols, sigma_col))
    }
    # Eq 11: Budget Flow
    cat("Adding budget flow constraints...\n")
    for (t_idx in T_idx) {
        x_ct_cols <- sapply(1:num_players, get_x_col, t_idx = t_idx)
        b_t_col <- get_b_col(t_idx)
        coeffs_t <- k[, t_idx] # k_c,t
        if (t_idx == 1) {
            rhs_budget1 <- k_prev_squad_cost + PREVIOUS_BUDGET
            add.constraint(lprec, xt = c(coeffs_t, 1), type = "=", rhs = rhs_budget1, indices = c(x_ct_cols, b_t_col))
        } else {
            x_ctm1_cols <- sapply(1:num_players, get_x_col, t_idx = t_idx - 1)
            b_tm1_col <- get_b_col(t_idx - 1)
            coeffs_tm1 <- -k[, t_idx - 1] # -k_c,t-1
            all_cols <- c(x_ct_cols, x_ctm1_cols, b_t_col, b_tm1_col)
            all_coeffs <- c(coeffs_t, coeffs_tm1, 1, -1)
            add.constraint(lprec, xt = all_coeffs, type = "=", rhs = 0, indices = all_cols)
        }
    }
    # Eq 12: Role Requirements
    cat("Adding role constraints...\n")
    for (t_idx in T_idx) {
        for (role_idx in D_idx) {
            player_cols <- sapply(1:num_players, get_x_col, t_idx = t_idx)
            coeffs_role <- a[, role_idx]
            add.constraint(lprec, xt = coeffs_role, type = "=", rhs = ROLE_REQUIREMENTS_vec[role_idx], indices = player_cols)
        }
    }
    # Eq 13: Team Limit
    cat("Adding team constraints...\n")
    for (t_idx in T_idx) {
        for (team_idx in S_idx) {
            player_cols <- sapply(1:num_players, get_x_col, t_idx = t_idx)
            coeffs_team <- beta[, team_idx]
            add.constraint(lprec, xt = coeffs_team, type = "<=", rhs = MAX_PER_TEAM, indices = player_cols)
        }
    }

    # --- 4. Solve the MILP ---
    cat("Solving MILP model for GW", CURRENT_GW, "...\n")
    status <- solve(lprec, timeout = 120) # 2 minute timeout

    # --- 5. Process and Display Results ---
    if (status == 0) {
        cat("\n--- Optimal Solution Found for GW", CURRENT_GW, "(lpSolveAPI status 0) ---\n")
        objective_value <- get.objective(lprec)
        cat(sprintf("Optimal Objective Value (Total Points - Penalties): %.2f\n", objective_value))
        solution_vector <- get.variables(lprec)

        # --- Extract results specifically for the CURRENT_GW (t_idx = 1) ---
        cat("\n--- Results for Actions Taken in Gameweek:", CURRENT_GW, "---\n")

        squad_indices_gw1 <- which(solution_vector[get_x_col(1, 1):get_x_col(num_players, 1)] > 0.9)
        current_squad_names <- player_map_rev[squad_indices_gw1] # The squad chosen *for* CURRENT_GW

        # Get details for this squad
        squad_gw1_details <- player_info %>%
             filter(name %in% current_squad_names) %>%
             left_join(fpl_data_horizon %>% filter(GW == CURRENT_GW) %>% select(name, value, xP), by = "name")

        cat("\nRecommended Squad:\n")
        position_order <- c("GK", "DEF", "MID", "FWD")
        squad_gw1_details <- squad_gw1_details %>%
            mutate(position = factor(position, levels = position_order)) %>%
            arrange(position)
        print(squad_gw1_details %>% select(name, position, team, value, xP), n=SQUAD_SIZE)

        squad_cost_gw1 <- sum(squad_gw1_details$value, na.rm = TRUE)
        squad_points_gw1 <- sum(squad_gw1_details$xP, na.rm = TRUE)
        cat(sprintf("\nSquad Cost: %.1fm (raw: %d)\n", squad_cost_gw1 / 10, squad_cost_gw1))
        cat(sprintf("Projected Points (Squad Only): %.2f\n", squad_points_gw1))

        # Transfers made *for* this CURRENT_GW (t_idx = 1)
        transfers_in_indices <- which(solution_vector[get_y_col(1, 1):get_y_col(num_players, 1)] > 0.9)
        transfers_out_indices <- which(solution_vector[get_z_col(1, 1):get_z_col(num_players, 1)] > 0.9)
        transfers_in_names <- player_map_rev[transfers_in_indices]
        transfers_out_names <- player_map_rev[transfers_out_indices]
        paid_transfers <- solution_vector[get_sigma_col(1)]
        paid_transfers <- max(0, round(paid_transfers, 2))
        points_hit <- paid_transfers * TRANSFER_PENALTY

        cat("\nTransfers:\n")
        cat(sprintf("  >> Number of transfers: %d (Free: %d, Paid: %.1f, Hit: -%.1f pts)\n",
                   length(transfers_out_names), delta[1], paid_transfers, points_hit))
        cat("  Players IN:", if(length(transfers_in_names)>0) paste(transfers_in_names, collapse=", ") else "None", "\n")
        cat("  Players OUT:", if(length(transfers_out_names)>0) paste(transfers_out_names, collapse=", ") else "None", "\n")

        # Budget remaining *after* these transfers (b_1)
        budget_remaining <- solution_vector[get_b_col(1)]
        cat(sprintf("\nBudget Remaining (End of GW %d actions): %.1fm (raw: %.1f)\n", CURRENT_GW, budget_remaining / 10, budget_remaining))

        # --- Display Future Plans (Optional) ---
        if (num_gws_horizon > 1) {
            cat("\n--- Future Plan Summary (Tentative) ---\n")
            for (t_idx_fut in 2:num_gws_horizon) {
                 gw_actual_fut = T_omega_actual_gws[t_idx_fut]
                 num_transfers_fut_out <- sum(solution_vector[get_z_col(1, t_idx_fut):get_z_col(num_players, t_idx_fut)] > 0.9)
                 paid_transfers_fut <- solution_vector[get_sigma_col(t_idx_fut)]
                 paid_transfers_fut <- max(0, round(paid_transfers_fut, 2))
                 points_hit_fut <- paid_transfers_fut * TRANSFER_PENALTY
                 budget_fut <- solution_vector[get_b_col(t_idx_fut)]
                 cat(sprintf("GW %d: Planned Transfers (In/Out): %d (Hit: -%.1f pts), End Budget: %.1fm\n",
                             gw_actual_fut, num_transfers_fut_out, points_hit_fut, budget_fut/10 ))
            }
        }

        # --- Store results for the NEXT iteration ---
        last_gw_squad_names <- current_squad_names # The squad selected FOR this GW
        last_gw_budget_raw <- budget_remaining    # The budget remaining AFTER this GW's actions

    } else {
        cat("\n--- Solver did not find an optimal solution for GW", CURRENT_GW, "---\n")
        cat("lpSolveAPI status code:", status, "\n")
        # Handle specific non-optimal statuses if needed
        if (status == 2) { cat("Model is infeasible.\n") }
        else if (status == 3) { cat("Model is unbounded.\n") }
        else if (status == 5) { cat("Solver timed out.\n") }

        # Decide how to proceed: Stop? Use a fallback?
        cat("!!! Stopping automated run due to solver failure. !!!\n")
        break # Exit the loop
    }

    # Optional: Clean up the model object to free memory
    rm(lprec)
    # Optional: Pause between iterations
    # Sys.sleep(1)

} # --- End of Gameweek Loop ---

cat("\n--- Rolling Horizon Optimization Complete for GWs", START_GW, "to", END_GW, "---\n")