#MILP OVERSATT FUNGERER NESTEN

# --- 0. Setup ---
rm(list = ls(all = TRUE))
# Pakker
# install.packages("lpSolveAPI") # Run once if not installed
library(tidyverse)
library(lpSolveAPI)

# --- Helper Function Definition ---
create_pivot_matrix <- function(data_long, id_col, name_col, value_col, row_set, col_set, default_value = 0) {
  # Ensure row/col sets are character for reliable matching later
  row_set_char <- as.character(row_set)
  col_set_char <- as.character(col_set)

  # Select and Pivot
  # Group by id and name, then summarize if duplicates might have different values
  # (Using first() assumes duplicates are identical or first is correct)
  id_col_sym <- rlang::ensym(id_col)
  name_col_sym <- rlang::ensym(name_col)
  value_col_sym <- rlang::ensym(value_col)

  data_unique <- data_long %>%
    group_by(!!id_col_sym, !!name_col_sym) %>%
    summarise(value = first(!!value_col_sym), .groups = 'drop') # Ensure unique value per id/name

  matrix_wide <- data_unique %>%
    select(!!id_col_sym, !!name_col_sym, value) %>%
    pivot_wider(names_from = !!name_col_sym, values_from = value, id_cols = !!id_col_sym, values_fill = list(value = default_value)) # Use values_fill with list

  # Get row names and convert data part to matrix
  pivoted_rownames <- as.character(matrix_wide[[rlang::as_string(id_col_sym)]])
  matrix_numeric <- as.matrix(matrix_wide[,-1])
  rownames(matrix_numeric) <- pivoted_rownames

  # Ensure all required rows/cols exist, fill NAs introduced by missing players/gws
  final_matrix <- matrix(default_value, nrow = length(row_set_char), ncol = length(col_set_char),
                         dimnames = list(row_set_char, col_set_char))

  # Find common rows/cols to transfer data
  common_rows <- intersect(row_set_char, rownames(matrix_numeric))
  common_cols <- intersect(col_set_char, colnames(matrix_numeric))

  # Fill the final matrix
  if (length(common_rows) > 0 && length(common_cols) > 0) {
     # Ensure the subset from matrix_numeric is also numeric before assignment
     subset_mat <- matrix_numeric[common_rows, common_cols, drop = FALSE]
     # Handle potential list columns if pivot_wider still produced them (shouldn't with summarise)
     if(is.list(subset_mat)) {
        subset_mat <- apply(subset_mat, c(1,2), function(x) ifelse(is.null(x[[1]]), default_value, x[[1]]))
     }
     final_matrix[common_rows, common_cols] <- subset_mat
  }

  # Final NA check/fill (should be redundant)
  final_matrix[is.na(final_matrix)] <- default_value

  # Convert matrix elements explicitly to numeric if needed (safety check)
   if(!is.numeric(final_matrix)) {
       final_matrix <- apply(final_matrix, 2, as.numeric)
       rownames(final_matrix) <- row_set_char # Apply may strip rownames
   }


  # Final type check
  stopifnot("Matrix is not numeric"=is.numeric(final_matrix))
  stopifnot("Matrix has wrong number of rows"=nrow(final_matrix) == length(row_set_char))
  stopifnot("Matrix has wrong number of columns"=ncol(final_matrix) == length(col_set_char))
  # Check dimnames carefully after potential modifications
  stopifnot("Matrix rownames mismatch or wrong order"=all(rownames(final_matrix) == row_set_char))
  stopifnot("Matrix colnames mismatch or wrong order"=all(colnames(final_matrix) == col_set_char))

  return(final_matrix)
}


# --- 1. Load and Prepare Data ---
print("Loading and preparing data...")
# Make sure the file path is correct
allesesonger <- read_csv("Differensiert gw alle tre sesonger(22-24), heltall.csv", show_col_types = FALSE)
data <- allesesonger |> filter(GW <= 2)

# Define Sets
T_setofgameweeks <- sort(unique(data$GW))
P_setofplayers <- unique(data$player_id)
# Ensure team names are valid and consistent
data <- data %>% mutate(team = trimws(team)) # Trim whitespace
C_setofteams <- sort(unique(data$team[!is.na(data$team) & data$team != ""])) # Ensure no NA or empty teams
L_substitution <- 1:3
n_T <- length(T_setofgameweeks)
n_P <- length(P_setofplayers)
n_C <- length(C_setofteams)
n_L <- length(L_substitution)

# Data Validation and Cleaning
data_complete <- data %>%
  # Ensure only valid players are completed
  filter(player_id %in% P_setofplayers) %>%
  tidyr::complete(player_id, GW = T_setofgameweeks) %>%
  group_by(player_id) %>%
  # Fill position/team carefully - only fill if NA
  mutate(
      position = ifelse(is.na(position), first(na.omit(position)), position),
      team = ifelse(is.na(team), first(na.omit(team)), team)
      ) %>%
  # Alternative fill if direction matters and NAs are persistent
  # fill(position, team, .direction = "downup") %>%
  ungroup() %>%
  mutate(
    total_points = replace_na(total_points, 0),
    value = replace_na(value, 50) # Use actual FPL scale (e.g., 5.0 * 10 = 50)
  ) %>%
  # Ensure team names are still valid after fill
  filter(team %in% C_setofteams)

# Handle Duplicates (using summarise to be safer)
data_cleaned <- data_complete %>%
    group_by(player_id, GW) %>%
    summarise(
        position = first(na.omit(position)), # Take first non-NA
        team = first(na.omit(team)),
        total_points = first(total_points), # Assumes duplicates have same points/value
        value = first(value),
        .groups = 'drop'
        ) %>%
     # Ensure players still exist in the main set after summarise
     filter(player_id %in% P_setofplayers)

# Check if any players were lost
lost_players <- setdiff(P_setofplayers, unique(data_cleaned$player_id))
if (length(lost_players) > 0) {
    warning("Some players were lost during data cleaning/summarising. Check data.")
    print(lost_players)
    # Update P_setofplayers if players are truly missing/invalid
    # P_setofplayers <- P_setofplayers[!P_setofplayers %in% lost_players]
    # n_P <- length(P_setofplayers)
}
data <- data_cleaned # Use the cleaned data
# --- End Data Validation ---


# Define Subsets (using the cleaned 'data')
# Recreate maps based on cleaned data
pos_map <- setNames(data$position, data$player_id)
team_map <- setNames(data$team, data$player_id)

# Ensure player IDs used for subsetting exist in the cleaned data's player IDs
valid_pids_in_data <- unique(data$player_id)
Pdef <- intersect(P_setofplayers, valid_pids_in_data[pos_map[as.character(valid_pids_in_data)] == "DEF"])
Pmid <- intersect(P_setofplayers, valid_pids_in_data[pos_map[as.character(valid_pids_in_data)] == "MID"])
Pfwd <- intersect(P_setofplayers, valid_pids_in_data[pos_map[as.character(valid_pids_in_data)] == "FWD"])
Pgk <- intersect(P_setofplayers, valid_pids_in_data[pos_map[as.character(valid_pids_in_data)] == "GK"])
P_not_gk <- setdiff(P_setofplayers, Pgk)
n_P_not_gk <- length(P_not_gk)

# Ensure P_c uses cleaned data and valid teams/players
P_c <- split(data$player_id[data$player_id %in% P_setofplayers], data$team[data$player_id %in% P_setofplayers])
P_c <- P_c[names(P_c) %in% C_setofteams] # Filter by valid teams
P_c <- P_c[sapply(P_c, length) > 0]
C_setofteams <- names(P_c) # Update team set based on players actually present
n_C <- length(C_setofteams)

# Define GW subsets
medianavgameweeks <- median(T_setofgameweeks)
T_FH <- T_setofgameweeks[T_setofgameweeks <= medianavgameweeks]
T_SH <- T_setofgameweeks[T_setofgameweeks > medianavgameweeks]

# Define Indices
t <- T_setofgameweeks
p <- P_setofplayers # Use the original full set for variable dimensions
l <- L_substitution

# Define Parameters
R <- 4; MK <- 2; MD <- 5; MM <- 5; MF <- 3; MC <- 3; E <- 11; EK <- 1
ED <- 3; EM <- 2; EF <- 1; BS <- 1000.0
phi <- (MK+MD+MM+MF) - E; phi_K <- MK - EK
Q_bar <- 2; Q_under_bar <- 1
epsilon <- 0.1; kappa <- c(0.01, 0.005, 0.001)
beta <- n_P + 1
alpha_bar <- n_P + 1


# --- Prepare Coefficient Matrices using the function ---
points_matrix <- create_pivot_matrix(data, player_id, GW, total_points, P_setofplayers, T_setofgameweeks, default_value = 0)
value_matrix <- create_pivot_matrix(data, player_id, GW, value, P_setofplayers, T_setofgameweeks, default_value = 50)
print("Data preparation complete.")

# --- 2. Variable Mapping and Model Initialization ---
print("Initializing model and mapping variables...")
var_map <- list()
col_counter <- 0

add_vars <- function(base_name, idx1 = NULL, idx2 = NULL, idx3 = NULL) {
  grid_df <- NULL
  if (!is.null(idx3)) { # e.g., g[p_ngk, t, l]
      if(length(idx1)>0 && length(idx2)>0 && length(idx3)>0) grid_df <- expand.grid(idx1, idx2, idx3)
  } else if (!is.null(idx2)) { # e.g., x[p, t]
      if(length(idx1)>0 && length(idx2)>0) grid_df <- expand.grid(idx1, idx2)
  } else if (!is.null(idx1)) { # e.g., w[t]
      if(length(idx1)>0) grid_df <- data.frame(idx1)
  }

  if (!is.null(grid_df) && nrow(grid_df) > 0) {
      names_list <- apply(grid_df, 1, function(row) paste(c(base_name, row), collapse="_"))
      current_count <- length(names_list)
      indices <- (col_counter + 1):(col_counter + current_count)
      var_map[[base_name]] <<- setNames(indices, names_list)
      col_counter <<- col_counter + current_count
  } else {
      # Handle cases where an index set might be empty (e.g., P_not_gk if only GKs exist)
      var_map[[base_name]] <<- integer(0) # Assign empty named integer vector
  }
}


# Define variables and map them
add_vars("x", P_setofplayers, T_setofgameweeks)
add_vars("x_freehit", P_setofplayers, T_setofgameweeks)
add_vars("y", P_setofplayers, T_setofgameweeks)
add_vars("f", P_setofplayers, T_setofgameweeks)
add_vars("h", P_setofplayers, T_setofgameweeks)
add_vars("g", P_not_gk, T_setofgameweeks, L_substitution) # Only non-keepers
add_vars("u", P_setofplayers, T_setofgameweeks)
add_vars("e", P_setofplayers, T_setofgameweeks)
add_vars("w", T_setofgameweeks)
add_vars("is_tc", P_setofplayers, T_setofgameweeks) # Renamed 'c'
add_vars("b", T_setofgameweeks)
add_vars("r", T_setofgameweeks)
add_vars("lambda", P_setofplayers, T_setofgameweeks)
add_vars("v", T_setofgameweeks)
add_vars("q", T_setofgameweeks)
add_vars("alpha", T_setofgameweeks)

n_variables_total <- col_counter
print(paste("Total variables mapped:", n_variables_total))

# Helper function to get column index from variable name components
get_var_col <- function(base_name, id1 = NULL, id2 = NULL, id3 = NULL) {
  var_name <- ""
  if (!is.null(id3)) {
    var_name <- paste(c(base_name, id1, id2, id3), collapse="_")
  } else if (!is.null(id2)) {
    var_name <- paste(c(base_name, id1, id2), collapse="_")
  } else if (!is.null(id1)) {
     var_name <- paste(c(base_name, id1), collapse="_")
  } else {
      stop("Invalid variable specification in get_var_col")
  }
  # Check if the variable base name exists and has entries
  if(is.null(var_map[[base_name]]) || length(var_map[[base_name]]) == 0) {
      stop(paste("Variable base name not found or empty in var_map:", base_name))
  }
  col_idx <- var_map[[base_name]][var_name]
  # Handle cases where the specific combination might be missing (shouldn't happen with expand.grid)
  if (is.null(col_idx) || is.na(col_idx)) stop(paste("Variable combination not found:", var_name))
  return(unname(col_idx))
}


# Create lpSolveAPI model object
lprec <- make.lp(0, n_variables_total)
lp.control(lprec, sense="max")

# --- 3. Set Variable Types and Bounds ---
print("Setting variable types and bounds...")
# Combine all binary variable indices safely
binary_vars_list <- list(var_map$x, var_map$x_freehit, var_map$y, var_map$f, var_map$h, var_map$g,
                         var_map$u, var_map$e, var_map$w, var_map$is_tc, var_map$b, var_map$r, var_map$lambda)
binary_vars <- unlist(binary_vars_list[!sapply(binary_vars_list, is.null)])
if(length(binary_vars) > 0) set.type(lprec, columns = binary_vars, type = "binary")

# Integer variables
integer_vars_list <- list(var_map$q, var_map$alpha)
integer_vars <- unlist(integer_vars_list[!sapply(integer_vars_list, is.null)])
if(length(integer_vars) > 0) {
    set.type(lprec, columns = integer_vars, type = "integer")
    # Set bounds for integer vars - *** FIX HERE ***
    if(length(var_map$alpha)>0) {
        cols_alpha <- var_map$alpha
        set.bounds(lprec, lower = rep(0, length(cols_alpha)), columns = cols_alpha)
    }
    if(length(var_map$q)>0) {
        cols_q <- var_map$q
        set.bounds(lprec, lower = rep(0, length(cols_q)), upper = rep(Q_bar, length(cols_q)), columns = cols_q)
    }
}

# Continuous variables - *** FIX HERE ***
if(length(var_map$v)>0) {
    cols_v <- var_map$v
    set.bounds(lprec, lower = rep(0, length(cols_v)), columns = cols_v)
}

print("Variable types and bounds set.")


# --- 4. Set Objective Function ---
# ... (Objective function code remains the same) ...
print("Setting objective function...")
obj_coeffs <- numeric(n_variables_total)
safe_assign_obj <- function(base, id1=NULL, id2=NULL, id3=NULL, coeff) { tryCatch({ col <- get_var_col(base, id1, id2, id3); obj_coeffs[col] <<- obj_coeffs[col] + coeff }, error = function(e) {}) }
# Points from starting lineup (y)
for (p_ in p) { for (t_ in t) { safe_assign_obj("y", p_, t_, coeff=points_matrix[as.character(p_), as.character(t_)]) }}
# Points from captain (f)
for (p_ in p) { for (t_ in t) { safe_assign_obj("f", p_, t_, coeff=points_matrix[as.character(p_), as.character(t_)]) }}
# Points from vice-captain (h)
for (p_ in p) { for (t_ in t) { safe_assign_obj("h", p_, t_, coeff=epsilon * points_matrix[as.character(p_), as.character(t_)]) }}
# Points from triple captain (is_tc)
for (p_ in p) { for (t_ in t) { safe_assign_obj("is_tc", p_, t_, coeff=2 * points_matrix[as.character(p_), as.character(t_)]) }}
# Points from substitutes (g)
if(length(P_not_gk) > 0) { for (p_ngk_ in P_not_gk) { for (t_ in t) { for (l_ in l) { safe_assign_obj("g", p_ngk_, t_, l_, coeff=kappa[l_] * points_matrix[as.character(p_ngk_), as.character(t_)]) }}}}
# Penalty from transfers (alpha)
relevant_ts <- t[t >= min(t, na.rm=TRUE)+1]
if(length(relevant_ts) > 0) { for (t_ in relevant_ts) { safe_assign_obj("alpha", t_, coeff= -R) }}
set.objfn(lprec, obj_coeffs)
print("Objective function set.")

# --- 5. Add Constraints ---
print("Adding constraints...")

# *** FIX in safe_add_constraint: Use "=" for equality ***
safe_add_constraint <- function(xt, type, rhs, indices) {
    # Replace "==" with "=" for lpSolveAPI
    type_fixed <- ifelse(type == "==", "=", type)

    if (length(indices) > 0 && length(xt) == length(indices)) {
        add.constraint(lprec, xt = xt, type = type_fixed, rhs = rhs, indices = indices)
    } else if (length(indices) == 0 && rhs == 0 && type_fixed %in% c("=", ">=", "<=")) {
       # Skip trivially satisfied constraints
    } else {
        warning(paste("Skipping constraint due to mismatched lengths or empty indices. Indices:", length(indices), "Coeffs:", length(xt)))
    }
}

# Constraint 4.8 - Exactly MK Goalkeepers in squad x
if (length(Pgk) > 0) {
    print("Adding constraint 4.8 (MK GKs)...")
    for (t_ in t) {
        vars_indices <- sapply(Pgk, get_var_col, base_name = "x", id2 = t_)
        safe_add_constraint(xt = rep(1, length(vars_indices)), type = "==", rhs = MK, indices = vars_indices)
    }
} else { print("Skipping constraint 4.8 (No GKs defined).") }

# Constraints 4.9, 4.10, 4.11 similarly...
if (length(Pdef)>0) { print("Adding constraint 4.9 (MD DEFs)..."); for (t_ in t) { vars_indices <- sapply(Pdef, get_var_col, base_name = "x", id2 = t_); safe_add_constraint(rep(1, length(vars_indices)), "==", MD, vars_indices)} } else { print("Skipping constraint 4.9 (No DEFs).") }
if (length(Pmid)>0) { print("Adding constraint 4.10 (MM MIDs)..."); for (t_ in t) { vars_indices <- sapply(Pmid, get_var_col, base_name = "x", id2 = t_); safe_add_constraint(rep(1, length(vars_indices)), "==", MM, vars_indices)} } else { print("Skipping constraint 4.10 (No MIDs).") }
if (length(Pfwd)>0) { print("Adding constraint 4.11 (MF FWDs)..."); for (t_ in t) { vars_indices <- sapply(Pfwd, get_var_col, base_name = "x", id2 = t_); safe_add_constraint(rep(1, length(vars_indices)), "==", MF, vars_indices)} } else { print("Skipping constraint 4.11 (No FWDs).") }


# Constraint 4.12 - Max MC players per team c in squad x
print("Adding constraint 4.12 (Max players per team)...")
for (t_ in t) {
    for (c_ in C_setofteams) {
        team_players <- P_c[[c_]]
        team_players_in_model <- intersect(team_players, P_setofplayers)
        if (length(team_players_in_model) > 0) {
            vars_indices <- sapply(team_players_in_model, get_var_col, base_name = "x", id2 = t_)
            safe_add_constraint(xt = rep(1, length(vars_indices)), type = "<=", rhs = MC, indices = vars_indices)
        }
    }
}

# Constraint 4.33 - Initial Budget
print("Adding constraint 4.33 (Initial Budget)...")
if (length(p) > 0 && length(t) > 0) {
    t1 <- min(t)
    # Check if player IDs 'p' actually exist as rownames in value_matrix
    valid_p_for_matrix <- intersect(p, as.numeric(rownames(value_matrix)))
    if(length(valid_p_for_matrix) > 0) {
        vars_indices_x <- sapply(valid_p_for_matrix, get_var_col, base_name = "x", id2 = t1)
        vars_indices_v <- get_var_col("v", t1)
        coeffs_x <- value_matrix[as.character(valid_p_for_matrix), as.character(t1)]
        safe_add_constraint(xt = c(coeffs_x, 1), type = "==", rhs = BS, indices = c(vars_indices_x, vars_indices_v))
    } else { warning("No valid players found for initial budget constraint.")}
}

# Constraint 4.35 - Squad Continuity (for t > 1)
print("Adding constraint 4.35 (Squad Continuity)...")
if (n_T > 1) {
    for (gw_idx in 2:n_T) {
        gw <- t[gw_idx]
        gw_prev <- t[gw_idx-1]
        for(p_ in p) {
             # Use tryCatch to handle potential errors if a specific player/gw combo is missing a variable index
             tryCatch({
                 idx_x_t     <- get_var_col("x", p_, gw)
                 idx_x_t_1   <- get_var_col("x", p_, gw_prev)
                 idx_u_t     <- get_var_col("u", p_, gw)
                 idx_e_t     <- get_var_col("e", p_, gw)
                 # Constraint: x[p, t] - x[p, t-1] + u[p, t] - e[p, t] == 0
                 safe_add_constraint(xt = c(1, -1, 1, -1), type = "==", rhs = 0, indices = c(idx_x_t, idx_x_t_1, idx_u_t, idx_e_t))
             }, error = function(e) {
                 warning(paste("Skipping continuity constraint for p=", p_, ", gw=", gw, " due to error: ", e$message))
             })
        }
    }
}

# --- !!! Add ALL other constraints from the thesis similarly !!! ---
# ... This remains the major task ...
print("Constraint addition section (incomplete - requires full model translation).")


# --- 6. Solve ---
print("Solving model...")
status_code <- solve(lprec)
print(paste("Solver status code:", status_code))

# --- 7. Extract Results ---
print("Extracting results...")
if (status_code == 0) {
    obj_value <- get.objective(lprec)
    print(paste("Objective value:", obj_value))

    if (abs(obj_value) < 1e-6 && n_variables_total > 0) {
         print("Objective is zero or near-zero. Check model constraints and variable definitions.")
    }

    var_values <- get.variables(lprec)

    # Map results back (example for squad GW1)
    if (!is.null(var_map$x) && length(var_map$x) > 0) {
        # Ensure p used here matches the players for whom variables were created
        valid_p_vars <- p[p %in% as.numeric(names(pos_map))] # Players present in cleaned data
        if(length(valid_p_vars) > 0) {
            squad_gw1_indices <- sapply(valid_p_vars, get_var_col, base_name = "x", id2 = min(t))
            squad_gw1_values <- var_values[squad_gw1_indices]
            selected_players_gw1 <- valid_p_vars[squad_gw1_values > 0.9]

            print("Optimal Squad for GW1 (Player IDs):")
            print(selected_players_gw1)
        } else {print("No valid players to extract squad for.")}
    } else { print("No 'x' variables defined.")}


    # Example: Free transfers for GW2
    if (n_T > 1 && !is.null(var_map$q) && length(var_map$q) > 0) {
        q_gw2_idx <- get_var_col("q", t[2])
        q_gw2_val <- var_values[q_gw2_idx]
        print(paste("Free Transfers available for GW2 (q_2):", round(q_gw2_val)))
    }

} else {
    print(paste("Model did not solve to optimality. Status code:", status_code))
    # Consult lpSolve status codes: https://lpsolve.sourceforge.net/5.5/solve.htm
}

# Clean up (optional)
# delete.lp(lprec)