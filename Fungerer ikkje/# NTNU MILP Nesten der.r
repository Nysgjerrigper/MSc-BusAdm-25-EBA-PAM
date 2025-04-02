# NTNU MILP Nesten der
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
  id_col_sym <- rlang::ensym(id_col)
  name_col_sym <- rlang::ensym(name_col)
  value_col_sym <- rlang::ensym(value_col)

  data_unique <- data_long %>%
    group_by(!!id_col_sym, !!name_col_sym) %>%
    # Take the first non-NA value if multiple exist per player/gw
    summarise(value = first(stats::na.omit(!!value_col_sym)), .groups = 'drop')

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
     subset_mat <- matrix_numeric[common_rows, common_cols, drop = FALSE]
     if(is.list(subset_mat)) { # Should not happen after summarise
        subset_mat <- apply(subset_mat, c(1,2), function(x) ifelse(is.null(x[[1]]), default_value, x[[1]]))
     }
     final_matrix[common_rows, common_cols] <- subset_mat
  }

  # Final NA check/fill
  final_matrix[is.na(final_matrix)] <- default_value

  # Convert matrix elements explicitly to numeric
  if(!is.numeric(final_matrix)) {
      storage.mode(final_matrix) <- "numeric" # More direct conversion
  }

  # Final type check
  stopifnot("Matrix is not numeric"=is.numeric(final_matrix))
  stopifnot("Matrix has wrong number of rows"=nrow(final_matrix) == length(row_set_char))
  stopifnot("Matrix has wrong number of columns"=ncol(final_matrix) == length(col_set_char))
  stopifnot("Matrix rownames mismatch or wrong order"=all(rownames(final_matrix) == row_set_char))
  stopifnot("Matrix colnames mismatch or wrong order"=all(colnames(final_matrix) == col_set_char))

  return(final_matrix)
}


# --- 1. Load and Prepare Data ---
print("Loading and preparing data...")
allesesonger <- read_csv("Differensiert gw alle tre sesonger(22-24), heltall.csv", show_col_types = FALSE)
data <- allesesonger |> filter(GW <= 2) # Using GW <= 2 for demonstration

# Define Sets
T_setofgameweeks <- sort(unique(data$GW))
P_setofplayers <- unique(data$player_id)
data <- data %>% mutate(team = trimws(team))
C_setofteams <- sort(unique(data$team[!is.na(data$team) & data$team != ""]))
L_substitution <- 1:3
n_T <- length(T_setofgameweeks)
n_P <- length(P_setofplayers)
n_C <- length(C_setofteams)
n_L <- length(L_substitution)

# Data Validation and Cleaning
data_complete <- data %>%
  filter(player_id %in% P_setofplayers) %>%
  tidyr::complete(player_id, GW = T_setofgameweeks) %>%
  group_by(player_id) %>%
  mutate(
      position = na.omit(position)[1], # Take first non-NA pos/team for player
      team = na.omit(team)[1]
      ) %>%
  ungroup() %>%
  mutate(
    total_points = replace_na(total_points, 0),
    value = replace_na(value, 50)
  ) %>%
  filter(!is.na(team), team %in% C_setofteams) # Ensure team is valid

# --- (Previous code up to data_complete) ---

# Handle Duplicates (using summarise to be safer and keep name)
data_cleaned <- data_complete %>%
    group_by(player_id, GW) %>%
    summarise(
        # *** ADD name HERE ***
        name = first(na.omit(name)),
        position = first(na.omit(position)),
        team = first(na.omit(team)),
        total_points = first(total_points),
        value = first(value),
        .groups = 'drop'
        ) %>%
     filter(player_id %in% P_setofplayers)

# Check if any players were lost
# ... (rest of player check code) ...
data <- data_cleaned # Use the cleaned data with the name column included
# --- End Data Validation ---

# Ensure player set only contains players actually in cleaned data
P_setofplayers <- sort(unique(data_cleaned$player_id))
n_P <- length(P_setofplayers)
data <- data_cleaned
print(paste("Using", n_P, "players and", n_T, "gameweeks."))
# --- End Data Validation ---


# Define Subsets (using the cleaned 'data')
pos_map <- setNames(data$position, data$player_id)
team_map <- setNames(data$team, data$player_id)
Pdef <- P_setofplayers[pos_map[as.character(P_setofplayers)] == "DEF"]
Pmid <- P_setofplayers[pos_map[as.character(P_setofplayers)] == "MID"]
Pfwd <- P_setofplayers[pos_map[as.character(P_setofplayers)] == "FWD"]
Pgk <- P_setofplayers[pos_map[as.character(P_setofplayers)] == "GK"]
P_not_gk <- setdiff(P_setofplayers, Pgk)
n_P_not_gk <- length(P_not_gk)
P_c <- split(data$player_id, data$team)
P_c <- P_c[names(P_c) %in% C_setofteams & sapply(P_c, length) > 0]
C_setofteams <- names(P_c)
n_C <- length(C_setofteams)

# Define GW subsets
medianavgameweeks <- ifelse(n_T > 0, median(T_setofgameweeks), 0)
T_FH <- T_setofgameweeks[T_setofgameweeks <= medianavgameweeks]
T_SH <- T_setofgameweeks[T_setofgameweeks > medianavgameweeks]

# Define Indices
t <- T_setofgameweeks
p <- P_setofplayers
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
# (add_vars and get_var_col functions defined as before)
var_map <- list()
col_counter <- 0
add_vars <- function(base_name, idx1 = NULL, idx2 = NULL, idx3 = NULL) { grid_df <- NULL; if (!is.null(idx3)) { if(length(idx1)>0 && length(idx2)>0 && length(idx3)>0) grid_df <- expand.grid(idx1, idx2, idx3) } else if (!is.null(idx2)) { if(length(idx1)>0 && length(idx2)>0) grid_df <- expand.grid(idx1, idx2) } else if (!is.null(idx1)) { if(length(idx1)>0) grid_df <- data.frame(idx1) }; if (!is.null(grid_df) && nrow(grid_df) > 0) { names_list <- apply(grid_df, 1, function(row) paste(c(base_name, row), collapse="_")); current_count <- length(names_list); indices <- (col_counter + 1):(col_counter + current_count); var_map[[base_name]] <<- setNames(indices, names_list); col_counter <<- col_counter + current_count } else { var_map[[base_name]] <<- integer(0) } }
get_var_col <- function(base_name, id1 = NULL, id2 = NULL, id3 = NULL) { var_name <- ""; if (!is.null(id3)) { var_name <- paste(c(base_name, id1, id2, id3), collapse="_") } else if (!is.null(id2)) { var_name <- paste(c(base_name, id1, id2), collapse="_") } else if (!is.null(id1)) { var_name <- paste(c(base_name, id1), collapse="_") } else { stop("Invalid variable specification in get_var_col") }; if(is.null(var_map[[base_name]]) || length(var_map[[base_name]]) == 0) { stop(paste("Variable base name not found or empty in var_map:", base_name)) }; col_idx <- var_map[[base_name]][var_name]; if (is.null(col_idx) || is.na(col_idx)) stop(paste("Variable combination not found:", var_name)); return(unname(col_idx)) }

# Define variables and map them
add_vars("x", p, t); add_vars("x_freehit", p, t); add_vars("y", p, t); add_vars("f", p, t); add_vars("h", p, t); add_vars("g", P_not_gk, t, l); add_vars("u", p, t); add_vars("e", p, t); add_vars("w", t); add_vars("is_tc", p, t); add_vars("b", t); add_vars("r", t); add_vars("lambda", p, t); add_vars("v", t); add_vars("q", t); add_vars("alpha", t)
n_variables_total <- col_counter
print(paste("Total variables mapped:", n_variables_total))

# Create lpSolveAPI model object
lprec <- make.lp(0, n_variables_total)
lp.control(lprec, sense="max")

# --- 3. Set Variable Types and Bounds ---
print("Setting variable types and bounds...")
binary_vars_list <- list(var_map$x, var_map$x_freehit, var_map$y, var_map$f, var_map$h, var_map$g, var_map$u, var_map$e, var_map$w, var_map$is_tc, var_map$b, var_map$r, var_map$lambda); binary_vars <- unlist(binary_vars_list[!sapply(binary_vars_list, is.null)]); if(length(binary_vars) > 0) set.type(lprec, columns = binary_vars, type = "binary")
integer_vars_list <- list(var_map$q, var_map$alpha); integer_vars <- unlist(integer_vars_list[!sapply(integer_vars_list, is.null)]); if(length(integer_vars) > 0) { set.type(lprec, columns = integer_vars, type = "integer"); if(length(var_map$alpha)>0) { cols_alpha <- var_map$alpha; set.bounds(lprec, lower = rep(0, length(cols_alpha)), columns = cols_alpha) }; if(length(var_map$q)>0) { cols_q <- var_map$q; set.bounds(lprec, lower = rep(0, length(cols_q)), upper = rep(Q_bar, length(cols_q)), columns = cols_q) }}
if(length(var_map$v)>0) { cols_v <- var_map$v; set.bounds(lprec, lower = rep(0, length(cols_v)), columns = cols_v) }
print("Variable types and bounds set.")

# --- 4. Set Objective Function ---
print("Setting objective function...")
obj_coeffs <- numeric(n_variables_total); safe_assign_obj <- function(base, id1=NULL, id2=NULL, id3=NULL, coeff) { tryCatch({ col <- get_var_col(base, id1, id2, id3); obj_coeffs[col] <<- obj_coeffs[col] + coeff }, error = function(e) {}) }; for (p_ in p) { for (t_ in t) { safe_assign_obj("y", p_, t_, coeff=points_matrix[as.character(p_), as.character(t_)]) }}; for (p_ in p) { for (t_ in t) { safe_assign_obj("f", p_, t_, coeff=points_matrix[as.character(p_), as.character(t_)]) }}; for (p_ in p) { for (t_ in t) { safe_assign_obj("h", p_, t_, coeff=epsilon * points_matrix[as.character(p_), as.character(t_)]) }}; for (p_ in p) { for (t_ in t) { safe_assign_obj("is_tc", p_, t_, coeff=2 * points_matrix[as.character(p_), as.character(t_)]) }}; if(length(P_not_gk) > 0) { for (p_ngk_ in P_not_gk) { for (t_ in t) { for (l_ in l) { safe_assign_obj("g", p_ngk_, t_, l_, coeff=kappa[l_] * points_matrix[as.character(p_ngk_), as.character(t_)]) }}}}; relevant_ts <- t[t >= min(t, na.rm=TRUE)+1]; if(length(relevant_ts) > 0) { for (t_ in relevant_ts) { safe_assign_obj("alpha", t_, coeff= -R) }}; set.objfn(lprec, obj_coeffs)
print("Objective function set.")

# --- 5. Add ALL Constraints ---
print("Adding constraints...")
constraint_count <- 0 # Keep track

safe_add_constraint <- function(xt, type, rhs, indices) {
    type_fixed <- ifelse(type == "==", "=", type)
    if (length(indices) > 0 && length(xt) == length(indices)) {
        add.constraint(lprec, xt = xt, type = type_fixed, rhs = rhs, indices = indices)
        constraint_count <<- constraint_count + 1 # Increment counter
    } else if (length(indices) == 0 && type_fixed %in% c("=", ">=", "<=") && abs(rhs) < 1e-9) {
       # Skip 0=0, 0>=0, 0<=0
    } else {
        warning(paste("Skipping constraint due to mismatched lengths or empty indices. Indices:", length(indices), "Coeffs:", length(xt), "Type:", type, "RHS:", rhs))
    }
}

# -- Gamechips (4.2 - 4.7) --
print("Adding Gamechip constraints (4.2-4.7)...")
if(length(T_FH) > 0) safe_add_constraint(rep(1, length(T_FH)), "<=", 1, sapply(T_FH, get_var_col, base_name="w")) # 4.2
if(length(T_SH) > 0) safe_add_constraint(rep(1, length(T_SH)), "<=", 1, sapply(T_SH, get_var_col, base_name="w")) # 4.3
idx_is_tc <- var_map$is_tc; if(length(idx_is_tc)>0) safe_add_constraint(rep(1, length(idx_is_tc)), "<=", 1, idx_is_tc) # 4.4
idx_b <- var_map$b;       if(length(idx_b)>0) safe_add_constraint(rep(1, length(idx_b)), "<=", 1, idx_b) # 4.5
idx_r <- var_map$r;       if(length(idx_r)>0) safe_add_constraint(rep(1, length(idx_r)), "<=", 1, idx_r) # 4.6
for(t_ in t) { # 4.7
    idx_w_t = get_var_col("w", t_)
    idx_is_tc_t = sapply(p, get_var_col, base_name="is_tc", id2 = t_)
    idx_b_t = get_var_col("b", t_)
    idx_r_t = get_var_col("r", t_)
    safe_add_constraint(c(1, rep(1, length(idx_is_tc_t)), 1, 1), "<=", 1, c(idx_w_t, idx_is_tc_t, idx_b_t, idx_r_t))
}

# -- Selected Squad (4.8 - 4.12) --
print("Adding Selected Squad constraints (4.8-4.12)...")
if (length(Pgk)>0)  { for (t_ in t) { idx <- sapply(Pgk, get_var_col, "x", t_); safe_add_constraint(rep(1,length(idx)), "=", MK, idx)} }
if (length(Pdef)>0) { for (t_ in t) { idx <- sapply(Pdef,get_var_col, "x", t_); safe_add_constraint(rep(1,length(idx)), "=", MD, idx)} }
if (length(Pmid)>0) { for (t_ in t) { idx <- sapply(Pmid,get_var_col, "x", t_); safe_add_constraint(rep(1,length(idx)), "=", MM, idx)} }
if (length(Pfwd)>0) { for (t_ in t) { idx <- sapply(Pfwd,get_var_col, "x", t_); safe_add_constraint(rep(1,length(idx)), "=", MF, idx)} }
for (t_ in t) { for (c_ in C_setofteams) { p_team <- intersect(P_c[[c_]], p); if(length(p_team)>0){ idx <- sapply(p_team, get_var_col, "x", t_); safe_add_constraint(rep(1,length(idx)), "<=", MC, idx)}}}

# -- Free Hit Squad (4.13 - 4.17) --
print("Adding Free Hit Squad constraints (4.13-4.17)...")
for(t_ in t){
    idx_r_t = get_var_col("r", t_)
    if(length(Pgk)>0) { idx_gk <- sapply(Pgk, get_var_col, "x_freehit", t_); safe_add_constraint(c(rep(1,length(idx_gk)), -MK), "=", 0, c(idx_gk, idx_r_t)) } # 4.13
    if(length(Pdef)>0){ idx_def<- sapply(Pdef,get_var_col, "x_freehit", t_); safe_add_constraint(c(rep(1,length(idx_def)),-MD), "=", 0, c(idx_def,idx_r_t)) } # 4.14
    if(length(Pmid)>0){ idx_mid<- sapply(Pmid,get_var_col, "x_freehit", t_); safe_add_constraint(c(rep(1,length(idx_mid)),-MM), "=", 0, c(idx_mid,idx_r_t)) } # 4.15
    if(length(Pfwd)>0){ idx_fwd<- sapply(Pfwd,get_var_col, "x_freehit", t_); safe_add_constraint(c(rep(1,length(idx_fwd)),-MF), "=", 0, c(idx_fwd,idx_r_t)) } # 4.16
    for (c_ in C_setofteams) { # 4.17
        p_team <- intersect(P_c[[c_]], p);
        if(length(p_team)>0){
             idx_team <- sapply(p_team, get_var_col, "x_freehit", t_)
             safe_add_constraint(c(rep(1, length(idx_team)), -MC), "<=", 0, c(idx_team, idx_r_t))
        }
    }
}

# -- Starting Line-up (4.18 - 4.26) --
print("Adding Starting Line-up constraints (4.18-4.26)...")
for(t_ in t){
    idx_y_all = sapply(p, get_var_col, "y", t_)
    idx_b_t = get_var_col("b", t_)
    idx_r_t = get_var_col("r", t_)
    safe_add_constraint(c(rep(1, length(idx_y_all)), -phi), "=", E, c(idx_y_all, idx_b_t)) # 4.18
    if(length(Pgk)>0) {idx_y_gk = sapply(Pgk, get_var_col, "y", t_); safe_add_constraint(c(rep(1,length(idx_y_gk)), -phi_K), "=", EK, c(idx_y_gk, idx_b_t))} # 4.19
    if(length(Pdef)>0){idx_y_def= sapply(Pdef,get_var_col, "y", t_); safe_add_constraint(rep(1,length(idx_y_def)), ">=", ED, idx_y_def)} # 4.20
    if(length(Pmid)>0){idx_y_mid= sapply(Pmid,get_var_col, "y", t_); safe_add_constraint(rep(1,length(idx_y_mid)), ">=", EM, idx_y_mid)} # 4.21
    if(length(Pfwd)>0){idx_y_fwd= sapply(Pfwd,get_var_col, "y", t_); safe_add_constraint(rep(1,length(idx_y_fwd)), ">=", EF, idx_y_fwd)} # 4.22
    for(p_ in p){
        idx_y_pt = get_var_col("y", p_, t_)
        idx_xh_pt= get_var_col("x_freehit", p_, t_)
        idx_l_pt = get_var_col("lambda", p_, t_)
        idx_x_pt = get_var_col("x", p_, t_)
        safe_add_constraint(c(1, -1, -1), "<=", 0, c(idx_y_pt, idx_xh_pt, idx_l_pt)) # 4.24: y - xh - lambda <= 0
        safe_add_constraint(c(1, -1), "<=", 0, c(idx_l_pt, idx_x_pt))             # 4.25: lambda - x <= 0
        safe_add_constraint(c(1, 1), "<=", 1, c(idx_l_pt, idx_r_t))              # 4.26: lambda + r <= 1
    }
}

# -- Captain/Vice (4.27 - 4.29) --
print("Adding Captain/Vice constraints (4.27-4.29)...")
for(t_ in t){
    idx_f_t = sapply(p, get_var_col, "f", t_)
    idx_tc_t= sapply(p, get_var_col, "is_tc", t_)
    idx_h_t = sapply(p, get_var_col, "h", t_)
    safe_add_constraint(c(rep(1, length(idx_f_t)), rep(1, length(idx_tc_t))), "=", 1, c(idx_f_t, idx_tc_t)) # 4.27
    safe_add_constraint(rep(1, length(idx_h_t)), "=", 1, idx_h_t) # 4.28
    for(p_ in p){ # 4.29
        idx_f_pt = get_var_col("f", p_, t_)
        idx_tc_pt= get_var_col("is_tc", p_, t_)
        idx_h_pt = get_var_col("h", p_, t_)
        idx_y_pt = get_var_col("y", p_, t_)
        safe_add_constraint(c(1, 1, 1, -1), "<=", 0, c(idx_f_pt, idx_tc_pt, idx_h_pt, idx_y_pt))
    }
}

# -- Substitution (4.30 - 4.32) --
print("Adding Substitution constraints (4.30-4.32)...")
if(length(P_not_gk) > 0 && length(l) > 0){ # Check if there are non-GKs and sub priorities
    for(t_ in t){
        idx_r_t = get_var_col("r", t_)
        for(p_ngk_ in P_not_gk){
            idx_y_pt = get_var_col("y", p_ngk_, t_)
            idx_g_ptl= sapply(l, get_var_col, base_name="g", id1=p_ngk_, id2=t_)
            idx_x_pt = get_var_col("x", p_ngk_, t_)
            idx_xh_pt= get_var_col("x_freehit", p_ngk_, t_)
            # 4.30: y + sum(g) - x - beta*r <= 0
            safe_add_constraint(c(1, rep(1, length(idx_g_ptl)), -1, -beta), "<=", 0, c(idx_y_pt, idx_g_ptl, idx_x_pt, idx_r_t))
            # 4.31: y + sum(g) - xh - beta*(1-r) <= 0 => y + sum(g) - xh + beta*r <= beta
            safe_add_constraint(c(1, rep(1, length(idx_g_ptl)), -1, beta), "<=", beta, c(idx_y_pt, idx_g_ptl, idx_xh_pt, idx_r_t))
        }
        for(l_ in l){ # 4.32
            idx_g_tl = sapply(P_not_gk, get_var_col, base_name="g", id2=t_, id3=l_)
            safe_add_constraint(rep(1, length(idx_g_tl)), "<=", 1, idx_g_tl)
        }
    }
} else {print("Skipping Substitution constraints (no non-GKs or no sub levels).")}


# -- Budget (4.33 - 4.39) --
# 4.33 already added
print("Adding Budget constraints (4.34, 4.36-4.39)...")
for(t_ in t) { # 4.36: Applies for all t
    for(p_ in p){
        idx_e_pt = get_var_col("e", p_, t_)
        idx_u_pt = get_var_col("u", p_, t_)
        safe_add_constraint(c(1, 1), "<=", 1, c(idx_e_pt, idx_u_pt))
    }
}
if (n_T > 1) {
    for (gw_idx in 2:n_T) {
        gw <- t[gw_idx]
        gw_prev <- t[gw_idx-1]
        idx_v_t = get_var_col("v", gw)
        idx_v_t_1=get_var_col("v", gw_prev)
        idx_u_pt = sapply(p, get_var_col, base_name="u", id2 = gw)
        idx_e_pt = sapply(p, get_var_col, base_name="e", id2 = gw)
        coeffs_u = value_matrix[as.character(p), as.character(gw)]
        coeffs_e = value_matrix[as.character(p), as.character(gw)]
        # 4.34: v[t] - v[t-1] - sum(val*u) + sum(val*e) = 0
        safe_add_constraint(c(1, -1, -coeffs_u, coeffs_e), "=", 0, c(idx_v_t, idx_v_t_1, idx_u_pt, idx_e_pt))

        idx_xh_pt = sapply(p, get_var_col, base_name="x_freehit", id2=gw)
        idx_x_pt_1= sapply(p, get_var_col, base_name="x", id2=gw_prev)
        coeffs_xh = value_matrix[as.character(p), as.character(gw)]
        coeffs_x_1= value_matrix[as.character(p), as.character(gw_prev)]
        # 4.37: sum(val*xh) - sum(val_1*x_1) - v_1 <= 0
        safe_add_constraint(c(coeffs_xh, -coeffs_x_1, -1), "<=", 0, c(idx_xh_pt, idx_x_pt_1, idx_v_t_1))

        idx_r_t = get_var_col("r", gw)
        # 4.38: sum(u) + beta*r <= beta
        safe_add_constraint(c(rep(1, length(idx_u_pt)), beta), "<=", beta, c(idx_u_pt, idx_r_t))
        # 4.39: sum(e) + beta*r <= beta
        safe_add_constraint(c(rep(1, length(idx_e_pt)), beta), "<=", beta, c(idx_e_pt, idx_r_t))
    }
}

# -- Transfers (4.40 - 4.44 + alpha calc) --
print("Adding Transfer constraints (4.40-4.44)...")
if (n_T > 1) {
    idx_q_t2 = get_var_col("q", t[2])
    safe_add_constraint(1, "=", Q_under_bar, idx_q_t2) # 4.40: q[2] = Q_

    for (gw_idx in 2:n_T) { # Constraints relating t (gw) and t+1 (next_gw)
        gw = t[gw_idx]
        idx_alpha_t = get_var_col("alpha", gw)
        idx_q_t     = get_var_col("q", gw)
        idx_e_pt    = sapply(p, get_var_col, "e", gw)
        idx_w_t     = get_var_col("w", gw)
        idx_r_t     = get_var_col("r", gw)

        # Alpha calculation: alpha[t] >= sum(e[p,t]) - q[t] => alpha[t] - sum(e) + q[t] >= 0
        safe_add_constraint(c(1, rep(-1, length(idx_e_pt)), 1), ">=", 0, c(idx_alpha_t, idx_e_pt, idx_q_t))
        # Alpha WC interaction: alpha[t] + beta*w[t] <= beta
        safe_add_constraint(c(1, beta), "<=", beta, c(idx_alpha_t, idx_w_t))

        if(gw_idx < n_T) { # Constraints involving t+1 only up to second-to-last t
            next_gw = t[gw_idx+1]
            idx_q_t_1 = get_var_col("q", next_gw)

            # 4.41: q[t+1] - alpha[t] - q[t] + sum(e) - beta*w[t] <= Q_
            safe_add_constraint(c(1, -1, -1, rep(1, length(idx_e_pt)), -beta), "<=", Q_under_bar,
                                c(idx_q_t_1, idx_alpha_t, idx_q_t, idx_e_pt, idx_w_t))

            # 4.42: alpha[t] + alpha_bar*q[t+1] <= alpha_bar * Q_bar
            safe_add_constraint(c(1, alpha_bar), "<=", alpha_bar * Q_bar, c(idx_alpha_t, idx_q_t_1))

            # 4.43: q[t+1] >= Q_
            safe_add_constraint(1, ">=", Q_under_bar, idx_q_t_1)

             # 4.44: q[t+1] - (Q_ - Q_bar)*w[t] - (Q_ - Q_bar)*r[t] <= Q_bar
            safe_add_constraint(c(1, -(Q_under_bar - Q_bar), -(Q_under_bar - Q_bar)), "<=", Q_bar,
                                c(idx_q_t_1, idx_w_t, idx_r_t))
        }
    }
}

print(paste("Finished adding constraints. Total constraints:", constraint_count))

# --- 6. Solve ---
print("Solving model...")
# Optional: Write LP file for debugging
# write.lp(lprec, "fpl_model_full.lp")
# print("LP file written to fpl_model_full.lp")

status_code <- solve(lprec)
print(paste("Solver status code:", status_code)) # 0 typically means optimal

# --- 7. Extract Results ---
print("Extracting results...")
if (status_code == 0) {
    obj_value <- get.objective(lprec)
    print(paste("Objective value:", obj_value))

    if (abs(obj_value) < 1e-6 && n_variables_total > 0) {
         print("Objective is zero or near-zero. Check model constraints and objective.")
    }

    var_values <- get.variables(lprec)

    # Create Player Name Lookup
    player_name_lookup <- data_cleaned %>%
        select(player_id, name) %>%
        distinct(player_id, .keep_all = TRUE)

    # Function to get names for selected players
    get_player_names <- function(var_base, gw) {
         if (is.null(var_map[[var_base]]) || length(var_map[[var_base]]) == 0) return(tibble(player_id=numeric(), name=character()))
         valid_p_vars <- p[p %in% as.numeric(names(pos_map))]
         if(length(valid_p_vars) == 0) return(tibble(player_id=numeric(), name=character()))

         indices <- sapply(valid_p_vars, get_var_col, base_name = var_base, id2 = gw)
         values <- var_values[indices]
         selected_ids <- valid_p_vars[values > 0.9]

         if(length(selected_ids) > 0) {
             return(tibble(player_id = selected_ids) %>% left_join(player_name_lookup, by = "player_id"))
         } else {
             return(tibble(player_id=numeric(), name=character()))
         }
    }

    # Extract and print results with names
    squad_gw1_df <- get_player_names("x", min(t))
    print("Optimal Squad for GW1:"); print(squad_gw1_df)

    starting_gw1_df <- get_player_names("y", min(t))
    print("Starting XI for GW1:"); print(starting_gw1_df)

    captain_gw1_df <- get_player_names("f", min(t))
    if(nrow(captain_gw1_df)==1) print(paste("Captain GW1:", captain_gw1_df$name, "(ID:", captain_gw1_df$player_id, ")"))

    tc_gw1_df <- get_player_names("is_tc", min(t))
    if(nrow(tc_gw1_df)==1) print(paste("Triple Captain GW1:", tc_gw1_df$name, "(ID:", tc_gw1_df$player_id, ")"))

    vc_gw1_df <- get_player_names("h", min(t))
    if(nrow(vc_gw1_df)==1) print(paste("Vice-Captain GW1:", vc_gw1_df$name, "(ID:", vc_gw1_df$player_id, ")"))

    # Transfers for GW2
    if (n_T > 1) {
        transfers_in_gw2_df <- get_player_names("e", t[2])
        if(nrow(transfers_in_gw2_df)>0) { print("Transfers IN for GW2:"); print(transfers_in_gw2_df) }

        transfers_out_gw2_df <- get_player_names("u", t[2])
        if(nrow(transfers_out_gw2_df)>0) { print("Transfers OUT for GW2:"); print(transfers_out_gw2_df) }

        if (!is.null(var_map$q) && length(var_map$q) > 0) {
            q_gw2_idx <- get_var_col("q", t[2])
            q_gw2_val <- var_values[q_gw2_idx]
            print(paste("Free Transfers available for GW2 (q_2):", round(q_gw2_val)))
        }
         if (!is.null(var_map$alpha) && length(var_map$alpha) > 0) {
            alpha_gw2_idx <- get_var_col("alpha", t[2])
            alpha_gw2_val <- var_values[alpha_gw2_idx]
            print(paste("Paid Transfers (alpha) for GW2:", round(alpha_gw2_val)))
         }
    }

} else {
    print(paste("Model did not solve to optimality. Status code:", status_code))
}

# Clean up
# delete.lp(lprec)