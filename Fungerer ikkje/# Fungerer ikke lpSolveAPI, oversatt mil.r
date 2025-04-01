# Fungerer ikke lpSolveAPI, oversatt milp

# Pakker
# install.packages("lpSolveAPI") # Run once if needed
library(lpSolveAPI)
library(tidyverse)

# --- Data Setup (Copied from previous attempts) ---
rm(list = ls(all = TRUE))
allesesonger <- read_csv("Differensiert gw alle tre sesonger(22-24), heltall.csv")
# Using 2 GWs for feasibility
data <- allesesonger |> filter(GW <= 2)

# --- Sets (Copied from previous attempts) ---
T_setofgameweeks <- unique(data$GW) # Gameweeks (e.g., 1, 2)
P_setofplayers <- unique(data$player_id) # Players
C_setofteams <- unique(data$team) # Teams
L_substitution <- 1:3 # Substitution priorities

# Subsets (Copied from previous attempts)
Pdef <- unique(data$player_id[data$position == "DEF"])
Pmid <- unique(data$player_id[data$position == "MID"])
Pfwd <- unique(data$player_id[data$position == "FWD"])
Pgk <- unique(data$player_id[data$position == "GK"])
P_not_gk <- unique(data$player_id[data$position != "GK"])
P_c <- split(data$player_id, data$team)

# Recalculate FH/SH based on actual values present
medianavgameweeks <- median(T_setofgameweeks)
T_FH <- T_setofgameweeks[T_setofgameweeks <= medianavgameweeks]
T_SH <- T_setofgameweeks[T_setofgameweeks > medianavgameweeks]
if (length(T_setofgameweeks) <= 1) {
    T_FH <- T_setofgameweeks
    T_SH <- integer(0)
} else if (length(T_FH) == 0) {
     T_FH <- min(T_setofgameweeks)
     T_SH <- T_setofgameweeks[T_setofgameweeks > min(T_setofgameweeks)]
} else if (length(T_SH) == 0 && length(T_FH) > 0) { # Ensure SH gets the last element if median splits unevenly
     T_SH <- max(T_setofgameweeks)
     T_FH <- T_setofgameweeks[T_setofgameweeks < max(T_setofgameweeks)]
}


# --- Parameters (Copied from previous attempts) ---
R <- 4              # Transfer penalty
MK <- 2              # GKs in squad
MD <- 5              # DEFs in squad
MM <- 5              # MIDs in squad
MF <- 3              # FWDs in squad
MC <- 3              # Max players per team
E <- 11              # Starting XI size
EK <- 1              # Starting GK
ED <- 3              # Min starting DEF
EM <- 3              # Min starting MID
EF <- 1              # Min starting FWD
BS <- 100.0 * 10      # Start budget (scaled)
phi <- (MK+MD+MM+MF) - E # Number of subs = 15 - 11 = 4
phi_K <- MK - EK       # Number of GK subs = 1
Q_bar <- 2             # Max free transfers (FPL rule: max is 2)
Q_under_bar <- 1       # Free transfers per gameweek
epsilon <- 0.1         # Vice-captain weight
kappa <- c(0.01, 0.005, 0.001) # Sub priority weights
beta <- length(P_setofplayers) # Big M for substitutions
alpha_bar <- length(P_setofplayers) # Big M for transfers (potentially unused if formulation is tight)

# Derived constants
num_players <- length(P_setofplayers)
num_teams <- length(C_setofteams)
num_gameweeks <- length(T_setofgameweeks)
num_subs_order <- length(L_substitution)
total_squad_size <- MK + MD + MM + MF # 15

# --- Data Preparation for Coefficients (Copied) ---
player_gw_combos <- expand.grid(
  player_id = P_setofplayers,
  GW = T_setofgameweeks,
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)
data_full <- merge(
  player_gw_combos,
  data[, c("player_id", "GW", "value", "total_points", "position", "team", "name")], # Added name
  by = c("player_id", "GW"),
  all.x = TRUE
)
data_full$value[is.na(data_full$value)] <- 50.0 # Scaled value
data_full$total_points[is.na(data_full$total_points)] <- 0.0
data_full <- data_full %>%
    group_by(player_id) %>%
    fill(position, team, name, .direction = "downup") %>%
    ungroup()
data_full$position[is.na(data_full$position)] <- "MID"
data_full$team[is.na(data_full$team)] <- C_setofteams[1]
data_full$name[is.na(data_full$name)] <- paste("Unknown Player", data_full$player_id[is.na(data_full$name)])


player_map <- setNames(1:num_players, P_setofplayers)
gameweek_map <- setNames(1:num_gameweeks, T_setofgameweeks)

get_value <- function(p_id, gw_val) {
    val <- data_full$value[data_full$player_id == p_id & data_full$GW == gw_val]
    ifelse(length(val) == 1, val, 50.0)
}
get_points <- function(p_id, gw_val) {
    pts <- data_full$total_points[data_full$player_id == p_id & data_full$GW == gw_val]
    ifelse(length(pts) == 1, pts, 0.0)
}

# --- Variable Indexing (Reusing from lpSolve version) ---
# Order: x, x_freehit, y, f, h, g, u, e, w, c, b, r, lambda, v, q, alpha
n_p = num_players
n_t = num_gameweeks
n_l = num_subs_order

# Using c[p,t] as per original script
var_counts <- c(
    x = n_p * n_t, x_freehit = n_p * n_t, y = n_p * n_t, f = n_p * n_t, h = n_p * n_t,
    g = n_p * n_t * n_l, u = n_p * n_t, e = n_p * n_t, w = n_t,
    c = n_p * n_t, # Original c[p,t]
    b = n_t, r = n_t, lambda = n_p * n_t, v = n_t, q = n_t, alpha = n_t
)
var_starts <- cumsum(c(0, var_counts))
names(var_starts) <- c(names(var_counts), "total")
total_vars <- var_starts[["total"]]

# Helper function to get the flat index (same as before)
get_idx <- function(var_name, p_idx=NULL, t_idx=NULL, l_idx=NULL) {
    start <- var_starts[[var_name]]
    idx <- start # Base for time-only variables

    if (var_name %in% c("x", "x_freehit", "y", "f", "h", "u", "e", "lambda", "c")) { # Added c here
        # Variables indexed by (p, t)
        idx <- start + (t_idx - 1) * n_p + p_idx
    } else if (var_name == "g") {
        # Variable indexed by (p, t, l)
        idx <- start + (t_idx - 1) * n_p * n_l + (l_idx - 1) * n_p + p_idx
    } else if (var_name %in% c("w", "b", "r", "v", "q", "alpha")) {
        # Variables indexed by (t)
        idx <- start + t_idx
    }
    return(as.integer(idx))
}

# --- Initialize lpSolveAPI Model ---
lp_model <- make.lp(0, total_vars)
lp.control(lp_model, sense = "max") # Set objective sense to maximize

# Optional: Set variable names (helps with debugging, reading .lp files)
var_names <- character(total_vars)
for (var_name in names(var_counts)) {
    count <- var_counts[[var_name]]
    indices <- (var_starts[[var_name]] + 1):(var_starts[[var_name]] + count)
    if (var_name %in% c("x", "x_freehit", "y", "f", "h", "u", "e", "lambda", "c")) {
        temp_indices <- expand.grid(p=1:n_p, t=1:n_t)
        var_names[indices] <- paste0(var_name, "_p", temp_indices$p, "_t", temp_indices$t)
    } else if (var_name == "g") {
         temp_indices <- expand.grid(p=1:n_p, l=1:n_l, t=1:n_t) # Order matters for mapping! Match get_idx
         temp_indices <- temp_indices[, c("p", "t", "l")] # Reorder to match get_idx logic
         var_names[indices] <- paste0(var_name, "_p", temp_indices$p, "_t", temp_indices$t, "_l", temp_indices$l)
    } else if (var_name %in% c("w", "b", "r", "v", "q", "alpha")) {
         var_names[indices] <- paste0(var_name, "_t", 1:n_t)
    }
}
colnames(lp_model) <- var_names


# --- Set Objective Function ---
objective.in <- numeric(total_vars)
for (t_real in T_setofgameweeks) {
    t_idx <- gameweek_map[[as.character(t_real)]]
    for (p_real in P_setofplayers) {
        p_idx <- player_map[[as.character(p_real)]]
        player_points_t <- get_points(p_real, t_real)

        if (player_points_t != 0) { # Optimization: only add non-zero coefficients
            objective.in[get_idx("y", p_idx, t_idx)] <- player_points_t
            objective.in[get_idx("f", p_idx, t_idx)] <- player_points_t # Base point + captain bonus
            objective.in[get_idx("c", p_idx, t_idx)] <- 2 * player_points_t # Triple captain bonus
        }
        if (epsilon * player_points_t != 0) {
             objective.in[get_idx("h", p_idx, t_idx)] <- epsilon * player_points_t # Vice-captain bonus
        }

        # Substitution points (g_ptl)
        for (l_idx in L_substitution) {
             if (kappa[l_idx] * player_points_t != 0) {
                objective.in[get_idx("g", p_idx, t_idx, l_idx)] <- kappa[l_idx] * player_points_t
             }
        }
    }
    # Transfer penalties (alpha_t)
    objective.in[get_idx("alpha", t_idx = t_idx)] <- -R
}
set.objfn(lp_model, objective.in)

# --- Add Constraints (Iteratively) ---

# Map players to positions/teams indices (Copied)
player_pos <- setNames(data_full$position[!duplicated(data_full$player_id)],
                       data_full$player_id[!duplicated(data_full$player_id)])
player_team <- setNames(data_full$team[!duplicated(data_full$player_id)],
                        data_full$player_id[!duplicated(data_full$player_id)])
Pgk_idx <- player_map[names(player_pos[player_pos == "GK"])]
Pdef_idx <- player_map[names(player_pos[player_pos == "DEF"])]
Pmid_idx <- player_map[names(player_pos[player_pos == "MID"])]
Pfwd_idx <- player_map[names(player_pos[player_pos == "FWD"])]
P_not_gk_idx <- player_map[names(player_pos[player_pos != "GK"])]
P_c_idx <- lapply(P_c, function(p_list) player_map[as.character(p_list)])


## Gamechips (4.2 - 4.6)
T_FH_idx <- gameweek_map[as.character(T_FH)]
T_SH_idx <- gameweek_map[as.character(T_SH)]

# Wildcard limits (4.2, 4.3)
if (length(T_FH_idx) > 0) {
    indices <- get_idx("w", t_idx = T_FH_idx)
    add.constraint(lp_model, xt = rep(1, length(indices)), type = "<=", rhs = 1, indices = indices)
}
if (length(T_SH_idx) > 0) {
    indices <- get_idx("w", t_idx = T_SH_idx)
    add.constraint(lp_model, xt = rep(1, length(indices)), type = "<=", rhs = 1, indices = indices)
}

# Triple Captain limit (4.4) - Sum over all p and t for c[p,t]
c_indices_all <- get_idx("c", p_idx=1:n_p, t_idx=1:n_t)
add.constraint(lp_model, xt = rep(1, length(c_indices_all)), type = "<=", rhs = 1, indices = c_indices_all)

# Bench Boost limit (4.5)
b_indices_all <- get_idx("b", t_idx = 1:n_t)
add.constraint(lp_model, xt = rep(1, length(b_indices_all)), type = "<=", rhs = 1, indices = b_indices_all)

# Free Hit limit (4.6)
r_indices_all <- get_idx("r", t_idx = 1:n_t)
add.constraint(lp_model, xt = rep(1, length(r_indices_all)), type = "<=", rhs = 1, indices = r_indices_all)

# Ensure only one GC per week (4.7) - Adjusted for c[p,t]
for (t_idx in 1:n_t) {
    w_idx_t <- get_idx("w", t_idx = t_idx)
    b_idx_t <- get_idx("b", t_idx = t_idx)
    r_idx_t <- get_idx("r", t_idx = t_idx)
    c_indices_t <- get_idx("c", p_idx=1:n_p, t_idx=t_idx)
    indices <- c(w_idx_t, b_idx_t, r_idx_t, c_indices_t)
    coeffs <- rep(1, length(indices))
    add.constraint(lp_model, xt = coeffs, type = "<=", rhs = 1, indices = indices)
}


## Selected squad constraints (4.8-4.12) - Standard Squad (non-FreeHit)
for (t_idx in 1:n_t) {
    # Position requirements (4.8-4.11)
    indices <- get_idx("x", p_idx=Pgk_idx, t_idx=t_idx); add.constraint(lp_model, xt=rep(1,length(indices)), type="==", rhs=MK, indices=indices)
    indices <- get_idx("x", p_idx=Pdef_idx, t_idx=t_idx); add.constraint(lp_model, xt=rep(1,length(indices)), type="==", rhs=MD, indices=indices)
    indices <- get_idx("x", p_idx=Pmid_idx, t_idx=t_idx); add.constraint(lp_model, xt=rep(1,length(indices)), type="==", rhs=MM, indices=indices)
    indices <- get_idx("x", p_idx=Pfwd_idx, t_idx=t_idx); add.constraint(lp_model, xt=rep(1,length(indices)), type="==", rhs=MF, indices=indices)

    # Team limit (4.12)
    for (team_players_idx in P_c_idx) {
        if (length(team_players_idx) > 0) {
            indices <- get_idx("x", p_idx = team_players_idx, t_idx = t_idx)
            add.constraint(lp_model, xt = rep(1, length(indices)), type = "<=", rhs = MC, indices = indices)
        }
    }
}

## Selected squad constraints (4.13-4.17) - Free Hit Squad (x_freehit)
# sum(x_freehit_pos) = POS_COUNT * r[t] => sum(x_freehit_pos) - POS_COUNT * r[t] = 0
# sum(x_freehit_team) <= MC * r[t] => sum(x_freehit_team) - MC * r[t] <= 0
for (t_idx in 1:n_t) {
    r_idx_t <- get_idx("r", t_idx=t_idx)

    # Position requirements (4.13-4.16)
    idx_gk <- get_idx("x_freehit", p_idx=Pgk_idx, t_idx=t_idx); add.constraint(lp_model, xt=c(rep(1,length(idx_gk)), -MK), type="==", rhs=0, indices=c(idx_gk, r_idx_t))
    idx_def <- get_idx("x_freehit", p_idx=Pdef_idx, t_idx=t_idx); add.constraint(lp_model, xt=c(rep(1,length(idx_def)), -MD), type="==", rhs=0, indices=c(idx_def, r_idx_t))
    idx_mid <- get_idx("x_freehit", p_idx=Pmid_idx, t_idx=t_idx); add.constraint(lp_model, xt=c(rep(1,length(idx_mid)), -MM), type="==", rhs=0, indices=c(idx_mid, r_idx_t))
    idx_fwd <- get_idx("x_freehit", p_idx=Pfwd_idx, t_idx=t_idx); add.constraint(lp_model, xt=c(rep(1,length(idx_fwd)), -MF), type="==", rhs=0, indices=c(idx_fwd, r_idx_t))

    # Team limit (4.17)
    for (team_players_idx in P_c_idx) {
        if (length(team_players_idx) > 0) {
            indices_team <- get_idx("x_freehit", p_idx = team_players_idx, t_idx = t_idx)
            add.constraint(lp_model, xt = c(rep(1, length(indices_team)), -MC), type = "<=", rhs = 0, indices = c(indices_team, r_idx_t))
        }
    }
}


## Starting Line-up Constraints (4.18-4.26)
for (t_idx in 1:n_t) {
    b_idx_t <- get_idx("b", t_idx=t_idx)
    r_idx_t <- get_idx("r", t_idx=t_idx)

    # Total starters (4.18) -> sum(y) - phi*b[t] = E
    y_indices <- get_idx("y", p_idx=1:n_p, t_idx=t_idx)
    add.constraint(lp_model, xt=c(rep(1, length(y_indices)), -phi), type="==", rhs=E, indices=c(y_indices, b_idx_t))

    # Starting GKs (4.19) -> sum(y_gk) - phi_K*b[t] = EK
    y_gk_indices <- get_idx("y", p_idx=Pgk_idx, t_idx=t_idx)
    add.constraint(lp_model, xt=c(rep(1, length(y_gk_indices)), -phi_K), type="==", rhs=EK, indices=c(y_gk_indices, b_idx_t))

    # Min starters per position (4.20-4.22) - >=
    idx_def_st <- get_idx("y", p_idx=Pdef_idx, t_idx=t_idx); add.constraint(lp_model, xt=rep(1,length(idx_def_st)), type=">=", rhs=ED, indices=idx_def_st)
    idx_mid_st <- get_idx("y", p_idx=Pmid_idx, t_idx=t_idx); add.constraint(lp_model, xt=rep(1,length(idx_mid_st)), type=">=", rhs=EM, indices=idx_mid_st)
    idx_fwd_st <- get_idx("y", p_idx=Pfwd_idx, t_idx=t_idx); add.constraint(lp_model, xt=rep(1,length(idx_fwd_st)), type=">=", rhs=EF, indices=idx_fwd_st)

    # Link starting lineup to squad (4.24-4.26 using lambda auxiliary)
    for (p_idx in 1:n_p) {
        y_idx_pt <- get_idx("y", p_idx, t_idx)
        xfh_idx_pt <- get_idx("x_freehit", p_idx, t_idx)
        lambda_idx_pt <- get_idx("lambda", p_idx, t_idx)
        x_idx_pt <- get_idx("x", p_idx, t_idx)

        # y[p,t] - x_freehit[p,t] - lambda[p,t] <= 0 (4.24)
        add.constraint(lp_model, xt=c(1, -1, -1), type="<=", rhs=0, indices=c(y_idx_pt, xfh_idx_pt, lambda_idx_pt))
        # lambda[p,t] - x[p,t] <= 0 (4.25)
        add.constraint(lp_model, xt=c(1, -1), type="<=", rhs=0, indices=c(lambda_idx_pt, x_idx_pt))
        # lambda[p,t] + r[t] <= 1 (4.26)
        add.constraint(lp_model, xt=c(1, 1), type="<=", rhs=1, indices=c(lambda_idx_pt, r_idx_t))
    }
}

## Captain and vice-captain constraints (4.27-4.29) - Using original c[p,t]
for (t_idx in 1:n_t) {
    f_indices_t <- get_idx("f", p_idx=1:n_p, t_idx=t_idx)
    c_indices_t <- get_idx("c", p_idx=1:n_p, t_idx=t_idx)
    h_indices_t <- get_idx("h", p_idx=1:n_p, t_idx=t_idx)

    # Exactly one captain choice (normal OR triple) (4.27) -> sum(f) + sum(c) = 1
    add.constraint(lp_model, xt=rep(1, length(f_indices_t) + length(c_indices_t)), type="==", rhs=1, indices=c(f_indices_t, c_indices_t))

    # Exactly one vice-captain (4.28) -> sum(h) = 1
    add.constraint(lp_model, xt=rep(1, length(h_indices_t)), type="==", rhs=1, indices=h_indices_t)

    # Captains must be starters (4.29) -> f[p,t] + c[p,t] + h[p,t] - y[p,t] <= 0
    for (p_idx in 1:n_p) {
        f_idx_pt <- get_idx("f", p_idx, t_idx)
        c_idx_pt <- get_idx("c", p_idx, t_idx)
        h_idx_pt <- get_idx("h", p_idx, t_idx)
        y_idx_pt <- get_idx("y", p_idx, t_idx)
        add.constraint(lp_model, xt=c(1, 1, 1, -1), type="<=", rhs=0, indices=c(f_idx_pt, c_idx_pt, h_idx_pt, y_idx_pt))
    }
}

## Substitution constraints (4.30-4.32) - For non-GK players
for (t_idx in 1:n_t) {
     r_idx_t <- get_idx("r", t_idx=t_idx)
    for (p_idx in P_not_gk_idx) { # Only non-GKs for subs
        y_idx_pt <- get_idx("y", p_idx, t_idx)
        x_idx_pt <- get_idx("x", p_idx, t_idx)
        xfh_idx_pt <- get_idx("x_freehit", p_idx, t_idx)
        g_indices_ptl <- get_idx("g", p_idx=p_idx, t_idx=t_idx, l_idx=L_substitution)

        # 4.30: y + sum(g) - x - beta*r <= 0
        add.constraint(lp_model, xt=c(1, rep(1, length(g_indices_ptl)), -1, -beta), type="<=", rhs=0, indices=c(y_idx_pt, g_indices_ptl, x_idx_pt, r_idx_t))

        # 4.31: y + sum(g) - x_fh + beta*r <= beta
        add.constraint(lp_model, xt=c(1, rep(1, length(g_indices_ptl)), -1, beta), type="<=", rhs=beta, indices=c(y_idx_pt, g_indices_ptl, xfh_idx_pt, r_idx_t))
    }
    # 4.32: sum(g[p,t,l], p in P_not_gk) <= 1 (for each t, l)
    for (l_idx in L_substitution) {
        g_indices_tl <- get_idx("g", p_idx=P_not_gk_idx, t_idx=t_idx, l_idx=l_idx)
        add.constraint(lp_model, xt=rep(1, length(g_indices_tl)), type="<=", rhs=1, indices=g_indices_tl)
    }
}


## Budget Constraints (4.33-4.39)
value_matrix <- matrix(NA, nrow=n_p, ncol=n_t, dimnames=list(P_setofplayers, T_setofgameweeks))
for(t_real in T_setofgameweeks) {
    t_idx = gameweek_map[[as.character(t_real)]]
    for(p_real in P_setofplayers) {
        p_idx = player_map[[as.character(p_real)]]
        value_matrix[p_idx, t_idx] <- get_value(p_real, t_real)
    }
}

# 4.33: Initial budget: sum(value[p,1] * x[p,1]) + v[1] = BS
t1_idx <- gameweek_map[[as.character(T_setofgameweeks[1])]] # Should be 1
v1_idx <- get_idx("v", t_idx=t1_idx)
x1_indices <- get_idx("x", p_idx=1:n_p, t_idx=t1_idx)
coeffs_433 <- c(value_matrix[1:n_p, t1_idx], 1) # Player values + coeff for v[1]
indices_433 <- c(x1_indices, v1_idx)
add.constraint(lp_model, xt=coeffs_433, type="==", rhs=BS, indices=indices_433)

# For t > 1
for (t_real in T_setofgameweeks) {
    t_idx <- gameweek_map[[as.character(t_real)]]
    if (t_real > T_setofgameweeks[1]) {
        # Find previous gameweek index dynamically
        t_prev_real <- T_setofgameweeks[match(t_real, T_setofgameweeks) - 1]
        t_prev_idx <- gameweek_map[[as.character(t_prev_real)]]

        v_idx_t <- get_idx("v", t_idx=t_idx)
        v_idx_prev <- get_idx("v", t_idx=t_prev_idx)
        r_idx_t <- get_idx("r", t_idx = t_idx)
        u_indices_t <- get_idx("u", p_idx=1:n_p, t_idx=t_idx)
        e_indices_t <- get_idx("e", p_idx=1:n_p, t_idx=t_idx)

        # 4.34: v[t-1] - v[t] + sum(val[p,t]*u[p,t]) - sum(val[p,t]*e[p,t]) = 0
        coeffs_434 <- c(1, -1, value_matrix[1:n_p, t_idx], -value_matrix[1:n_p, t_idx])
        indices_434 <- c(v_idx_prev, v_idx_t, u_indices_t, e_indices_t)
        add.constraint(lp_model, xt=coeffs_434, type="==", rhs=0, indices=indices_434)

        # 4.35: x[p,t-1] + e[p,t] - u[p,t] - x[p,t] = 0 (for each p)
        for(p_idx in 1:n_p) {
            x_idx_prev <- get_idx("x", p_idx, t_prev_idx)
            e_idx_t <- get_idx("e", p_idx, t_idx)
            u_idx_t <- get_idx("u", p_idx, t_idx)
            x_idx_t <- get_idx("x", p_idx, t_idx)
            add.constraint(lp_model, xt=c(1, 1, -1, -1), type="==", rhs=0, indices=c(x_idx_prev, e_idx_t, u_idx_t, x_idx_t))
        }

        # 4.37: Free Hit budget: sum(val[p,t]*x_fh[p,t]) - sum(val[p,t-1]*x[p,t-1]) - v[t-1] - M*r[t] <= M
        # Using M = BS
        xfh_indices_t <- get_idx("x_freehit", p_idx=1:n_p, t_idx=t_idx)
        x_indices_prev <- get_idx("x", p_idx=1:n_p, t_idx=t_prev_idx)
        coeffs_437 <- c(value_matrix[1:n_p, t_idx], -value_matrix[1:n_p, t_prev_idx], -1, -BS)
        indices_437 <- c(xfh_indices_t, x_indices_prev, v_idx_prev, r_idx_t)
        add.constraint(lp_model, xt=coeffs_437, type="<=", rhs= -BS) # Corrected RHS: <= M*(1-r[t]) -> ... <= M - M*r[t] -> ... + M*r[t] <= M. My prev attempt had M on RHS, this moves it.

        # 4.38: Transfer out limit: sum(u[p,t]) + M*r[t] <= M
        # Using M = total_squad_size
        add.constraint(lp_model, xt=c(rep(1, n_p), total_squad_size), type="<=", rhs=total_squad_size, indices=c(u_indices_t, r_idx_t))

        # 4.39: Transfer in limit: sum(e[p,t]) + M*r[t] <= M
        add.constraint(lp_model, xt=c(rep(1, n_p), total_squad_size), type="<=", rhs=total_squad_size, indices=c(e_indices_t, r_idx_t))
    }
}

# 4.36: Cannot transfer in AND out same player: e[p,t] + u[p,t] <= 1
for (t_real in T_setofgameweeks) {
     t_idx <- gameweek_map[[as.character(t_real)]]
     # Skip t=1 for transfers u, e (no transfers *into* GW1)
     if (t_real > T_setofgameweeks[1]) {
       for (p_idx in 1:n_p) {
           e_idx_pt <- get_idx("e", p_idx, t_idx)
           u_idx_pt <- get_idx("u", p_idx, t_idx)
           add.constraint(lp_model, xt=c(1, 1), type="<=", rhs=1, indices=c(e_idx_pt, u_idx_pt))
       }
     }
}


## Transfer Constraints (4.40 - 4.44)
# 4.40: Initial free transfers q[2] = Q_under_bar (assuming GWs start at 1 and are contiguous)
if (length(T_setofgameweeks) >= 2 && T_setofgameweeks[1] == 1 && T_setofgameweeks[2] == 2) {
    t2_idx <- gameweek_map[["2"]]
    q2_idx <- get_idx("q", t_idx=t2_idx)
    add.constraint(lp_model, xt=1, type="==", rhs=Q_under_bar, indices=q2_idx)
} else if (length(T_setofgameweeks) >= 1) {
     # Set initial FTs for the very first gameweek in the set
     t1_idx <- gameweek_map[[as.character(T_setofgameweeks[1])]]
     q1_idx <- get_idx("q", t_idx = t1_idx)
     add.constraint(lp_model, xt=1, type="==", rhs=Q_under_bar, indices=q1_idx)
}

# Constraints involving t+1 (apply for t=1 to n_t-1)
for (t_idx in 1:(n_t - 1)) {
    # Find next gameweek index dynamically
    t_real <- T_setofgameweeks[t_idx]
    t_next_real <- T_setofgameweeks[t_idx + 1]
    t_next_idx <- gameweek_map[[as.character(t_next_real)]]

    q_idx_t <- get_idx("q", t_idx=t_idx)
    q_idx_next <- get_idx("q", t_idx=t_next_idx)
    alpha_idx_t <- get_idx("alpha", t_idx=t_idx)
    w_idx_t <- get_idx("w", t_idx=t_idx)
    r_idx_t <- get_idx("r", t_idx=t_idx)
    e_indices_t <- get_idx("e", p_idx=1:n_p, t_idx=t_idx) # Transfers IN for week t

    # 4.41: Free transfer evolution: -q[t+1] + M*w[t] + q[t] - sum(e[p,t]) + alpha[t] >= -Q_u
    # Using M = total_squad_size
    coeffs_441 <- c(-1, total_squad_size, 1, rep(-1, n_p), 1)
    indices_441 <- c(q_idx_next, w_idx_t, q_idx_t, e_indices_t, alpha_idx_t)
    add.constraint(lp_model, xt=coeffs_441, type=">=", rhs=-Q_under_bar, indices=indices_441)

    # Link penalty transfers: alpha[t] >= sum(e[p,t]) - q[t]
    # => alpha[t] - sum(e[p,t]) + q[t] >= 0
    coeffs_alpha <- c(1, rep(-1, n_p), 1)
    indices_alpha <- c(alpha_idx_t, e_indices_t, q_idx_t)
    add.constraint(lp_model, xt=coeffs_alpha, type=">=", rhs=0, indices=indices_alpha)

    # 4.43: Min free transfers next week: q[t+1] >= Q_under_bar
    add.constraint(lp_model, xt=1, type=">=", rhs=Q_under_bar, indices=q_idx_next)

    # 4.44: Max free transfers next week (with WC/FH reset):
    # q[t+1] + (Q_bar - Q_u)*w[t] + (Q_bar - Q_u)*r[t] <= Q_bar
    coeff_w = Q_bar - Q_under_bar
    coeff_r = Q_bar - Q_under_bar
    add.constraint(lp_model, xt=c(1, coeff_w, coeff_r), type="<=", rhs=Q_bar, indices=c(q_idx_next, w_idx_t, r_idx_t))
}

# Add explicit non-negativity and upper bounds where needed
# v[t] >= 0, alpha[t] >= 0 (default in lpSolveAPI)
# q[t] >= 0 (default), but also q[t] <= Q_bar
for (t_idx in 1:n_t) {
    q_idx_t <- get_idx("q", t_idx=t_idx)
    add.constraint(lp_model, xt=1, type="<=", rhs=Q_bar, indices=q_idx_t)
}


# --- Set Variable Types ---
binary_vars_idx <- unique(c( # Use unique to avoid duplicates if indices overlap somehow
    get_idx("x", p_idx=1:n_p, t_idx=1:n_t),
    get_idx("x_freehit", p_idx=1:n_p, t_idx=1:n_t),
    get_idx("y", p_idx=1:n_p, t_idx=1:n_t),
    get_idx("f", p_idx=1:n_p, t_idx=1:n_t),
    get_idx("h", p_idx=1:n_p, t_idx=1:n_t),
    get_idx("g", p_idx=1:n_p, t_idx=1:n_t, l_idx=1:n_l),
    get_idx("u", p_idx=1:n_p, t_idx=1:n_t),
    get_idx("e", p_idx=1:n_p, t_idx=1:n_t),
    get_idx("w", t_idx=1:n_t),
    get_idx("c", p_idx=1:n_p, t_idx=1:n_t), # Using c[p,t]
    get_idx("b", t_idx=1:n_t),
    get_idx("r", t_idx=1:n_t),
    get_idx("lambda", p_idx=1:n_p, t_idx=1:n_t)
))

integer_vars_idx <- unique(c(
    get_idx("q", t_idx=1:n_t),
    get_idx("alpha", t_idx=1:n_t)
))

set.type(lp_model, columns = binary_vars_idx, type = "binary")
set.type(lp_model, columns = integer_vars_idx, type = "integer")

# Continuous vars (v) are handled by default (non-negative continuous)

# --- Solve the Model ---
print(paste("Total variables:", total_vars))
print(paste("Total constraints:", dim(lp_model)[1])) # Number of rows added
print(paste("Binary variables:", length(binary_vars_idx)))
print(paste("Integer variables:", length(integer_vars_idx)))

# Optional: Write the model to a file for inspection
# write.lp(lp_model, "fpl_model_api.lp")

# Time the solve
start_time <- Sys.time()

# Set solver timeout (e.g., 5 minutes = 300 seconds) - adjust as needed
lp.control(lp_model, timeout = 300)

status_code <- solve(lp_model)

end_time <- Sys.time()
print(paste("Solve time:", end_time - start_time))
print(paste("Solver Status Code:", status_code, "(0=Optimal, 2=Infeasible, 3=Unbounded, etc.)"))


# --- Process Results ---
if (status_code == 0) {
    print("Optimal solution found!")
    print(paste("Objective value (Max Points):", get.objective(lp_model)))

    # Extract solution values
    sol_values <- get.variables(lp_model)

    # Function to get variable value from solution vector (same as before)
    get_sol_val <- function(var_name, p_idx=NULL, t_idx=NULL, l_idx=NULL) {
        idx <- get_idx(var_name, p_idx, t_idx, l_idx)
        # Add tolerance for binary/integer checks
        val <- sol_values[idx]
        if(idx %in% binary_vars_idx || idx %in% integer_vars_idx) {
           # Check type more robustly before rounding
           var_type = get.type(lp_model, columns=idx)
           if(var_type == "binary" || var_type == "integer"){
               return(round(val))
           }
        }
        return(val)
    }

    # Example: Print selected squad for each gameweek (same as before)
    for (t_real in T_setofgameweeks) {
        t_idx <- gameweek_map[[as.character(t_real)]]
        print(paste("--- Gameweek", t_real, "---"))

        # Find player indices for squad, starting, captain etc. using get_sol_val > 0.5
        squad_p_idx <- which(sapply(1:n_p, function(p) get_sol_val("x", p_idx=p, t_idx=t_idx)) > 0.5)
        starting_p_idx <- which(sapply(1:n_p, function(p) get_sol_val("y", p_idx=p, t_idx=t_idx)) > 0.5)
        captain_p_idx <- which(sapply(1:n_p, function(p) get_sol_val("f", p_idx=p, t_idx=t_idx)) > 0.5)
        vicecap_p_idx <- which(sapply(1:n_p, function(p) get_sol_val("h", p_idx=p, t_idx=t_idx)) > 0.5)
        triplecap_p_idx <- which(sapply(1:n_p, function(p) get_sol_val("c", p_idx=p, t_idx=t_idx)) > 0.5)

        # Map indices back to actual player IDs
        squad_players <- P_setofplayers[squad_p_idx]
        starting_players <- P_setofplayers[starting_p_idx]
        captain_player <- P_setofplayers[captain_p_idx]
        vicecap_player <- P_setofplayers[vicecap_p_idx]
        triplecap_player <- P_setofplayers[triplecap_p_idx]
        bench_players <- setdiff(squad_players, starting_players)

        # Look up details in data_full
        squad_details <- data_full %>% filter(player_id %in% squad_players, GW == t_real) %>% select(name, position, team, value, total_points)
        starting_details <- data_full %>% filter(player_id %in% starting_players, GW == t_real) %>% select(name, position, team)
        bench_details <- data_full %>% filter(player_id %in% bench_players, GW == t_real) %>% select(name, position, team)

        print("Squad:")
        print(squad_details)
        print("Starting XI:")
        print(starting_details)
        print("Bench:")
        print(bench_details)

        if(length(captain_player) > 0) print(paste("Captain:", data_full$name[data_full$player_id == captain_player & data_full$GW == t_real][1]))
        if(length(vicecap_player) > 0) print(paste("Vice-Captain:", data_full$name[data_full$player_id == vicecap_player & data_full$GW == t_real][1]))
        if(length(triplecap_player) > 0) print(paste("TRIPLE Captain:", data_full$name[data_full$player_id == triplecap_player & data_full$GW == t_real][1]))


        # Transfers
        if (t_real > T_setofgameweeks[1]) {
            transfers_out_idx <- which(sapply(1:n_p, function(p) get_sol_val("u", p_idx=p, t_idx=t_idx)) > 0.5)
            transfers_in_idx <- which(sapply(1:n_p, function(p) get_sol_val("e", p_idx=p, t_idx=t_idx)) > 0.5)
            transfers_out <- P_setofplayers[transfers_out_idx]
            transfers_in <- P_setofplayers[transfers_in_idx]

            if(length(transfers_out) > 0 || length(transfers_in) > 0) {
                 print("Transfers:")
                 print(paste(" Out:", paste(unique(data_full$name[data_full$player_id %in% transfers_out & data_full$GW == t_real]), collapse=", ")))
                 print(paste(" In:", paste(unique(data_full$name[data_full$player_id %in% transfers_in & data_full$GW == t_real]), collapse=", ")))
            }
             print(paste(" Free Transfers Available:", get_sol_val("q", t_idx=t_idx)))
             print(paste(" Penalty Transfers (Hits):", get_sol_val("alpha", t_idx=t_idx)))
        } else {
            print(paste(" Initial Free Transfers:", get_sol_val("q", t_idx=t_idx)))
        }

        # Chips
        if(get_sol_val("w", t_idx=t_idx) > 0.5) print(" Chip Active: Wildcard")
        if(get_sol_val("b", t_idx=t_idx) > 0.5) print(" Chip Active: Bench Boost")
        if(get_sol_val("r", t_idx=t_idx) > 0.5) print(" Chip Active: Free Hit")
        if(length(triplecap_player) > 0) print(" Chip Active: Triple Captain") # Implied by c[p,t] > 0


        print(paste(" Remaining Budget:", round(get_sol_val("v", t_idx=t_idx) / 10.0, 1) )) # Rescale budget
        cat("\n")

    }

} else {
    print("Solver failed to find an optimal solution or timed out.")
    # You might want to check get.solution for more details if available
}

# Clean up the model object from memory
# delete.lp(lp_model) # Uncomment if needed

# --- END OF lpSolveAPI Translation ---