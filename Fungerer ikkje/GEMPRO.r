# Blandet heltalls modell  ----
# Basert på Kristiansen et al. # NOTE! ALL EQUATIONS AND TABLE NUMBERS REFER TO THIS
# THIS MIGHT BE CHANGED AT A LATER TIME, BUT IS KEPT LIKE THIS UNDER PROD. TO BE
# "OVERSIKTLIG" -PER 30.03
rm(list = ls(all = TRUE))

# Pakker
library(tidyverse)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)
# library(ROI.plugin.symphony) # Keep installed if you want to try it later
# library(ROI.plugin.lpsolve) # Keep installed if you want to try it later


# Forsøker på en sesong først for å se om det fungerer, Den nyeste sesongen 23/24
allesesonger <- read_csv("Differensiert gw alle tre sesonger(22-24), heltall.csv", show_col_types = FALSE)

# En hel sesong tar veldig lang tid så derfor prøver jeg først med 3 uker
data <- allesesonger |> filter(GW <= 2)

# --- Data Preparation for Coefficients ---
# Ensure unique player-GW combinations and handle missing values explicitly
player_gw_combos <- expand.grid(
  player_id = unique(data$player_id),
  GW = unique(data$GW),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)

data_prepared <- data |>
  select(player_id, GW, value, total_points, position, team) |>
  # Ensure value is numeric (it often comes scaled by 10 in FPL data)
  mutate(value = as.numeric(value)) |>
  # Right join to ensure all player-GW combos exist
  right_join(player_gw_combos, by = c("player_id", "GW")) |>
  # Fill missing values:
  # - Need position/team info for players potentially missing in early GWs
  #   Group by player and fill downwards/upwards
  group_by(player_id) |>
  fill(position, team, .direction = "downup") |>
  ungroup() |>
  # - Assume 0 points for missing points data
  mutate(total_points = ifelse(is.na(total_points), 0, total_points)) |>
  # - Set a default value (e.g., 4.0m = 40) for missing values
  #   Or perhaps better: fill based on the player's known values
  group_by(player_id) |>
  fill(value, .direction = "downup") |>
  ungroup() |>
  # If a player *never* had a value (newly added?), assign a default
  mutate(value = ifelse(is.na(value), 40, value)) # Default 4.0m

# Check if any essential info is still missing
if(any(is.na(data_prepared$position)) || any(is.na(data_prepared$team))) {
  warning("Missing position or team data after preparation. Check data source.")
  # Handle potentially problematic rows, e.g., remove them or assign defaults
  data_prepared <- data_prepared |> filter(!is.na(position) & !is.na(team))
}
if(any(is.na(data_prepared$value))) {
    warning("Missing value data after preparation. Assigning default 4.0m")
    data_prepared$value <- ifelse(is.na(data_prepared$value), 40, data_prepared$value)
}
if(any(is.na(data_prepared$total_points))) {
    warning("Missing points data after preparation. Assigning 0 points.")
    data_prepared$total_points <- ifelse(is.na(data_prepared$total_points), 0, data_prepared$total_points)
}


# --- Sets Table 4.1 ---
T_setofgameweeks <- unique(data_prepared$GW) # Sett med gameweeks
P_setofplayers <- unique(data_prepared$player_id) # Sett med spillere
C_setofteams <- unique(data_prepared$team) # Sett med lag
L_substitution <- 1:3 # Ubytter rekkefølge

# --- Subsets Table 4.2 ---
# Use the prepared data to define subsets to ensure consistency
get_player_ids_by_pos <- function(pos, gw = min(T_setofgameweeks)) {
  unique(data_prepared$player_id[data_prepared$position == pos & data_prepared$GW == gw])
}
Pgk <- get_player_ids_by_pos("GK")
Pdef <- get_player_ids_by_pos("DEF")
Pmid <- get_player_ids_by_pos("MID")
Pfwd <- get_player_ids_by_pos("FWD")
P_not_gk <- unique(data_prepared$player_id[data_prepared$position != "GK" & data_prepared$GW == min(T_setofgameweeks)])

# Create P_c using the prepared data, ensure players are assigned consistently
P_c <- data_prepared %>%
  filter(GW == min(T_setofgameweeks)) %>% # Use first GW for initial team association
  select(player_id, team) %>%
  distinct() %>%
  split(.$team) %>%
  map(~.$player_id)

# Check if any team subset is empty or missing
if (length(P_c) != length(C_setofteams)) {
    warning("Mismatch between C_setofteams and P_c keys.")
}
if (any(sapply(P_c, length) == 0)) {
    warning("Some teams in P_c have zero players.")
}

# Ensure all players are in P_c
players_in_pc <- unlist(P_c, use.names = FALSE)
if (!all(P_setofplayers %in% players_in_pc)) {
    missing_players <- P_setofplayers[!P_setofplayers %in% players_in_pc]
    warning(paste("Players missing from P_c:", paste(missing_players, collapse=", ")))
    # Decide how to handle: add them to a dummy team? or ensure data prep covers them?
    # For now, let's filter them out of P_setofplayers if they aren't in P_c
    P_setofplayers <- intersect(P_setofplayers, players_in_pc)
    # Also need to update position subsets if P_setofplayers changed significantly
    Pgk <- intersect(Pgk, P_setofplayers)
    Pdef <- intersect(Pdef, P_setofplayers)
    Pmid <- intersect(Pmid, P_setofplayers)
    Pfwd <- intersect(Pfwd, P_setofplayers)
    P_not_gk <- intersect(P_not_gk, P_setofplayers)
    print("Filtered P_setofplayers and subsets to match P_c.")
}


medianavgameweeks <- median(T_setofgameweeks)
T_FH <- T_setofgameweeks[T_setofgameweeks <= medianavgameweeks]
T_SH <- T_setofgameweeks[T_setofgameweeks > medianavgameweeks]

## Index Table 4.3 -----
t <- T_setofgameweeks # Gameweek index (use 'gw' or 't_idx' in loops to avoid confusion)
p <- P_setofplayers # Player index
l <- L_substitution # Substitution index

## Parametre Table 4.4 -----
R <- 4              # Straff for hver overgang utover gratis overganger
MK <- 2             # Goalkeepers required in squad
MD <- 5             # Defenders required in squad
MM <- 5             # Midfielders required in squad
MF <- 3             # Forwards required in squad
MC <- 3             # Max players allowed from the same team
E <- 11             # Players required in starting line-up
EK <- 1             # Goalkeepers required in starting line-up
ED <- 3             # Min defenders required in starting line-up
EM <- 3             # Min midfielders required (FPL uses dynamic 2-5) -> Kristiansen uses 3
EF <- 1             # Min forwards required in starting line-up
BS <- 1000          # Start budsjett (use 1000 for 100.0m)
phi <- (MK+MD+MM+MF) - E # Number of substitutes on bench (Should be 15 - 11 = 4) --> E+EK is WRONG! FPL lineup is 11 total.
# phi = 15 - 11 = 4 (Correct)
phi_K <- MK - EK      # Number of GK substitutes (2-1 = 1)
Q_bar <- 2            # Max number of free transfers to accumulate (FPL rules changed, often 2 now) -> Kristiansen used 4
Q_under_bar <- 1      # Free transfers given per gameweek

## Konstant parametre -----
epsilon <- 0.001 # Should be very small (original paper: 0.1 seems large)
kappa <- c(0.01, 0.005, 0.001) # Weights for subs priorities
# Ensure kappa has the correct length
if(length(kappa) != length(L_substitution)) {
    stop("Length of kappa must match L_substitution")
}
beta <- length(P_setofplayers) + 1     # Sufficiently large constant (Big M)
alpha_bar <- length(P_setofplayers) + 1 # Sufficiently large constant (Big M)

# --- Coefficient Dataframes for OMPR ---
value_coeffs <- data_prepared |> select(p = player_id, t = GW, value)
points_coeffs <- data_prepared |> select(p = player_id, t = GW, points = total_points)

# --- Model Definition ---
model <- MILPModel() %>%

  # ---- Binære variabler (Tabell 4.5) ----
  # Using p_idx, t_idx to avoid conflicts with set names p, t
  add_variable(x[p_idx, t_idx], p_idx = p, t_idx = t, type = "binary") %>%
  add_variable(x_freehit[p_idx, t_idx], p_idx = p, t_idx = t, type = "binary") %>%
  add_variable(y[p_idx, t_idx], p_idx = p, t_idx = t, type = "binary") %>%
  add_variable(f[p_idx, t_idx], p_idx = p, t_idx = t, type = "binary") %>% # Captain
  add_variable(h[p_idx, t_idx], p_idx = p, t_idx = t, type = "binary") %>% # Vice-captain
  add_variable(g[p_idx, t_idx, l_idx], p_idx = p, t_idx = t, l_idx = l, type = "binary") %>% # Substitution
  add_variable(u[p_idx, t_idx], p_idx = p, t_idx = t, type = "binary") %>% # Transfers out
  add_variable(e[p_idx, t_idx], p_idx = p, t_idx = t, type = "binary") %>% # Transfers in
  add_variable(w[t_idx], t_idx = t, type = "binary") %>% # Wildcard
  add_variable(c_chip[t_idx], t_idx = t, type = "binary") %>% # Triple Captain chip (used per GW)
  # NOTE: Kristiansen c[p,t] selects WHICH player is triple captained IF chip is active
  # This is complex. Let's simplify: c_chip[t] activates the chip for gameweek t.
  # The objective function will handle the points bonus for the captain if c_chip[t] is 1.
  # add_variable(c[p_idx, t_idx], p_idx = p, t_idx = t, type = "binary") %>% # Original Triple Captain per player
  add_variable(b[t_idx], t_idx = t, type = "binary") %>% # Bench Boost
  add_variable(r[t_idx], t_idx = t, type = "binary") %>% # Free Hit
  add_variable(lambda[p_idx, t_idx], p_idx = p, t_idx = t, type = "binary") %>% # Auxiliary for FH lineup

  # ---- Kontinuerlige/heltallsvariabler ----
  add_variable(v[t_idx], t_idx = t, type = "continuous", lb = 0) %>% # Budget
  add_variable(q[t_idx], t_idx = t, type = "integer", lb = 0, ub = Q_bar) %>% # Free transfers
  add_variable(alpha[t_idx], t_idx = t, type = "integer", lb = 0) # Penalty transfers


# --- Constraints ----

## Gamechips (4.2 - 4.6) --- Use the new c_chip variable
model <- model %>%
  # Wildcard limits (4.2, 4.3)
  add_constraint(sum_expr(w[t_idx], t_idx = T_FH) <= 1) %>%
  add_constraint(sum_expr(w[t_idx], t_idx = T_SH) <= 1) %>%

  # Triple Captain limit (4.4 simplified)
  add_constraint(sum_expr(c_chip[t_idx], t_idx = t) <= 1) %>%

  # Bench Boost limit (4.5)
  add_constraint(sum_expr(b[t_idx], t_idx = t) <= 1) %>%

  # Free Hit limit (4.6)
  add_constraint(sum_expr(r[t_idx], t_idx = t) <= 1) %>%

  # Only one chip per gameweek (4.7 - modified for c_chip)
  add_constraint(w[t_idx] + c_chip[t_idx] + b[t_idx] + r[t_idx] <= 1, t_idx = t)


## Selected squad constraints (4.8-4.12) ----
model <- model %>%
  # Position requirements (4.8-4.11) - Check definition of position sets
  add_constraint(sum_expr(x[p_idx, t_idx], p_idx = Pgk) == MK, t_idx = t) %>%
  add_constraint(sum_expr(x[p_idx, t_idx], p_idx = Pdef) == MD, t_idx = t) %>%
  add_constraint(sum_expr(x[p_idx, t_idx], p_idx = Pmid) == MM, t_idx = t) %>%
  add_constraint(sum_expr(x[p_idx, t_idx], p_idx = Pfwd) == MF, t_idx = t) %>% # Total = 15 players

  # Team limit constraints (4.12) - Loop version
  add_constraint(sum_expr(x[p_idx, t_idx], p_idx = P_c[[team_name]]) <= MC,
                 t_idx = t, team_name = C_setofteams)
# Alternative vectorized way (might be faster if many teams):
# model <- model %>%
#   add_constraint(sum_expr(x[p_idx, t_idx], p_idx = P_c[[team_name]]) <= MC,
#                  .list = list(t_idx = t, team_name = C_setofteams))


## Free Hit Squad Constraints (4.13-4.17) ---
# These constraints should only be active if r[t] = 1.
# We can enforce this using big-M or indicator constraints.
# Original formulation: sum(x_fh) = MK*r[t] etc. This multiplication might be the issue.
# Let's try Big-M reformulation:
# sum(x_fh[gk]) <= MK * r[t]  -> sum(x_fh[gk]) - MK <= M*(1-r[t]) - epsilon ??? NO
# sum(x_fh[gk]) >= MK * r[t]  -> MK - sum(x_fh[gk]) <= M*(1-r[t]) ??? NO

# Let's try enforcing them only when r[t]=1. If r[t]=0, x_freehit should be 0 anyway.
model <- model %>%
    # Ensure x_freehit is zero if r[t] is zero
    add_constraint(x_freehit[p_idx, t_idx] <= r[t_idx], p_idx = p, t_idx = t) %>%

    # Position requirements for FH squad (active only if r[t]=1)
    # Sum(x_fh[GK]) = MK * r[t] --> Linearized:
    # Sum(x_fh[GK]) <= MK
    # Sum(x_fh[GK]) >= MK - beta * (1 - r[t])
    # Sum(x_fh[GK]) <= MK + beta * (1 - r[t]) -> This is redundant due to above
    add_constraint(sum_expr(x_freehit[p_idx, t_idx], p_idx = Pgk) >= MK - beta * (1 - r[t_idx]), t_idx = t) %>%
    add_constraint(sum_expr(x_freehit[p_idx, t_idx], p_idx = Pgk) <= MK, t_idx = t) %>% # Implicitly <= MK * r[t_idx] via x_fh <= r

    add_constraint(sum_expr(x_freehit[p_idx, t_idx], p_idx = Pdef) >= MD - beta * (1 - r[t_idx]), t_idx = t) %>%
    add_constraint(sum_expr(x_freehit[p_idx, t_idx], p_idx = Pdef) <= MD, t_idx = t) %>%

    add_constraint(sum_expr(x_freehit[p_idx, t_idx], p_idx = Pmid) >= MM - beta * (1 - r[t_idx]), t_idx = t) %>%
    add_constraint(sum_expr(x_freehit[p_idx, t_idx], p_idx = Pmid) <= MM, t_idx = t) %>%

    add_constraint(sum_expr(x_freehit[p_idx, t_idx], p_idx = Pfwd) >= MF - beta * (1 - r[t_idx]), t_idx = t) %>%
    add_constraint(sum_expr(x_freehit[p_idx, t_idx], p_idx = Pfwd) <= MF, t_idx = t) %>%

    # Team limit for FH squad (active only if r[t]=1)
    # Sum(x_fh[team]) <= MC * r[t] --> Linearized:
    # Sum(x_fh[team]) <= MC
    # Sum(x_fh[team]) >= 0 (implicit)
    # We only need Sum(x_fh[team]) <= MC, this is implicitly handled by x_fh <= r and sum(x_fh) = 15
    # We need to enforce the upper bound strictly:
    add_constraint(sum_expr(x_freehit[p_idx, t_idx], p_idx = P_c[[team_name]]) <= MC,
                   t_idx = t, team_name = C_setofteams)
    # Check: If r[t]=0, then all x_fh=0, sum=0 <= MC (OK). If r[t]=1, sum <= MC (OK).


## Starting Line-up Constraints (4.18-4.22) ----
# Original: sum(y) = E + phi*b[t]. Linearize the b[t] part.
# If b[t]=0, sum(y)=E. If b[t]=1, sum(y)=E+phi (=15).
# sum(y) <= E + phi*b[t]  => sum(y) - E <= phi*b[t] (OK as b is binary)
# sum(y) >= E + phi*b[t]  => E - sum(y) <= phi*(1-b[t]) ??? NO
# Need Big-M:
# sum(y) <= E + phi*b[t]
# sum(y) >= E + phi*b[t] - beta*(1-b[t]) ??? -> sum(y) >= E + phi - beta*(1-b[t]) is too complex
# Alternative linearization:
# sum(y) = E*(1-b[t]) + (E+phi)*b[t] --> Still non-linear
# Let's use two constraints with Big-M:
# sum(y) >= E - beta*b[t]  (If b=0, sum(y)>=E; if b=1, sum(y)>=-large)
# sum(y) <= E + beta*b[t]  (If b=0, sum(y)<=E; if b=1, sum(y)<=large)
# sum(y) >= (E+phi) - beta*(1-b[t]) (If b=1, sum(y)>=E+phi; if b=0, sum(y)>=-large)
# sum(y) <= (E+phi) + beta*(1-b[t]) (If b=1, sum(y)<=E+phi; if b=0, sum(y)<=large)

model <- model %>%
    # Starting lineup size (4.18 - Linearized)
    add_constraint(sum_expr(y[p_idx, t_idx], p_idx = p) >= E - beta * b[t_idx], t_idx = t) %>%
    add_constraint(sum_expr(y[p_idx, t_idx], p_idx = p) <= E + beta * b[t_idx], t_idx = t) %>%
    add_constraint(sum_expr(y[p_idx, t_idx], p_idx = p) >= (E + phi) - beta * (1 - b[t_idx]), t_idx = t) %>%
    add_constraint(sum_expr(y[p_idx, t_idx], p_idx = p) <= (E + phi) + beta * (1 - b[t_idx]), t_idx = t) %>% # E+phi = 15

    # Starting GK size (4.19 - Linearized similarly)
    add_constraint(sum_expr(y[p_idx, t_idx], p_idx = Pgk) >= EK - beta * b[t_idx], t_idx = t) %>%
    add_constraint(sum_expr(y[p_idx, t_idx], p_idx = Pgk) <= EK + beta * b[t_idx], t_idx = t) %>%
    add_constraint(sum_expr(y[p_idx, t_idx], p_idx = Pgk) >= (EK + phi_K) - beta * (1 - b[t_idx]), t_idx = t) %>%
    add_constraint(sum_expr(y[p_idx, t_idx], p_idx = Pgk) <= (EK + phi_K) + beta * (1 - b[t_idx]), t_idx = t) %>% # EK+phi_K = 2

    # Position requirements in starting lineup (4.20-4.22) - These are minimums, OK.
    add_constraint(sum_expr(y[p_idx, t_idx], p_idx = Pdef) >= ED, t_idx = t) %>%
    add_constraint(sum_expr(y[p_idx, t_idx], p_idx = Pmid) >= EM, t_idx = t) %>% # Note: FPL allows 2 mids, Kristiansen uses 3 min
    add_constraint(sum_expr(y[p_idx, t_idx], p_idx = Pfwd) >= EF, t_idx = t) %>%

    # Lineup must be subset of squad (4.24 - 4.26: y[p, t] <= x_freehit[p, t] + lambda[p, t])
    # lambda[p,t] = x[p,t] * (1-r[t]) --> lambda <= x ; lambda <= 1-r ; lambda >= x + (1-r) - 1 = x - r
    add_constraint(y[p_idx, t_idx] <= x_freehit[p_idx, t_idx] + lambda[p_idx, t_idx], p_idx = p, t_idx = t) %>%
    add_constraint(lambda[p_idx, t_idx] <= x[p_idx, t_idx], p_idx = p, t_idx = t) %>%
    add_constraint(lambda[p_idx, t_idx] <= (1 - r[t_idx]), p_idx = p, t_idx = t) %>%
    add_constraint(lambda[p_idx, t_idx] >= x[p_idx, t_idx] - r[t_idx], p_idx = p, t_idx = t)


## Captain and vice-captain constraints (4.27-4.29) ----
# Using simplified c_chip: Captain (f) is always chosen. TC chip (c_chip) boosts points.
model <- model %>%
    # Exactly one captain (4.27 - simplified, no c[p,t])
    add_constraint(sum_expr(f[p_idx, t_idx], p_idx = p) == 1, t_idx = t) %>%
    # Exactly one vice-captain (4.28)
    add_constraint(sum_expr(h[p_idx, t_idx], p_idx = p) == 1, t_idx = t) %>%
    # Captain and vice-captain must be in starting lineup (4.29 - simplified)
    add_constraint(f[p_idx, t_idx] + h[p_idx, t_idx] <= y[p_idx, t_idx], p_idx = p, t_idx = t)


## Substitution constraints (4.30-4.32) ----
# y + sum(g_l) <= squad_player
# squad_player depends on Free Hit: x_fh if r=1, x if r=0
# Combine using lambda: squad_player = x_fh + lambda
model <- model %>%
    # Player is either starting or on bench (with priority) or not in squad (4.30/4.31 combined)
    add_constraint(y[p_idx, t_idx] + sum_expr(g[p_idx, t_idx, l_idx], l_idx = l) <= x_freehit[p_idx, t_idx] + lambda[p_idx, t_idx],
                   p_idx = p, t_idx = t) %>%
    # Max one substitution assignment per non-GK player (4.32)
    add_constraint(sum_expr(g[p_idx, t_idx, l_idx], p_idx = P_not_gk) <= 1, t_idx = t, l_idx = l)


## Budget Constraints (4.33-4.39) ---- REFACTORED COEFFICIENTS
# Using value_coeffs dataframe

# Initial budget constraint (4.33) - GW = 1
model <- model %>%
  add_constraint(
    sum_expr(
      value_coeffs$value[value_coeffs$p == p_idx & value_coeffs$t == 1] * x[p_idx, 1],
      p_idx = p # Sum over all players for GW=1
    ) <= BS - v[1] # Rearranged: cost <= BS - remaining_budget
    # Using <= might be safer if initial budget doesn't have to be exact
    # Use == if the initial state implies v[1] is exactly the leftover amount
  ) %>%
  # Ensure initial budget is non-negative (already done by variable bounds)
  # add_constraint(v[1] >= 0)

# For the remaining gameweeks (4.34-4.39) - Loop required
for (gw_idx in 2:length(T_setofgameweeks)) {
  current_t <- T_setofgameweeks[gw_idx]
  previous_t <- T_setofgameweeks[gw_idx - 1]

  # Budget evolution (4.34) - Only if NOT Free Hit (FH handled separately)
  # v[t] = v[t-1] + sell_value - buy_value
  # This constraint should be conditional on r[t] = 0
  # v[t] <= v[t-1] + sum(value[t]*u[t]) - sum(value[t]*e[t]) + beta*r[t]
  # v[t] >= v[t-1] + sum(value[t]*u[t]) - sum(value[t]*e[t]) - beta*r[t]
  model <- model %>%
    add_constraint(
      v[current_t] <= v[previous_t] +
        sum_expr(value_coeffs$value[value_coeffs$p == p_idx & value_coeffs$t == current_t] * u[p_idx, current_t], p_idx = p) -
        sum_expr(value_coeffs$value[value_coeffs$p == p_idx & value_coeffs$t == current_t] * e[p_idx, current_t], p_idx = p) +
        beta * r[current_t]
    ) %>%
    add_constraint(
      v[current_t] >= v[previous_t] +
        sum_expr(value_coeffs$value[value_coeffs$p == p_idx & value_coeffs$t == current_t] * u[p_idx, current_t], p_idx = p) -
        sum_expr(value_coeffs$value[value_coeffs$p == p_idx & value_coeffs$t == current_t] * e[p_idx, current_t], p_idx = p) -
        beta * r[current_t]
    )

    # Squad continuity (4.35) - Only if NOT Free Hit
    # x[t] = x[t-1] - u[t] + e[t] --> Conditional on r[t] = 0
    # x[t] <= x[t-1] - u[t] + e[t] + r[t]
    # x[t] >= x[t-1] - u[t] + e[t] - r[t]
    model <- model %>%
      add_constraint(
        x[p_idx, current_t] <= x[p_idx, previous_t] - u[p_idx, current_t] + e[p_idx, current_t] + r[current_t],
        p_idx = p
      ) %>%
      add_constraint(
        x[p_idx, current_t] >= x[p_idx, previous_t] - u[p_idx, current_t] + e[p_idx, current_t] - r[current_t],
        p_idx = p
      )

    # Free Hit budget constraint (4.37) - Value of FH squad <= Value of previous squad + budget
    # sum(value[t]*x_fh[t]) <= sum(value[t-1]*x[t-1]) + v[t-1]
    # This should only be active if r[t]=1. Big M needed.
    # sum(value[t]*x_fh[t]) - sum(value[t-1]*x[t-1]) - v[t-1] <= beta*(1-r[t])
    model <- model %>%
      add_constraint(
        sum_expr(value_coeffs$value[value_coeffs$p == p_idx & value_coeffs$t == current_t] * x_freehit[p_idx, current_t], p_idx = p) <=
        sum_expr(value_coeffs$value[value_coeffs$p == p_idx & value_coeffs$t == previous_t] * x[p_idx, previous_t], p_idx = p) +
        v[previous_t] + beta*(1-r[current_t])
      )

    # Free Hit squad reversion: After a FH week t, the squad in t+1 reverts to the squad from t-1
    # This is implicitly handled by 4.35 if we make sure u[t] and e[t] are 0 when r[t]=1.
    # Handled by constraints 4.38/4.39 below.

    # Free Hit transfer restrictions (4.38-4.39)
    # sum(u[t]) <= E * (1 - r[t]) -> If r=1, sum(u)=0. If r=0, sum(u)<=E (loose bound, actual is 15). Use beta?
    # sum(e[t]) <= E * (1 - r[t]) -> If r=1, sum(e)=0. If r=0, sum(e)<=E (loose bound, actual is 15). Use beta?
    # Let's use the total number of players (15) as the Big M here instead of E=11.
    model <- model %>%
      add_constraint(
        sum_expr(u[p_idx, current_t], p_idx = p) <= (MK+MD+MM+MF) * (1 - r[current_t])
      ) %>%
      add_constraint(
        sum_expr(e[p_idx, current_t], p_idx = p) <= (MK+MD+MM+MF) * (1 - r[current_t])
      )
}

# Transfer limitation (4.36) - Can't buy and sell same player in same week
model <- model %>%
  add_constraint(
    e[p_idx, t_idx] + u[p_idx, t_idx] <= 1,
    p_idx = p, t_idx = t
  )


## Transfer Constraints (4.40 - 4.44) ----
# Need to handle t=1 (or start of horizon) and t+1 carefully.
# Assuming the optimization starts at t=1 and we need to decide transfers *for* t=2 onwards.
# q[t] = free transfers *available* at the start of GW t (used for transfers *into* GW t)

# Constraint 4.40: Initial FT for GW2 (transfers INTO GW2). FPL rule: usually 1 FT.
# If starting from GW1, there are no transfers *into* GW1. FTs accumulate *for* GW2.
# The paper seems to index q[t] as FTs available *after* GW t-1 decisions, used for GW t.
# Let's assume q[2] refers to FTs used for transfers between GW1 and GW2.
# If starting fresh, q[2] should be Q_under_bar (1).
# If rolling from previous state, q[2] could be 1 or 2. Let's assume 1 for now.
# The first decision point for transfers is *before* GW2.
model <- model %>%
   add_constraint(q[t[1]] == 0) # No FTs available *at* the very start (GW1)
   # Add constraint for q[2] if length(t) > 1
   if (length(t) > 1) {
       model <- model %>% add_constraint(q[t[2]] == Q_under_bar) # FTs available for transfers into GW2
   }


# Loop for constraints involving t+1
for (gw_idx in 1:(length(T_setofgameweeks) - 1)) {
  current_t <- T_setofgameweeks[gw_idx]
  next_t <- T_setofgameweeks[gw_idx + 1]

  # Constraint 4.41: FT evolution
  # q[t+1] <= E*w[t] + q[t] - sum(e[p,t]) + Q_under_bar + alpha[t] -- THIS SEEMS WRONG.
  # Let's rethink:
  # transfers_made = sum(e[p, t+1], p) -- Transfers *into* t+1
  # ft_used = min(q[t+1], transfers_made)
  # penalty_transfers (alpha[t+1]) = max(0, transfers_made - q[t+1])
  # ft_next (q[t+2]) = min(Q_bar, max(0, q[t+1] - transfers_made) + Q_under_bar)  (If no WC/FH)

  # Kristiansen formulation (modified):
  # q[t+1] relates to transfers *into* gameweek t+1.
  # alpha[t] = penalty transfers for *entering* gameweek t.
  # Let's index transfers e[p,t], u[p,t] as actions taken *before* gameweek t starts.
  # Then sum(e[p, t], p) = transfers made before GW t.
  # q[t] = FT available before GW t.
  # alpha[t] = penalty hits incurred for GW t transfers.
  # q_remaining = max(0, q[t] - sum(e[p,t], p))
  # q[t+1] = min(Q_bar, q_remaining + Q_under_bar) (if no WC/FH)

  # From paper eq 4.41 & 4.44:
  # q[t+1] >= q[t] - sum(e[p,t]) + Q_under_bar - alpha[t] - E*w[t] -- Lower bound? No...
  # q[t+1] <= Q_bar + (Q_u - Q_b)*w[t] + (Q_u - Q_b)*r[t] -- Upper bound reset by WC/FH
  # alpha[t] >= sum(e[p,t]) - q[t] -- Penalty calculation

  # Constraint 4.41 (Number of penalty transfers)
  # alpha[t] >= sum(e[p,t], p) - q[t]
  model <- model %>%
    add_constraint(alpha[next_t] * (1 - w[next_t]) * (1-r[next_t]) >= sum_expr(e[p_idx, next_t], p_idx = p) - q[next_t])

  # Constraint 4.42 (Link alpha to alpha_bar - ensure alpha is non-negative) - Already bounded lb=0

  # Constraint 4.43 & 4.44 (FT evolution)
  # If WC or FH used in t, q[t+1] = Q_under_bar.
  # Else, q[t+1] = min(Q_bar, max(0, q[t] - sum(e[p,t])) + Q_under_bar)
  # Let q_potential = (q[t] - sum(e[p,t])) + Q_under_bar
  # q[t+1] <= Q_bar
  # q[t+1] <= q_potential + beta*(w[t]+r[t])
  # q[t+1] >= q_potential - beta*(w[t]+r[t])
  # q[t+1] >= Q_under_bar - beta*(1-w[t]-r[t]) ??? -- This logic is complex to linearize directly.

  # Let's use the paper's formulation slightly adapted:
  # q[t+1] is the state *before* transfers for t+1 are made.
  # It depends on transfers *into* t (sum(e[t])) and chips *at* t (w[t], r[t]).

  # FT calculation for NEXT week (q[next_t]) based on CURRENT week (current_t) actions.
  # Transfers sum(e[p, current_t]) happen *before* current_t starts.
  # q[current_t] are the FT available for these transfers.
  # alpha[current_t] are the hits taken for these transfers.
  # q_after_transfers = q[current_t] - sum(e[p, current_t])
  # q_carryover = max(0, q_after_transfers) --> q_carryover >= q_after_transfers; q_carryover >= 0
  # q[next_t] = min(Q_bar, q_carryover + Q_under_bar) (if no w[current_t] or r[current_t])
  # If w[current_t] or r[current_t] is 1, then q[next_t] = Q_under_bar.

  # Using auxiliary variable q_carryover[t] >= 0
  # add_variable(q_carryover[t_idx], t_idx=t, type="integer", lb=0)
  # add_constraint(q_carryover[current_t] >= q[current_t] - sum_expr(e[p_idx, current_t], p_idx=p))
  #
  # q[next_t] <= Q_bar
  # q[next_t] <= q_carryover[current_t] + Q_under_bar + beta*(w[current_t] + r[current_t])
  # q[next_t] >= q_carryover[current_t] + Q_under_bar - beta*(w[current_t] + r[current_t])
  # q[next_t] <= Q_under_bar + beta*(1 - w[current_t] - r[current_t]) # This doesn't seem right
  # q[next_t] >= Q_under_bar - beta*(1 - w[current_t] - r[current_t]) # Needs Q_underbar when w=1 or r=1

  # Try simpler formulation from paper (4.44 seems key)
  # q[t+1] <= Q_bar
  # q[t+1] <= Q_under_bar + beta * (w[t] + r[t])  -- If chip used, allows up to Q_under_bar
  # q[t+1] >= Q_under_bar - beta * (1 - w[t] - r[t]) -- If chip used, forces at least Q_under_bar

  model <- model %>%
     # 4.44 Upper bound reset by WC/FH (modified to set to Q_under_bar)
     add_constraint(q[next_t] <= Q_bar) %>% # Always max Q_bar FT
     add_constraint(q[next_t] <= Q_under_bar + beta * (w[current_t] + r[current_t])) %>% # Allow reset downards
     add_constraint(q[next_t] >= Q_under_bar - beta * (1-(w[current_t] + r[current_t]))) # Force reset upwards

     # Need evolution for non-chip weeks
     # q[t+1] <= min(Q_bar, max(0, q[t]-sum(e[t])) + Q_u )
     # add_constraint(q[next_t] <= q[current_t] - sum_expr(e[p_idx, current_t], p_idx=p) + Q_under_bar + beta*(w[current_t] + r[current_t])) # If no chip, upper bound by standard roll-over
     # add_constraint(q[next_t] >= Q_under_bar) # Always at least Q_under_bar (FPL rule, might not apply if q[t]-sum(e) negative) - NO, can be 0 carryover.
     # Need q[t+1] >= 0 handled by variable bound.

     # Let's stick to the paper's 4.41, 4.42, 4.43, 4.44 interpretation for now, might need revisit.
     # 4.41: Relates penalty alpha[t] to transfers e[t] and available q[t] (already added)
     # 4.42: alpha[t] <= alpha_bar * (Q_bar - q[t+1]) ??? This links penalty cost to *next* week's FTs?? Seems odd. Maybe typo in paper?
             # Let's omit 4.42 for now.
     # 4.43: q[t+1] >= Q_under_bar ??? FPL allows carryover of 0. Let's use >= 0. (Handled by variable bound)
     # 4.44: q[t+1] <= Q_bar + (Q_under_bar - Q_bar)*w[t] + (Q_under_bar - Q_bar)*r[t]
             # If w=1 or r=1, q[t+1] <= Q_bar + Q_u - Q_b = Q_u. (Sets max to Q_u when chip used)
             # If w=0, r=0, q[t+1] <= Q_bar (Standard max)
     # This seems more plausible for setting the *maximum* allowed FT.
     add_constraint(
         q[next_t] <= Q_bar + (Q_under_bar - Q_bar)*w[current_t] + (Q_under_bar - Q_bar)*r[current_t]
     )

     # We still need the actual calculation for q[t+1] based on q[t] and e[t] when no chip is played.
     # Maybe 4.41 should define q[t+1]?
     # E*w[t] + q[t] - sum(e[p,t]) + Q_u + alpha[t] >= q[t+1] ???

     # Let's assume the simplest logic: If no WC/FH at t: q[t+1] = min(Q_bar, max(0, q[t] - sum(e[t])) + Q_u)
     # Linearization needed for max(0, ...)
     # Let diff = q[t] - sum(e[t]). Need diff_pos = max(0, diff).
     # Add variable diff_pos[t] >= 0
     # diff_pos[t] >= diff
     # q[t+1] <= Q_bar
     # q[t+1] <= diff_pos[t] + Q_u + beta*(w[t]+r[t])
     # q[t+1] >= diff_pos[t] + Q_u - beta*(w[t]+r[t])

}

# --- Objective Function (Chapter 5) --- REFACTORED COEFFICIENTS
model <- model %>%
  set_objective(
    # Starting lineup points (y)
    sum_expr(points_coeffs$points[points_coeffs$p == p_idx & points_coeffs$t == t_idx] * y[p_idx, t_idx], p_idx = p, t_idx = t) +

    # Captain points bonus (f * points * (1 + 2*c_chip)) --> Need linearization
    # Base points already counted in y. Add bonus points.
    # Bonus = f * points * (1 + 2*c_chip) - f * points = f * points * 2 * c_chip (if c_chip was per player)
    # Simpler: Add f*points if c_chip=0, Add f*points*2 if c_chip=1
    # Bonus = f*points * (1 + c_chip)
    # Let obj_c = f * points * (1+c_chip) = f*points + f*points*c_chip --> Linearize f*c_chip
    # Add variable fc[p,t] = f[p,t]*c_chip[t]
    # fc <= f; fc <= c_chip; fc >= f + c_chip - 1
    # Objective part: sum(points[p,t] * f[p,t]) + sum(points[p,t] * fc[p,t])

    # Add auxiliary variables and constraints for captain bonus linearization
    # add_variable(fc[p_idx, t_idx], p_idx = p, t_idx = t, type = "binary") %>%
    # add_constraint(fc[p_idx, t_idx] <= f[p_idx, t_idx], p_idx = p, t_idx = t) %>%
    # add_constraint(fc[p_idx, t_idx] <= c_chip[t_idx], p_idx = p, t_idx = t) %>%
    # add_constraint(fc[p_idx, t_idx] >= f[p_idx, t_idx] + c_chip[t_idx] - 1, p_idx = p, t_idx = t) %>%

    # Add captain points to objective:
    sum_expr(points_coeffs$points[points_coeffs$p == p_idx & points_coeffs$t == t_idx] * f[p_idx, t_idx], p_idx = p, t_idx = t) + # Base captain points (double)
    # Add Triple Captain points bonus (additional points if c_chip active)
    sum_expr(points_coeffs$points[points_coeffs$p == p_idx & points_coeffs$t == t_idx] * f[p_idx, t_idx] * c_chip[t_idx], p_idx = p, t_idx = t) + # THIS IS NON-LINEAR -> Needs linearization (fc variable)
    # sum_expr(points_coeffs$points[points_coeffs$p == p_idx & points_coeffs$t == t_idx] * fc[p_idx, t_idx], p_idx = p, t_idx = t) + # Linearized TC bonus


    # Vice-captain points (epsilon * h * points) - Small contribution if captain doesn't play (model doesn't check this)
    sum_expr(epsilon * points_coeffs$points[points_coeffs$p == p_idx & points_coeffs$t == t_idx] * h[p_idx, t_idx], p_idx = p, t_idx = t) +

    # Substitution points (kappa * g * points) - Contribution if starters don't play (model doesn't check this)
    sum_expr(kappa[1] * points_coeffs$points[points_coeffs$p == p_idx & points_coeffs$t == t_idx] * g[p_idx, t_idx, 1], p_idx = p, t_idx = t) +
    sum_expr(kappa[2] * points_coeffs$points[points_coeffs$p == p_idx & points_coeffs$t == t_idx] * g[p_idx, t_idx, 2], p_idx = p, t_idx = t) +
    sum_expr(kappa[3] * points_coeffs$points[points_coeffs$p == p_idx & points_coeffs$t == t_idx] * g[p_idx, t_idx, 3], p_idx = p, t_idx = t) -

    # Transfer penalties (R * alpha)
    sum_expr(R * alpha[t_idx], t_idx = t),

    sense = "max"
  )

# --- Introduce the linearization for Triple Captain ---
model <- model %>%
    add_variable(fc[p_idx, t_idx], p_idx = p, t_idx = t, type = "binary") %>%
    add_constraint(fc[p_idx, t_idx] <= f[p_idx, t_idx], p_idx = p, t_idx = t) %>%
    add_constraint(fc[p_idx, t_idx] <= c_chip[t_idx], p_idx = p, t_idx = t) %>% # Indexing t_idx only for c_chip
    add_constraint(fc[p_idx, t_idx] >= f[p_idx, t_idx] + c_chip[t_idx] - 1, p_idx = p, t_idx = t) # Indexing t_idx only for c_chip

# --- RE-SET Objective Function with linearized Triple Captain Bonus ---
model <- model %>%
  set_objective(
    sum_expr(points_coeffs$points[points_coeffs$p == p_idx & points_coeffs$t == t_idx] * y[p_idx, t_idx], p_idx = p, t_idx = t) + # Starting points
    sum_expr(points_coeffs$points[points_coeffs$p == p_idx & points_coeffs$t == t_idx] * f[p_idx, t_idx], p_idx = p, t_idx = t) + # Captain bonus (1x points)
    sum_expr(points_coeffs$points[points_coeffs$p == p_idx & points_coeffs$t == t_idx] * fc[p_idx, t_idx], p_idx = p, t_idx = t) + # Triple Captain bonus (additional 1x points if c_chip=1)
    sum_expr(epsilon * points_coeffs$points[points_coeffs$p == p_idx & points_coeffs$t == t_idx] * h[p_idx, t_idx], p_idx = p, t_idx = t) + # Vice points
    sum_expr(kappa[1] * points_coeffs$points[points_coeffs$p == p_idx & points_coeffs$t == t_idx] * g[p_idx, t_idx, 1], p_idx = p, t_idx = t) + # Sub1 points
    sum_expr(kappa[2] * points_coeffs$points[points_coeffs$p == p_idx & points_coeffs$t == t_idx] * g[p_idx, t_idx, 2], p_idx = p, t_idx = t) + # Sub2 points
    sum_expr(kappa[3] * points_coeffs$points[points_coeffs$p == p_idx & points_coeffs$t == t_idx] * g[p_idx, t_idx, 3], p_idx = p, t_idx = t) - # Sub3 points
    sum_expr(R * alpha[t_idx], t_idx = t), # Transfer cost
    sense = "max"
  )


# --- Solve Model ---
# Try GLPK first as it's generally robust for MILP if the model is correct
solver <- "glpk"
# solver <- "symphony" # Requires ROI.plugin.symphony
# solver <- "lpsolve" # Requires ROI.plugin.lpsolve

print(paste("Attempting to solve with:", solver))
# Add a time limit if the model is large/complex
# result <- solve_model(model, with_ROI(solver = solver, control = list(tm_limit = 1000 * 60 * 5))) # 5 min timeout
result <- tryCatch({
    solve_model(model, with_ROI(solver = solver))
}, error = function(e) {
    print(paste("Error during solving with", solver, ":"))
    print(e)
    # You can add more debugging here, like writing the model to a file if the solver supports it
    # ROI_write(model, "failed_model.lp", type="lp") # Requires specific solver support in ROI
    return(NULL) # Return NULL on error
})

# --- Check Solution ---
if (!is.null(result)) {
    print(paste("Solver status:", result$status))
    if (result$status == "optimal" || result$status == "feasible" || grepl("optimal", result$status, ignore.case=TRUE)) {
        cat("Model solved successfully with status:", result$status, "\n")
        cat("Objective value:", result$objective_value, "\n")

        # --- Extract and display results (Example for GW 1) ---
        gw_to_show = 1
        solution <- get_solution(result, x[p_idx, t_idx]) %>% filter(value > 0.9, t_idx == gw_to_show)
        starting <- get_solution(result, y[p_idx, t_idx]) %>% filter(value > 0.9, t_idx == gw_to_show)
        captain <- get_solution(result, f[p_idx, t_idx]) %>% filter(value > 0.9, t_idx == gw_to_show)
        vicecap <- get_solution(result, h[p_idx, t_idx]) %>% filter(value > 0.9, t_idx == gw_to_show)
        # subs <- get_solution(result, g[p_idx, t_idx, l_idx]) %>% filter(value > 0.9, t_idx == gw_to_show)
        budget_left <- get_solution(result, v[t_idx]) %>% filter(t_idx == gw_to_show)
        transfers_in <- get_solution(result, e[p_idx, t_idx]) %>% filter(value > 0.9, t_idx == gw_to_show+1) # Transfers for next GW
        transfers_out <- get_solution(result, u[p_idx, t_idx]) %>% filter(value > 0.9, t_idx == gw_to_show+1)# Transfers for next GW
        free_transfers <- get_solution(result, q[t_idx]) %>% filter(t_idx == gw_to_show+1)
        penalty_hits <- get_solution(result, alpha[t_idx]) %>% filter(t_idx == gw_to_show+1)

        print(paste("--- Results for GW", gw_to_show, "---"))
        print("Squad:")
        print(left_join(solution, data_prepared %>% filter(GW == gw_to_show) %>% select(player_id, name, team, position, value), by = c("p_idx"="player_id")))
        print("Starting XI:")
        print(left_join(starting, data_prepared %>% filter(GW == gw_to_show) %>% select(player_id, name, team, position), by = c("p_idx"="player_id")))
        print("Captain:")
        print(left_join(captain, data_prepared %>% filter(GW == gw_to_show) %>% select(player_id, name), by = c("p_idx"="player_id")))
        print("Vice-Captain:")
        print(left_join(vicecap, data_prepared %>% filter(GW == gw_to_show) %>% select(player_id, name), by = c("p_idx"="player_id")))
        print(paste("Budget Remaining:", budget_left$value[1]))

        if(gw_to_show < max(t)) {
             print(paste("--- Transfers planned for GW", gw_to_show + 1, "---"))
             print(paste("Free Transfers Available:", free_transfers$value[1]))
             print(paste("Penalty Hits:", penalty_hits$value[1] * R))
             print("Transfers Out:")
             print(left_join(transfers_out, data_prepared %>% filter(GW == gw_to_show) %>% select(player_id, name, value), by = c("p_idx"="player_id")))
             print("Transfers In:")
             print(left_join(transfers_in, data_prepared %>% filter(GW == gw_to_show+1) %>% select(player_id, name, value), by = c("p_idx"="player_id")))
        }


    } else {
        cat("Failed to find optimal/feasible solution. Status:", result$status, "\n")
        # Potentially inspect solver output if available in result$message or similar
    }
} else {
    cat("Model solving failed with an error.\n")
}

solver_status(result)
solution <- solve_model(model, with_ROI(solver = solver))
solution

print(model)  # Check total number of constraints
