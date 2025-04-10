# Blandet heltalls modell  ----
# Basert på Kristiansen et al. # NOTE! ALL EQUATIONS AND TABLE NUMBERS REFER TO THEIR THESIS
# THIS MIGHT BE CHANGED AT A LATER TIME, BUT IS KEPT LIKE THIS UNDER PROD. TO KEEP EVERYTHING 
# "OVERSIKTLIG" -PER 30.03
rm(list = ls(all = TRUE))
# Pakker
library(tidyverse)
library(ompr); library(ompr.roi); library(ROI.plugin.glpk)
#citation("keras3");citation("tensorflow");citation("ompr")

# Forsøker på en sesong først for å se om det fungerer, Den nyeste sesongen er minst
allesesonger <- read_csv("Stigende GW, alle tre sesonger(22-24).csv")

#length(unique(allesesonger$name)) == length(unique(allesesonger$player_id))

# En hel sesong tar veldig lang tid så derfor prøver jeg først med 3 uker
data <- allesesonger |> filter(GW <= 3)
#data |> filter(name == "Mohamed Salah") |> select(GW, name, player_id, team, position, total_points) # Sjekker at data er riktig

# Set Table 4.1 Kristiansen et al. 2018
T_setofgameweeks <- unique(data$GW) # Sett med gameweeks
P_setofplayers <- unique(data$player_id) # Sett med spillere
C_setofteams <- unique(data$team) # Sett med lag
L_substitution <- c(1:3) # Ubytter rekkefølge, er bare 3 innbyttere 

# Subset  Table 4.2 Kristiansen et al. 2018
Pdef <- unique(data$player_id[data$position == "DEF"]) # Sett Forsvarsplillere
Pmid <- unique(data$player_id[data$position == "MID"]) # Sett Midtbane -||-
Pfwd <- unique(data$player_id[data$position == "FWD"]) # Sett Angreps-||-
Pgk <- unique(data$player_id[data$position == "GK"]) # Sett målvakts-||-
P_not_gk <- unique(data$player_id[data$position != "GK"]) # Sett spillere som ikke er målvakt til constraint 4.3

P_c <- split(data$player_id, data$team)  # Sett med spillere etter lag c

medianavgameweeks <- median(T_setofgameweeks) # Midt i sesongen
T_FH <- T_setofgameweeks[T_setofgameweeks <= medianavgameweeks] # Første halvdel av datasettet
T_SH <- T_setofgameweeks[T_setofgameweeks > medianavgameweeks] # Andre halvdel av datasettet

## Index Table 4.3 Kristiansen et al. 2018 -----

t <- T_setofgameweeks# Gameweek
p <- P_setofplayers # Spillere
l <- L_substitution # Ubytter rekkefølge

## Parametre Table 4.4 Kristiansen et al. 2018 -----
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
BS <- 1000.0           # Start budsjett, i value kolonnen er det satt *10 
phi <- (MK+MD+MM+MF) - (E+EK) # Number of substitutes on bench
phi_K <- MK - EK        # Number of GK substitutes
Q_bar <- 4            # Max number of free transfers to accumulate
Q_under_bar <- 1                # Free transfers given per gameweek 
 
## Konstant parametre -----
epsilon <- 0.1  #  eps << 1
kappa <- c(0.01, 0.005, 0.001) # Weights for subs priorities (3 values for 3 substitutes)
beta <- length(P_setofplayers)     # Tilstrekkelig høy konstant??? 
alpha_bar <- length(P_setofplayers) # Tilstrekkelig høy konstant???

# Variabler Tabell 4.5 Kristiansen et al. 2018 ----
# Initialiser modellen
model <- MILPModel() |>
  
  # ---- Binære variabler (Tabell 4.5) ----
  # Squad selection (x_pt)
  add_variable(x[p, t], p = p, t = t, type = "binary") %>%
  
  # Free Hit squad (x_pt^freehit)
  add_variable(x_freehit[p, t], p = p, t = t, type = "binary") %>%
  
  # Starting line-up (y_pt)
  add_variable(y[p, t], p = p, t = t, type = "binary") %>%
  
  # Captain (f_pt)
  add_variable(f[p, t], p = p, t = t, type = "binary") %>%
  
  # Vice-captain (h_pt)
  add_variable(h[p, t], p = p, t = t, type = "binary") %>%
  
  # Substitutionsprioritet (g_ptl)
  add_variable(g[p_ngk, t, l], p_ngk = P_not_gk, t = t, l = L_substitution, type = "binary") %>%
  #add_variable(g[p, t, l], p = p, t = t, l = L_substitution, type = "binary") %>%
  
  # Transfers out (u_pt)
  add_variable(u[p, t], p = p, t = t, type = "binary") %>%
  
  # Transfers in (e_pt)
  add_variable(e[p, t], p = p, t = t, type = "binary") %>%
  
  # Wildcard (w_t)
  add_variable(w[t], t = t, type = "binary") %>%
  
  # Triple Captain (c_pt)
  add_variable(c[p, t], p = p, t = t, type = "binary") %>%
  
  # Bench Boost (b_t)
  add_variable(b[t], t = t, type = "binary") %>%
  
  # Free Hit (r_t)
  add_variable(r[t], t = t, type = "binary") %>%
  
  # Binær "Auxiliary" variabel (lambda_pt)
  add_variable(lambda[p, t], p = p, t = t, type = "binary") %>%

  # ---- Kontinuerlige/heltallsvariabler ----
  # Gjenværende budsjett (v_t)
  add_variable(v[t], t = t, type = "continuous", lb = 0) %>%
  
  # Gratis overganger (q_t)
  add_variable(q[t], t = t, type = "integer", lb = 0, ub = Q_bar) %>%
  
  # Straffeoverganger (alpha_t)
  add_variable(alpha[t], t = t, type = "integer", lb = 0)


# Constrains ----

## Gamechips (4.2 - 4.6)
model <- model %>%
  
  # ---- Gamechips (4.2 - 4.6) ----
  # Wildcard (w_t)
  add_constraint(sum_expr(w[t], t = T_FH) <= 1) %>% # First
  
  add_constraint(sum_expr(w[t], t = T_SH) <= 1) %>% # Second
  
  # Triple Captain (c_pt) - FIXED
  add_constraint(sum_expr(c[p, t], p = P_setofplayers, t = T_setofgameweeks) <= 1) %>%
  
  # Bench Boost (b_t)
  add_constraint(sum_expr(b[t], t = T_setofgameweeks) <= 1) %>%
  
  # Free Hit (r_t)
  add_constraint(sum_expr(r[t], t = T_setofgameweeks) <= 1) %>%

  # Ensure only one GC per week - FIXED
  add_constraint(w[t] + sum_expr(c[p, t], p = P_setofplayers) + b[t] + r[t] <= 1, 
                t = T_setofgameweeks)


##Selected squad constraints (4.8-4.17) ----
model <- model %>%
    # Position requirements 4.8-4.11
    add_constraint(sum_expr(x[p, t], p = Pgk, t = t) == MK, t = t) %>% # Exactly 2 GK
    add_constraint(sum_expr(x[p, t], p = Pdef, t = t) == MD, t = t) %>% # Exactly 5 DEF
    add_constraint(sum_expr(x[p, t], p = Pmid, t = t) == MM, t = t) %>% # Exactly 5 MID
    add_constraint(sum_expr(x[p, t], p = Pfwd, t = t) == MF, t = t)  # Exactly 3 FWD


# Remove the problematic team constraint
# add_constraint(
#     sum_expr(x[p, t], p = P_c[[c]]) <= MC,
#     c = C_setofteams, t = t
# )

    # Team limit constraints (max 3 players from each team) 4.12
    for (team_name in C_setofteams) {
      team_players <- P_c[[team_name]]
      model <- model %>%
        add_constraint(
          sum_expr(x[p, t], p = team_players) <= MC,
          t = t
        )
    } 

model <- model |>    
    # Free Hit Only Constraints 4.13 - 4.17
    #add_constraint(sum_expr(x_freehit[p, t], p = p) == MK*r[t], t = t) %>% 
    #add_constraint(sum_expr(x_freehit[p, t], p = p) == MD*r[t], t = t) %>% 
    #add_constraint(sum_expr(x_freehit[p, t], p = p) == MM*r[t], t = t) %>% 
    #add_constraint(sum_expr(x_freehit[p, t], p = p) == MF*r[t], t = t)
add_constraint(sum_expr(x_freehit[p_gk, t], p_gk = Pgk) == MK * r[t], t = t) %>%
add_constraint(sum_expr(x_freehit[p_def, t], p_def = Pdef) == MD * r[t], t = t) %>%
add_constraint(sum_expr(x_freehit[p_mid, t], p_mid = Pmid) == MM * r[t], t = t) %>%
add_constraint(sum_expr(x_freehit[p_fwd, t], p_fwd = Pfwd) == MF * r[t], t = t) %>%
# Also add the total size constraint (optional but good practice)
add_constraint(sum_expr(x_freehit[p, t], p = p) == (MK + MD + MM + MF) * r[t], t = t) %>%

# Then add team constraints in a separate loop
for (team_name in C_setofteams) {
  team_players <- P_c[[team_name]]
  model <- model %>%
    add_constraint(
      sum_expr(x_freehit[p, t], p = team_players) <= MC*r[t],
      t = t
    )
} 

## Starting Line-up Constraints (4.18-4.22) ----
model <- model |> 
    # Starting lineup constraints 4.18-4.19
    add_constraint(sum_expr(y[p, t], p = p) == E + phi*b[t], t = t) |> # Antall spillere i XI, med unntak hvis Bench Bust er aktivert da legges till subsa
    add_constraint(sum_expr(y[p, t], p = Pgk) == EK + phi_K*b[t], t = t) |> # Samma som over bare for keepere, dvs. 1 keeper på, een av
  
    # Position requirements in starting lineup 4.20-4.22
    add_constraint(sum_expr(y[p, t], p = Pdef) >= ED, t = t) %>% # At least 3 DEF starting
    add_constraint(sum_expr(y[p, t], p = Pmid) >= EM, t = t) %>% # At least 3 MID starting
    add_constraint(sum_expr(y[p, t], p = Pfwd) >= EF, t = t) %>% # At least 1 FWD sta
    
    # 4.23 ( Hvis jeg hadde lest ordentlig hva de mener i teksten under 4.23 så hadde jeg spart mye tid...)
    # add_constraint(y[p,t] <= x_freehit[p, t] + x[p, t]*(1-r[t]), p = p, t = t) %>% # Starting lineup must be in squad 
    
    # Free hit contrainst 4.24 - 4.26
    add_constraint(y[p, t] <= x_freehit[p, t] + lambda[p, t], p = p, t = t) %>% 
    add_constraint(lambda[p, t] <= x[p, t], p = p, t = t) %>% 
    add_constraint(lambda[p, t] <= (1-r[t]), p = p, t = t)

## Captain and vice-captain constraints (4.27-4.29) ----
model <- model |> 
    #add_constraint(sum_expr(f[p, t]) + sum_expr(c[p,t])== 1,p = p, t = t) %>% # Only one cap per gw
    #add_constraint(sum_expr(h[p, t], p = p) == 1, t = t) %>% # Samma som over for Vise kap
    #add_constraint(f[p, t] + c[p, t] + h[p, t] <= y[p, t], p = p, t = t) # Either cap, vc or 3xcap in one gw
# 4.27: One Captain OR Triple Captain per week
add_constraint(sum_expr(f[p, t], p = p) + sum_expr(c[p, t], p = p) == 1, t = t) %>%
# 4.28: One Vice-Captain per week
add_constraint(sum_expr(h[p, t], p = p) == 1, t = t) %>%
# 4.29: Captaincy only for starting players
add_constraint(f[p, t] + c[p, t] + h[p, t] <= y[p, t], p = p, t = t) %>%


## Substitution constraints (4.30-4.32) ----
model <- model |>
    #add_constraint( y[p, t] + sum_expr(g[p, t, l], l = l) <= x[p, t] + beta*r[t], p = P_not_gk, t = t) %>% #4.30
    #add_constraint( y[p, t] + sum_expr(g[p, t, l], l = l) <= x_freehit[p, t] + beta*(1-r[t]), p = P_not_gk, t = t) %>% #4.31
    #add_constraint(sum_expr(g[p, t, l] ,p = P_not_gk) <= 1, t = t, l = l) #4.32
# Iterate over non-goalkeepers for these constraints
add_constraint( y[p_ngk, t] + sum_expr(g[p_ngk, t, l], l = l) <= x[p_ngk, t] + beta*r[t], p_ngk = P_not_gk, t = t) %>% # 4.30
add_constraint( y[p_ngk, t] + sum_expr(g[p_ngk, t, l], l = l) <= x_freehit[p_ngk, t] + beta*(1-r[t]), p_ngk = P_not_gk, t = t) %>% # 4.31
add_constraint(sum_expr(g[p_ngk, t, l], p_ngk = P_not_gk) <= 1, t = t, l = l) # 4.32
## Budget Constraints # 4.33-4.36 !!!! MULIG FEIL


#CBSpt <- data |> select(player_id, GW, value)
#head(CBSpt, 1)

#model <- model |> 
#    add_constraint(BS - sum_expr(CBSpt*x[p,t], p = p) == v[t], t = 1) |>#4.33
#    add_constraint(v[t-1] + sum_expr(CBSpt[p, t]*u[p, t], p = p) - sum_expr(CSBpt[p, t]*e[p, t], p = p) == v[t], t = 2:length(T_setofgameweeks)) |>#4.34
#    add_constraint(x[p, t-1] + e[p, t] - u[p, t] == x[p, t], p = p, t = 2:length(T_setofgameweeks)) |>  #4.35
#    add_constraint(e[p, t] + u[p, t] <= 1, p = p, t = t) |>  # 4.36
#    add_constraint(sum_expr(CBSpt*x[p, t-1] + v[t-1] >= sum_expr(CBSpt[p, t]*x_freehit[p,t], t = 2:length(T_setofgameweeks), p = p))) |> # 4.37
#    add_constraint(sum_expr(u[p, t] <= E(1-r[t]), p = p, t = 2:length(T_setofgameweeks))) |># 4.38
#    add_constraint(sum_expr(e[p, t] <= E(1-r[t]), p = p, t = 2:length(T_setofgameweeks)))# 4.39
# Fungerer ikke

# First, create player-gameweek combinations
player_gw_combos <- expand.grid(
  player_id = P_setofplayers,
  GW = T_setofgameweeks
)

# Join with data to get values
player_gw_values <- merge(
  player_gw_combos,
  data[, c("player_id", "GW", "value")],
  by = c("player_id", "GW"),
  all.x = TRUE
)

# Replace NA values with a default (5.0)
player_gw_values$value[is.na(player_gw_values$value)] <- 5.0

# Create indices for coefficient vectors
player_gw_indices <- paste0(player_gw_values$player_id, "_", player_gw_values$GW)

# Create coefficient vectors for each GW
player_values <- player_gw_values$value
names(player_values) <- player_gw_indices

# Initial budget constraint (4.33)
model <- model %>% 
  add_constraint(
    BS - sum_expr(
      player_values[paste0(p, "_", 1)] * x[p, 1],
      p = p
    ) == v[1]
  )

# For the remaining gameweeks (4.34-4.39)
for (gw in T_setofgameweeks) {
  if (gw > 1) {
    # Budget evolution (4.34)
    model <- model %>%
      add_constraint(
        v[gw-1] + 
        sum_expr(
          player_values[paste0(p, "_", gw)] * u[p, gw],
          p = p
        ) - 
        sum_expr(
          player_values[paste0(p, "_", gw)] * e[p, gw],
          p = p
        ) == v[gw]
      )
    
    # Squad continuity (4.35)
    model <- model %>%
      add_constraint(
        x[p, gw] == x[p, gw-1] - u[p, gw] + e[p, gw],
        p = p
      )
    
    # Free Hit budget constraint (4.37) Reversed
    model <- model %>%
      add_constraint(
        sum_expr(
          player_values[paste0(p, "_", gw)] * x_freehit[p, gw],
          p = p
        ) <= 
        sum_expr(
          player_values[paste0(p, "_", gw-1)] * x[p, gw-1],
          p = p
        ) + v[gw-1]
      )
    
    # Free Hit transfer restrictions (4.38-4.39)
    model <- model %>%
      add_constraint(
        sum_expr(u[p, gw], p = p) <= E * (1 - r[gw])
      ) %>%
      add_constraint(
        sum_expr(e[p, gw], p = p) <= E * (1 - r[gw])
      )
  }
}

# Transfer limitation (4.36) - This applies to all gameweeks
model <- model %>%
  add_constraint(
    e[p, t] + u[p, t] <= 1,
    p = p, t = t
  )

# Transfer Constraints 4.40 - 4.44
 #model <- model |>
 #   add_constraint(q[2] == Q_under_bar) |> # 4.40
 #   add_constraint(E*w[t] + q[t]-sum_expr(e[p,t], p = p) + Q_under_bar + alpha[t] >= q[t+1], t = 2:T_setofgameweeks) |> # 4.41
 #   add_constraint(alpha_bar*(Q_bar-q[t+1]) >= alpha[t], t = t) |> # 4.42
 #   add_constraint(q[t+1] >= Q_under_bar, t = t) |> # 4.43
 #   add_constraint(q[t+1] <= Q_bar + (Q_under_bar - Q_bar)*w[t] + (Q_under_bar - Q_bar)*r[t], t = t) # 4.44
# Fungerer ikke

# Fixed ranges for transfer constraints
model <- model |>
  add_constraint(q[2] == Q_under_bar) |> # 4.40
  
  # Apply constraints with t+1 only for appropriate gameweeks
  add_constraint(
    E*w[t] + q[t] - sum_expr(e[p,t], p = p) + Q_under_bar + alpha[t] >= q[t+1], 
    t = 1:(length(T_setofgameweeks)-1)
  ) |> # 4.41
  
  add_constraint(
    alpha_bar * (Q_bar - q[t+1]) >= alpha[t], 
    t = 1:(length(T_setofgameweeks)-1)
  ) |> # 4.42
  
  add_constraint(
    q[t+1] >= Q_under_bar, 
    t = 1:(length(T_setofgameweeks)-1)
  ) |> # 4.43
  
  add_constraint(
    q[t+1] <= Q_bar + (Q_under_bar - Q_bar)*w[t] + (Q_under_bar - Q_bar)*r[t], 
    t = 1:(length(T_setofgameweeks)-1)
  ) # 4.44

# Obj. Function Ch-5

# Create player-gameweek points combinations
player_points_values <- merge(
  player_gw_combos,
  data[, c("player_id", "GW", "total_points")],
  by = c("player_id", "GW"),
  all.x = TRUE
)

# Replace NA values with a default (0.0)
player_points_values$total_points[is.na(player_points_values$total_points)] <- 0.0

# Create indices for coefficient vectors
player_points_indices <- paste0(player_points_values$player_id, "_", player_points_values$GW)

# Create coefficient vectors for points
player_points <- player_points_values$total_points
names(player_points) <- player_points_indices

# Build the full objective function at once
model <- model %>%
  set_objective(
    # Starting lineup points
    sum_expr(player_points[paste0(p, "_", t)] * y[p, t], p = p, t = t) + 
    
    # Captain points
    sum_expr(player_points[paste0(p, "_", t)] * f[p, t], p = p, t = t) + 
    
    # Vice-captain points
    sum_expr(epsilon * player_points[paste0(p, "_", t)] * h[p, t], p = p, t = t) + 
    
    # Triple captain points
    sum_expr(2 * player_points[paste0(p, "_", t)] * c[p, t], p = p, t = t) + 
    
    # Sub points (priority 1)
    sum_expr(kappa[1] * player_points[paste0(p, "_", t)] * g[p_ngk, t, 1], p_ngk = P_not_gk, t = t) + 
    
    # Sub points (priority 2)
    sum_expr(kappa[2] * player_points[paste0(p, "_", t)] * g[p_ngk, t, 2], p_ngk = P_not_gk, t = t) + 
    
    # Sub points (priority 3)
    sum_expr(kappa[3] * player_points[paste0(p, "_", t)] * g[p_ngk, t, 3], p_ngk = P_not_gk, t = t)  
    
    # Transfer penalties
    - R * sum_expr(alpha[t], t = t),
    
    sense = "max"
  )

print(model)  # Check total number of constraints

# --- Solve the model ---
result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE)) # Keep verbose=TRUE for solver logs

# --- Extract and Print Results ---
if (result$status == "optimal" || result$status == "success") {

  print(paste("Solver Status:", result$status))
  print(paste("Objective Value:", result$objective_value))

  # Get player info for mapping IDs to names
  player_info <- data %>%
    distinct(player_id, .keep_all = TRUE) %>%
    select(player_id, name, position, team)

  # Loop through gameweeks to show results
  for (t_gw in t) {
    print(paste("\n--- Gameweek", t_gw, "Results ---"))

    # Extract Squad (x variables = 1)
    # The columns in the output of get_solution match the index names: 'p', 't'
    squad_solution <- get_solution(result, x[p, t_gw]) %>% # Use 'p' as the index name
      filter(value > 0.9) %>%
      select(p) %>% # Select the column named 'p'
      mutate(p = as.numeric(p)) %>% # Ensure player_id is numeric for joining
      left_join(player_info, by = c("p" = "player_id")) # Join using the 'p' column

    print("Squad:")
    print(squad_solution)

    # Extract Lineup (y variables = 1)
    lineup_solution <- get_solution(result, y[p, t_gw]) %>% # Use 'p'
      filter(value > 0.9) %>%
      select(p) %>% # Use 'p'
      mutate(p = as.numeric(p)) %>%
      left_join(player_info, by = c("p" = "player_id"))

    print("Lineup:")
    print(lineup_solution)

    # Extract Captain (f variables = 1)
    captain_solution <- get_solution(result, f[p, t_gw]) %>% # Use 'p'
      filter(value > 0.9) %>%
      select(p) %>% # Use 'p'
      mutate(p = as.numeric(p)) %>%
      left_join(player_info, by = c("p" = "player_id"))

    print("Captain:")
    if(nrow(captain_solution) > 0) print(captain_solution) else print("None")

    # Extract Vice-Captain (h variables = 1)
    vice_captain_solution <- get_solution(result, h[p, t_gw]) %>% # Use 'p'
      filter(value > 0.9) %>%
      select(p) %>% # Use 'p'
      mutate(p = as.numeric(p)) %>%
      left_join(player_info, by = c("p" = "player_id"))

    print("Vice-Captain:")
     if(nrow(vice_captain_solution) > 0) print(vice_captain_solution) else print("None")

    # Extract Transfers (e and u variables = 1)
     transfers_in_solution <- get_solution(result, e[p, t_gw]) %>% # Use 'p'
       filter(value > 0.9) %>%
       select(p) %>% # Use 'p'
       mutate(p = as.numeric(p)) %>%
       left_join(player_info, by = c("p" = "player_id"))

     print("Transfers In:")
     if(nrow(transfers_in_solution) > 0) print(transfers_in_solution) else print("None")

     transfers_out_solution <- get_solution(result, u[p, t_gw]) %>% # Use 'p'
       filter(value > 0.9) %>%
       select(p) %>% # Use 'p'
       mutate(p = as.numeric(p)) %>%
       left_join(player_info, by = c("p" = "player_id"))

     print("Transfers Out:")
     if(nrow(transfers_out_solution) > 0) print(transfers_out_solution) else print("None")

    # Extract Budget (v variable) - Index is 't'
    budget_solution <- get_solution(result, v[t_gw]) %>%
       select(value) # Just get the value

    print("Remaining Budget:")
    if(nrow(budget_solution) > 0) print(budget_solution$value) else print("N/A")

    # Extract Penalized Transfers (alpha variable) - Index is 't'
    alpha_solution <- get_solution(result, alpha[t_gw]) %>%
       select(value) # Just get the value

    print("Penalized Transfers (alpha):")
    if(nrow(alpha_solution) > 0) print(round(alpha_solution$value)) else print("N/A") # Round alpha

  }

} else {
  print(paste("Solver failed with status:", result$status))
}
