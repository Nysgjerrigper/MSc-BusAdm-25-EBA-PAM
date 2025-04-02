import pandas as pd
import pulp
import numpy as np
import time
import sys
import os
import threading
# Med start GW
# --- Input Parameters ---
START_GAMEWEEK = 9 # <--- ADD THIS: Set to the desired starting gameweek (e.g., 1, 5, etc.)
MAX_GAMEWEEK = 10 # Set to the desired maximum gameweek (e.g., 38 for full season)
CSV_FILE_PATH = "C:/Users/peram/Documents/test/Differensiert gw alle tre sesonger(22-24), heltall.csv"
SOLVER_TIME_LIMIT = 15 # Seconds (e.g., 3600 for 1 hour) or None for no limit
PROGRESS_UPDATE_INTERVAL = 120 # Seconds

# --- Check Versions ---
print(f"--- Running on Python {sys.version} ---")
print(f"--- PuLP Version: {pulp.__version__} ---")

# --- 0. Setup & Data Loading ---
print("\n--- 0. Setup & Data Loading ---")
try:
    allesesonger = pd.read_csv(CSV_FILE_PATH)
    print(f"CSV '{CSV_FILE_PATH}' loaded successfully.")
except FileNotFoundError:
    print(f"ERROR: CSV file '{CSV_FILE_PATH}' not found.")
    exit()
except Exception as e:
    print(f"ERROR: Failed to load CSV file. Reason: {e}")
    exit()

print("Initial data shape:", allesesonger.shape)
print("Initial columns:", allesesonger.columns.tolist())

# --- Data Pre-processing and Filtering ---
print("\n--- Pre-processing Data ---")
essential_input_cols = ['player_id', 'GW', 'name', 'position', 'team', 'total_points', 'value']
missing_cols = [col for col in essential_input_cols if col not in allesesonger.columns]
if missing_cols:
    print(f"ERROR: Missing essential columns in the CSV: {missing_cols}")
    exit()

# Ensure GW is numeric
try:
    allesesonger['GW'] = pd.to_numeric(allesesonger['GW'])
except ValueError:
    print("ERROR: Could not convert 'GW' column to numeric.")
    exit()

# Filter Gameweeks
# Filter Gameweeks
print(f"Filtering data for GW >= {START_GAMEWEEK} and GW <= {MAX_GAMEWEEK}...") # Optional: Update print statement
data_raw = allesesonger[(allesesonger['GW'] >= START_GAMEWEEK) & (allesesonger['GW'] <= MAX_GAMEWEEK)].copy()
if data_raw.empty:
    print(f"ERROR: No data found for GW <= {MAX_GAMEWEEK}.")
    exit()
print(f"Filtered data for GW <= {MAX_GAMEWEEK}. Shape: {data_raw.shape}")

# Clean categorical columns
for col in ['team', 'position', 'name']:
    if col in data_raw.columns:
        data_raw[col] = data_raw[col].astype(str).str.strip()

# --- 1. Define Initial Sets and Perform Data Cleaning/Validation ---
print("\n--- 1. Data Cleaning & Set Definition ---")

# Define initial sets
T_setofgameweeks = sorted(data_raw['GW'].unique())
P_setofplayers_initial = sorted(data_raw['player_id'].unique())
C_setofteams_initial = sorted(data_raw['team'].dropna().unique())
L_substitution = list(range(1, 4)) # Priorities 1, 2, 3

n_T = len(T_setofgameweeks)
n_P_initial = len(P_setofplayers_initial)

if n_T == 0 or n_P_initial == 0:
    print("ERROR: Initial Gameweek or Player set is empty after filtering.")
    exit()
print(f"Initial sets: {n_T} Gameweeks, {n_P_initial} Players, {len(C_setofteams_initial)} Teams.")

# Create all player-GW combinations
player_gw_combos = pd.MultiIndex.from_product([P_setofplayers_initial, T_setofgameweeks],
                                              names=['player_id', 'GW'])
data_complete = pd.DataFrame(index=player_gw_combos).reset_index()

# Merge and fill missing data
print("Merging and filling missing data...")
data_merged = pd.merge(data_complete, data_raw, on=['player_id', 'GW'], how='left', suffixes=('', '_discard'))
data_merged = data_merged[[col for col in data_merged.columns if not col.endswith('_discard')]]

data_merged.sort_values(by=['player_id', 'GW'], inplace=True)
essential_info_cols = ['name', 'position', 'team']
for col in essential_info_cols:
     data_merged[col] = data_merged.groupby('player_id')[col].transform(lambda x: x.ffill().bfill())

data_merged['total_points'].fillna(0, inplace=True)
data_merged['value'].fillna(50.0, inplace=True) # Use float

# Filter out rows missing essential info or having invalid teams
print("Filtering invalid rows...")
initial_rows = data_merged.shape[0]
data_filtered = data_merged.dropna(subset=essential_info_cols).copy()
# Ensure team is string for reliable filtering
data_filtered['team'] = data_filtered['team'].astype(str)
data_filtered = data_filtered[data_filtered['team'].isin(C_setofteams_initial)]

# Remove duplicates
rows_before_dedup = data_filtered.shape[0]
data_cleaned = data_filtered.drop_duplicates(subset=['player_id', 'GW'], keep='first').copy()
rows_after_dedup = data_cleaned.shape[0]
print(f"Removed {initial_rows - rows_after_dedup} rows during cleaning/filtering.")

if data_cleaned.empty:
    print("ERROR: No data remaining after cleaning.")
    exit()
print(f"Cleaned data shape: {data_cleaned.shape}")

# Define FINAL Sets
p = sorted(data_cleaned['player_id'].unique())
t = T_setofgameweeks
l = L_substitution
C_setofteams_final = sorted(data_cleaned['team'].unique())
n_P = len(p)
n_T = len(t)
n_C = len(C_setofteams_final)
n_L = len(l)
print(f"FINAL sets: {n_T} GWs, {n_P} Players, {n_C} Teams.")

# Define Subsets
print("Defining player subsets...")
final_player_info = data_cleaned.drop_duplicates(subset=['player_id'], keep='first')
pos_map = pd.Series(final_player_info['position'].values, index=final_player_info['player_id'])

Pgk = [p_ for p_ in p if pos_map.get(p_) == "GK"]
Pdef = [p_ for p_ in p if pos_map.get(p_) == "DEF"]
Pmid = [p_ for p_ in p if pos_map.get(p_) == "MID"]
Pfwd = [p_ for p_ in p if pos_map.get(p_) == "FWD"]
P_not_gk = [p_ for p_ in p if p_ not in Pgk]
print(f"  Positions: {len(Pgk)} GK, {len(Pdef)} DEF, {len(Pmid)} MID, {len(Pfwd)} FWD")

P_c = data_cleaned[data_cleaned['player_id'].isin(p)].groupby('team')['player_id'].unique().apply(list).to_dict()
P_c = {team: players for team, players in P_c.items() if team in C_setofteams_final and players}
C_setofteams = sorted(list(P_c.keys()))
n_C = len(C_setofteams) # Update n_C based on actual teams with players
print(f"  Defined player lists for {n_C} teams.")

# Define GW subsets
if not t:
    print("ERROR: Gameweek set 't' is empty.")
    exit()
medianavgameweeks = np.median(t) if n_T > 0 else t[0]
T_FH = [t_ for t_ in t if t_ <= medianavgameweeks]
T_SH = [t_ for t_ in t if t_ > medianavgameweeks]
print(f"  Gameweek halves: FH <= {medianavgameweeks}, SH > {medianavgameweeks}")

# Define Parameters
print("Defining model parameters...")
R = 4; MK = 2; MD = 5; MM = 5; MF = 3; MC = 3; E = 11; EK = 1
ED = 3; EM = 3; EF = 1; BS = 1000.0 # Scaled
phi = (MK + MD + MM + MF) - E; phi_K = MK - EK
Q_bar = 2; Q_under_bar = 1
epsilon = 0.1; kappa = {1: 0.01, 2: 0.005, 3: 0.001}
beta = n_P + 1; alpha_bar = n_P + 1
print("Parameters defined.")

# Prepare Coefficient Data Structures
print("Preparing coefficient matrices...")
try:
    points_matrix_df = data_cleaned.pivot(index='player_id', columns='GW', values='total_points')
    value_matrix_df = data_cleaned.pivot(index='player_id', columns='GW', values='value')
    points_matrix_df = points_matrix_df.reindex(index=p, columns=t, fill_value=0.0)
    value_matrix_df = value_matrix_df.reindex(index=p, columns=t, fill_value=50.0)
    print("Coefficient matrices ready.")
except Exception as e:
    print(f"ERROR creating pivot tables: {e}")
    exit()

# --- 2. Define PuLP Model ---
print("\n--- 2. Defining PuLP Model ---")
model = pulp.LpProblem("FPL_Optimization", pulp.LpMaximize)

# --- 3. Define Variables ---
print("Defining PuLP variables...")
start_vars = time.time()
x = pulp.LpVariable.dicts("Squad", (p, t), cat='Binary')
x_freehit = pulp.LpVariable.dicts("Squad_FH", (p, t), cat='Binary')
y = pulp.LpVariable.dicts("Lineup", (p, t), cat='Binary')
f = pulp.LpVariable.dicts("Captain", (p, t), cat='Binary')
h = pulp.LpVariable.dicts("ViceCaptain", (p, t), cat='Binary')
is_tc = pulp.LpVariable.dicts("TripleCaptainChipActive", (p, t), cat='Binary')
g = pulp.LpVariable.dicts("Substitution", (P_not_gk, t, l), cat='Binary') if P_not_gk and l else {}
u = pulp.LpVariable.dicts("TransferOut", (p, t), cat='Binary')
e = pulp.LpVariable.dicts("TransferIn", (p, t), cat='Binary')
w = pulp.LpVariable.dicts("WildcardChipActive", t, cat='Binary')
b = pulp.LpVariable.dicts("BenchBoostChipActive", t, cat='Binary')
r = pulp.LpVariable.dicts("FreeHitChipActive", t, cat='Binary')
lambda_var = pulp.LpVariable.dicts("Aux_LineupInSquad", (p, t), cat='Binary')
v = pulp.LpVariable.dicts("RemainingBudget", t, lowBound=0, cat='Continuous')
q = pulp.LpVariable.dicts("FreeTransfersAvailable", t, lowBound=0, upBound=Q_bar, cat='Integer')
alpha = pulp.LpVariable.dicts("PenalizedTransfers", t, lowBound=0, cat='Integer')
# Auxiliary for transfer linearization
ft_carried_over_nonneg = pulp.LpVariable.dicts("FT_Carry", t, lowBound=0)
print(f"Variables defined in {time.time() - start_vars:.2f} seconds.")

# --- 4. Define Objective Function ---
print("Defining objective function...")
start_obj = time.time()
points_from_lineup = pulp.lpSum(points_matrix_df.loc[p_, t_] * y[p_][t_] for p_ in p for t_ in t)
points_from_captain = pulp.lpSum(points_matrix_df.loc[p_, t_] * f[p_][t_] for p_ in p for t_ in t)
points_from_vice = pulp.lpSum(epsilon * points_matrix_df.loc[p_, t_] * h[p_][t_] for p_ in p for t_ in t)
points_from_tc = pulp.lpSum(2 * points_matrix_df.loc[p_, t_] * is_tc[p_][t_] for p_ in p for t_ in t)
points_from_subs = pulp.lpSum(kappa[l_] * points_matrix_df.loc[p_ngk][t_] * g[p_ngk][t_][l_]
                               for p_ngk in P_not_gk for t_ in t for l_ in l) if g else 0
transfer_penalty = pulp.lpSum(R * alpha[t_] for t_ in t if t_ > min(t)) if n_T > 1 else 0
objective = (points_from_lineup + points_from_captain + points_from_vice +
             points_from_tc + points_from_subs - transfer_penalty)
model += objective, "Total_Expected_Points"
print(f"Objective function defined in {time.time() - start_obj:.2f} seconds.")

# --- 5. Define Constraints ---
print("Adding constraints...")
start_cons = time.time()

# -- Gamechips (4.2 - 4.7) --
print("  Adding Gamechip constraints...")
if T_FH: model += pulp.lpSum(w[t_] for t_ in T_FH) <= 1, "WC_Limit_FH"
if T_SH: model += pulp.lpSum(w[t_] for t_ in T_SH) <= 1, "WC_Limit_SH"
model += pulp.lpSum(is_tc[p_][t_] for p_ in p for t_ in t) <= 1, "TC_Limit_Total"
model += pulp.lpSum(b[t_] for t_ in t) <= 1, "BB_Limit_Total"
model += pulp.lpSum(r[t_] for t_ in t) <= 1, "FH_Limit_Total"
for t_ in t:
    model += w[t_] + pulp.lpSum(is_tc[p_][t_] for p_ in p) + b[t_] + r[t_] <= 1, f"GC_Per_Week_{t_}"

# -- Selected Squad (4.8 - 4.12) --
print("  Adding Selected Squad constraints...")
for t_ in t:
    if Pgk: model += pulp.lpSum(x[p_gk][t_] for p_gk in Pgk) == MK, f"Squad_GK_{t_}"
    if Pdef: model += pulp.lpSum(x[p_def][t_] for p_def in Pdef) == MD, f"Squad_DEF_{t_}"
    if Pmid: model += pulp.lpSum(x[p_mid][t_] for p_mid in Pmid) == MM, f"Squad_MID_{t_}"
    if Pfwd: model += pulp.lpSum(x[p_fwd][t_] for p_fwd in Pfwd) == MF, f"Squad_FWD_{t_}"
    for c_team in C_setofteams:
        players_in_team = P_c.get(c_team, [])
        if players_in_team:
             model += pulp.lpSum(x[p_tm][t_] for p_tm in players_in_team) <= MC, f"Squad_TeamLimit_{c_team}_{t_}"

# -- Free Hit Squad (4.13 - 4.17) --
print("  Adding Free Hit Squad constraints...")
for t_ in t:
    if Pgk: model += pulp.lpSum(x_freehit[p_gk][t_] for p_gk in Pgk) == MK * r[t_], f"FH_Squad_GK_{t_}"
    if Pdef: model += pulp.lpSum(x_freehit[p_def][t_] for p_def in Pdef) == MD * r[t_], f"FH_Squad_DEF_{t_}"
    if Pmid: model += pulp.lpSum(x_freehit[p_mid][t_] for p_mid in Pmid) == MM * r[t_], f"FH_Squad_MID_{t_}"
    if Pfwd: model += pulp.lpSum(x_freehit[p_fwd][t_] for p_fwd in Pfwd) == MF * r[t_], f"FH_Squad_FWD_{t_}"
    model += pulp.lpSum(x_freehit[p_][t_] for p_ in p) == (MK + MD + MM + MF) * r[t_], f"FH_Squad_TotalSize_{t_}"
    for c_team in C_setofteams:
        players_in_team = P_c.get(c_team, [])
        if players_in_team:
             model += pulp.lpSum(x_freehit[p_tm][t_] for p_tm in players_in_team) <= MC * r[t_], f"FH_Squad_TeamLimit_{c_team}_{t_}"

# -- Starting Line-up (4.18 - 4.26) --
print("  Adding Starting Line-up constraints...")
for t_ in t:
    model += pulp.lpSum(y[p_][t_] for p_ in p) == E + phi * b[t_], f"Start_Size_{t_}"
    if Pgk: model += pulp.lpSum(y[p_gk][t_] for p_gk in Pgk) == EK + phi_K * b[t_], f"Start_GK_{t_}"
    if Pdef: model += pulp.lpSum(y[p_def][t_] for p_def in Pdef) >= ED, f"Start_MinDEF_{t_}"
    if Pmid: model += pulp.lpSum(y[p_mid][t_] for p_mid in Pmid) >= EM, f"Start_MinMID_{t_}"
    if Pfwd: model += pulp.lpSum(y[p_fwd][t_] for p_fwd in Pfwd) >= EF, f"Start_MinFWD_{t_}"
    for p_ in p: # Linearization 4.24-4.26
        model += y[p_][t_] <= x_freehit[p_][t_] + lambda_var[p_][t_], f"Start_InSquad_LinkA_{p_}_{t_}"
        model += lambda_var[p_][t_] <= x[p_][t_], f"Start_InSquad_LinkB_{p_}_{t_}"
        model += lambda_var[p_][t_] <= 1 - r[t_], f"Start_InSquad_LinkC_{p_}_{t_}"

# -- Captain/Vice (4.27 - 4.29) --
print("  Adding Captain/Vice constraints...")
for t_ in t:
    model += pulp.lpSum(f[p_][t_] for p_ in p) + pulp.lpSum(is_tc[p_][t_] for p_ in p) == 1, f"Captain_Or_TC_Unique_{t_}"
    model += pulp.lpSum(h[p_][t_] for p_ in p) == 1, f"ViceCaptain_Unique_{t_}"
    for p_ in p:
        model += f[p_][t_] + is_tc[p_][t_] + h[p_][t_] <= y[p_][t_], f"Captaincy_In_Lineup_{p_}_{t_}"

# -- Substitution (4.30 - 4.32) --
print("  Adding Substitution constraints...")
if P_not_gk and l and g:
    for t_ in t:
        for p_ngk in P_not_gk:
             # y[p] + sum(g[p,l]) <= active_squad[p]
            model += y[p_ngk][t_] + pulp.lpSum(g[p_ngk][t_][l_] for l_ in l) <= \
                     x_freehit[p_ngk][t_] + lambda_var[p_ngk][t_], f"Sub_If_Benched_{p_ngk}_{t_}"
        for l_ in l:
            model += pulp.lpSum(g[p_ngk][t_][l_] for p_ngk in P_not_gk) <= 1, f"Sub_Priority_Unique_{t_}_{l_}"

# -- Budget (4.33 - 4.39) --
print("  Adding Budget constraints...")
t1 = min(t)
model += v[t1] + pulp.lpSum(value_matrix_df.loc[p_, t1] * x[p_][t1] for p_ in p) == BS, "Budget_Initial"
for t_ in t:
    for p_ in p:
        model += e[p_][t_] + u[p_][t_] <= 1, f"Transfer_In_Out_Limit_{p_}_{t_}"

if n_T > 1:
    for gw_idx in range(1, n_T):
        t_curr = t[gw_idx]; t_prev = t[gw_idx-1]
        # 4.34: Budget Evolution
        sales_value = pulp.lpSum(value_matrix_df.loc[p_, t_curr] * u[p_][t_curr] for p_ in p)
        purchase_cost = pulp.lpSum(value_matrix_df.loc[p_, t_curr] * e[p_][t_curr] for p_ in p)
        model += v[t_curr] == v[t_prev] + sales_value - purchase_cost, f"Budget_Evolution_{t_curr}"
        # 4.35: Squad Continuity
        for p_ in p:
            model += x[p_][t_curr] == x[p_][t_prev] - u[p_][t_curr] + e[p_][t_curr], f"Squad_Continuity_{p_}_{t_curr}"
        # 4.37: Free Hit Budget Limit (Big-M formulation)
        value_prev_squad_t_prev_val = pulp.lpSum(value_matrix_df.loc[p_,t_prev] * x[p_][t_prev] for p_ in p)
        cost_fh_squad_t_curr_val = pulp.lpSum(value_matrix_df.loc[p_,t_curr] * x_freehit[p_][t_curr] for p_ in p)
        M_fh_budget = BS + (MK + MD + MM + MF) * 150 # Max possible squad value + budget
        model += cost_fh_squad_t_curr_val - M_fh_budget * r[t_curr] <= v[t_prev] + value_prev_squad_t_prev_val, f"FH_Budget_Limit_{t_curr}"
        # 4.38 / 4.39: Free Hit Transfer Restriction
        M_fh_transfer = MK + MD + MM + MF # Max number of players in squad
        model += pulp.lpSum(u[p_][t_curr] for p_ in p) <= M_fh_transfer * (1 - r[t_curr]), f"FH_NoTransfersOut_{t_curr}"
        model += pulp.lpSum(e[p_][t_curr] for p_ in p) <= M_fh_transfer * (1 - r[t_curr]), f"FH_NoTransfersIn_{t_curr}"

# -- Transfers (4.40 - 4.44 + alpha calculation) --
print("  Adding Transfer constraints...")
if n_T > 1:
    t2 = t[1]
    model += q[t2] == Q_under_bar, "Transfer_Initial_Q_GW2"
    for gw_idx in range(1, n_T):
        t_curr = t[gw_idx]; t_prev = t[gw_idx-1]
        # Penalized transfers calculation
        model += alpha[t_curr] >= pulp.lpSum(e[p_][t_curr] for p_ in p) - q[t_curr], f"PenalizedTransfers_Calculation_{t_curr}"
        model += alpha[t_curr] <= alpha_bar * (1 - w[t_curr]), f"PenalizedTransfers_WC_Override_{t_curr}"

        if gw_idx < n_T - 1:
            t_next = t[gw_idx+1]
            # Free transfer carry-over logic (linearized)
            ft_used = pulp.lpSum(e[p_][t_curr] for p_ in p) - alpha[t_curr]
            model += ft_carried_over_nonneg[t_curr] >= q[t_curr] - ft_used, f"FT_Carry_Min_{t_curr}" # Ensure it captures carry-over correctly
            # q[t+1] = min(Q_bar, ft_carried_over + Q_)
            model += q[t_next] <= ft_carried_over_nonneg[t_curr] + Q_under_bar, f"Transfer_Evo_Max_{t_next}"
            # Reset q[t+1] logic for WC/FH (Force to Q_under_bar)
            model += q[t_next] <= Q_under_bar + Q_bar * (1 - w[t_curr]), f"Transfer_Reset_WC_Upper_{t_next}"
            model += q[t_next] >= Q_under_bar * w[t_curr], f"Transfer_Reset_WC_Lower_{t_next}"
            model += q[t_next] <= Q_under_bar + Q_bar * (1 - r[t_curr]), f"Transfer_Reset_FH_Upper_{t_next}"
            model += q[t_next] >= Q_under_bar * r[t_curr], f"Transfer_Reset_FH_Lower_{t_next}"
            # Constraint 4.42: No penalized transfers if max FT are carried over
            model += alpha[t_curr] + alpha_bar * q[t_next] <= alpha_bar * Q_bar, f"Transfer_NoPaidIfMaxFT_{t_curr}"

print(f"Constraints added in {time.time() - start_cons:.2f} seconds.")

# --- 6. Solving Model ---
print("\n--- 6. Solving Model ---")
print(f"Number of variables: {model.numVariables()}")
print(f"Number of constraints: {model.numConstraints()}")
start_solve = time.time()

# --- Solver Configuration ---
try:
    num_cores = os.cpu_count()
    print(f"Detected {num_cores} logical CPU cores.")
    # Using n-1 cores often provides a good balance
    threads_to_use = max(1, num_cores - 1 if num_cores else 1)
    print(f"Setting CBC solver to use {threads_to_use} threads.")
except NotImplementedError:
    print("Could not detect number of CPU cores automatically. Setting threads to 4.")
    threads_to_use = 4

solver = pulp.PULP_CBC_CMD(threads=threads_to_use, msg=True, timeLimit=SOLVER_TIME_LIMIT)

# --- Threading for Progress Updates ---
solve_status = None
solver_exception = None

def solve_in_thread():
    global solve_status, solver_exception  # Use global instead of nonlocal
    try:
        print("Solver thread started...")
        solve_status = model.solve(solver)
        print("Solver thread finished.")
    except Exception as e:
        solver_exception = e
        print(f"Solver thread encountered an error: {e}")

solver_thread = threading.Thread(target=solve_in_thread)
solver_thread.start()

print(f"Main thread waiting for solver (updates every {PROGRESS_UPDATE_INTERVAL}s)...")
while solver_thread.is_alive():
    elapsed_time = time.time() - start_solve
    print(f"[{time.strftime('%H:%M:%S')}] Solver running... (Elapsed: {elapsed_time:.0f}s)")
    # Wait smartly
    wait_start = time.time()
    while time.time() - wait_start < PROGRESS_UPDATE_INTERVAL:
        if not solver_thread.is_alive():
            break
        time.sleep(1)

solver_thread.join()
solve_time = time.time() - start_solve

# --- Post-Solve Processing ---
if solver_exception:
    print("\n--- Solver Error ---")
    print(f"An error occurred in the solver thread: {solver_exception}")
    status = -1000
else:
    status = solve_status

if status is None:
     print("\nERROR: Solver status was not captured.")
     status = -1000

print(f"\n--- Solve Complete ---")
print(f"Final Solver Status Code: {status}")
if status > -1000:
    print(f"Final Solver Status: {pulp.LpStatus.get(status, 'Unknown Status')}") # Use get for safety
print(f"Total Solve Time: {solve_time:.2f} seconds")


# --- 7. Extract and Display Results ---
print("\n--- 7. Results ---")
if status == pulp.LpStatusOptimal or status == pulp.LpStatusNotSolved:
    try:
        obj_value = pulp.value(model.objective)
        print(f"Objective value: {obj_value:.4f}")
        if abs(obj_value) < 1e-6 and status == pulp.LpStatusOptimal:
            print("WARNING: Objective is zero or near-zero.")
    except AttributeError:
        print("Could not retrieve objective value (likely not solved to optimality).")
        obj_value = None

    # --- Create Player Name Lookup ---
    player_name_map = final_player_info.set_index('player_id')['name'].to_dict()

    # --- Function to get names and details for selected players ---
    def get_selected_players_with_value(var_dict, gw, player_map, value_df):
        selected_ids = []
        # Check if the variable dictionary structure is (player, gameweek)
        if isinstance(var_dict, dict) and p and t and isinstance(var_dict.get(p[0]), dict):
             selected_ids = [p_ for p_ in p if p_ in var_dict and gw in var_dict[p_] and var_dict[p_][gw].varValue > 0.99]
        # Add checks for other structures if needed, e.g., only indexed by gameweek 'w', 'b', 'r'
        # This part might need refinement based on how single-index variables like 'w', 'b', 'r' are accessed if needed here

        names = [player_map.get(p_id, f"Unknown ID: {p_id}") for p_id in selected_ids]
        # Get value for the specific gameweek, handle potential KeyError
        values = []
        for p_id in selected_ids:
            try:
                val = value_df.loc[p_id, gw]
                values.append(val)
            except KeyError:
                values.append('N/A') # Player or GW not in value matrix index/columns

        return pd.DataFrame({'player_id': selected_ids, 'name': names, f'value_gw{gw}': values}).sort_values(by='player_id').reset_index(drop=True) if selected_ids else pd.DataFrame(columns=['player_id', 'name', f'value_gw{gw}'])


    # --- Display Key Decisions ---
    prev_budget = {} # Reset for this results display run
    budget_start_gw1 = BS - pulp.lpSum(value_matrix_df.loc[p_, min(t)] * x[p_][min(t)].varValue for p_ in p if x[p_][min(t)].varValue is not None).value()
    prev_budget[min(t)] = budget_start_gw1 # Budget remaining *after* initial selection

    for t_ in t:
        print(f"\n" + "="*20 + f" Gameweek {t_} Decisions " + "="*20)
        try: # Wrap gameweek processing in try-except for robustness
            # Determine active chip
            is_fh_active = r[t_].varValue > 0.99 if r[t_].varValue is not None else False
            is_bb_active = b[t_].varValue > 0.99 if b[t_].varValue is not None else False
            is_wc_active = w[t_].varValue > 0.99 if w[t_].varValue is not None else False
            tc_active_player_df = get_selected_players_with_value(is_tc, t_, player_name_map, value_matrix_df)
            is_tc_active = not tc_active_player_df.empty

            # Get lineup
            lineup_df = get_selected_players_with_value(y, t_, player_name_map, value_matrix_df)

            # Print Squad
            if is_fh_active:
                print(f"!!! FREE HIT ACTIVE (GW{t_}) !!!")
                active_squad_df = get_selected_players_with_value(x_freehit, t_, player_name_map, value_matrix_df)
                print(f"\nFree Hit Squad (Temporary for GW{t_}): {len(active_squad_df)} players")
                if not active_squad_df.empty: print(active_squad_df.to_string(index=False))
                persistent_squad_df = get_selected_players_with_value(x, t_, player_name_map, value_matrix_df)
                print(f"\nPersistent Squad (GW{t_} - Underlying): {len(persistent_squad_df)} players")
                if not persistent_squad_df.empty: print(persistent_squad_df.to_string(index=False))
            else:
                active_squad_df = get_selected_players_with_value(x, t_, player_name_map, value_matrix_df)
                print(f"\nSquad (GW{t_}): {len(active_squad_df)} players")
                if not active_squad_df.empty: print(active_squad_df.to_string(index=False))

            # Print Lineup
            print(f"\nStarting Lineup (GW{t_}): {len(lineup_df)} players")
            if is_bb_active: print("  (Bench Boost Active)")
            elif is_fh_active: print("  (Selected from Free Hit Squad)")
            if not lineup_df.empty: print(lineup_df.to_string(index=False))

            # Print Captaincy
            captain_df = get_selected_players_with_value(f, t_, player_name_map, value_matrix_df)
            if not captain_df.empty: print(f"Captain: {captain_df['name'].iloc[0]} (ID: {captain_df['player_id'].iloc[0]})")
            vc_df = get_selected_players_with_value(h, t_, player_name_map, value_matrix_df)
            if not vc_df.empty: print(f"Vice-Captain: {vc_df['name'].iloc[0]} (ID: {vc_df['player_id'].iloc[0]})")

            # Print Chip Status Summary
            if is_wc_active: print("WILDCARD ACTIVE")
            if is_bb_active: print("BENCH BOOST ACTIVE")
            if is_tc_active: print(f"TRIPLE CAPTAIN ACTIVE on: {tc_active_player_df['name'].iloc[0]} (ID: {tc_active_player_df['player_id'].iloc[0]})")

            # --- Transfers and Budget Verification ---
            if t_ > min(t):
                print("\n--- Transfers & Budget ---")
                transfers_in_df = get_selected_players_with_value(e, t_, player_name_map, value_matrix_df)
                transfers_out_df = get_selected_players_with_value(u, t_, player_name_map, value_matrix_df)

                # Ensure values are numeric before summing
                cost_in = pd.to_numeric(transfers_in_df[f'value_gw{t_}'], errors='coerce').sum() if not transfers_in_df.empty else 0
                value_out = pd.to_numeric(transfers_out_df[f'value_gw{t_}'], errors='coerce').sum() if not transfers_out_df.empty else 0
                net_transfer_value = value_out - cost_in

                t_prev = t[t.index(t_) - 1]
                budget_start = prev_budget[t_prev]
                budget_end_calc = budget_start + net_transfer_value
                budget_end_model = pulp.value(v[t_]) if v[t_].varValue is not None else None

                print(f"Budget (Start of GW{t_}): {budget_start:.1f}")
                if not transfers_in_df.empty:
                    print(f"Transfers IN ({len(transfers_in_df)}): Cost = {cost_in:.1f}")
                    #print(transfers_in_df.to_string(index=False)) # Optional: Print full list
                else: print(f"Transfers IN: 0")
                if not transfers_out_df.empty:
                    print(f"Transfers OUT ({len(transfers_out_df)}): Value = {value_out:.1f}")
                    #print(transfers_out_df.to_string(index=False)) # Optional: Print full list
                else: print(f"Transfers OUT: 0")

                print(f"Net Transfer Value: {net_transfer_value:.1f}")
                print(f"Calculated Budget (End of GW{t_}): {budget_start:.1f} + {net_transfer_value:.1f} = {budget_end_calc:.1f}")
                if budget_end_model is not None:
                    print(f"Model Budget (End of GW{t_}): {budget_end_model:.1f}")
                    if abs(budget_end_calc - budget_end_model) > 0.01:
                        print("!!! BUDGET DISCREPANCY DETECTED !!!")
                else:
                    print("Model Budget (End of GW{t_}): Error retrieving value.")

                q_val = q[t_].varValue if q[t_].varValue is not None else 'N/A'
                alpha_val = alpha[t_].varValue if alpha[t_].varValue is not None else 'N/A'
                print(f"Free Transfers Available (Start of GW{t_}): {q_val if q_val == 'N/A' else round(q_val)}")
                print(f"Penalized Transfers Made: {alpha_val if alpha_val == 'N/A' else round(alpha_val)}")

            # Store budget for next iteration
            current_budget_end = pulp.value(v[t_]) if v[t_].varValue is not None else prev_budget.get(t_prev, 0) # Use previous if current fails
            prev_budget[t_] = current_budget_end
            print(f"\nRemaining Budget (End of GW{t_}): {current_budget_end:.1f}")

        except Exception as e_report:
             print(f"\n!!! Error processing results for GW {t_}: {e_report} !!!")
             # Attempt to print basic variable values if possible
             try:
                 print(f"  v[{t_}]: {v[t_].varValue}")
                 print(f"  r[{t_}]: {r[t_].varValue}")
                 # Add others if needed
             except:
                 print("  Could not retrieve basic variable values for this GW.")


elif status == pulp.LpStatusInfeasible:
    print("ERROR: Model is infeasible. Check constraints and data.")
elif status == pulp.LpStatusUnbounded:
    print("ERROR: Model is unbounded. Check objective function and constraints.")
else:
    print(f"Model did not solve to optimality or an error occurred. Status Code: {status}")

print("\n--- Script Finished ---")