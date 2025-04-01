import pandas as pd
import pulp
import numpy as np
import time
import sys
import os
import threading
import math # Added for potential rounding if needed

# Dette skriptet har med rullerende horisont

# --- Input Parameters ---
START_GAMEWEEK = 1
MAX_GAMEWEEK = 2  # Set to the desired maximum gameweek for the whole run
SUB_HORIZON_LENGTH = 1 # Number of GWs to look ahead (including current) in each step
# *** IMPORTANT: Update this path to your actual file location ***
CSV_FILE_PATH = "C:/Users/peram/Documents/test/Differensiert gw alle tre sesonger(22-24), heltall.csv"
onemin = 60
SOLVER_TIME_LIMIT = onemin * 4  # Seconds (e.g., 900 for 15 mins per subproblem) or None
PROGRESS_UPDATE_INTERVAL = 30 # Seconds (shorter interval might be useful)

# --- Check Versions ---
print(f"--- Running on Python {sys.version} ---")
print(f"--- PuLP Version: {pulp.__version__} ---")

# --- 0. Setup & Data Loading (Load ONCE) ---
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

# --- Data Pre-processing and Filtering (for the entire MAX_GAMEWEEK range) ---
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

# Filter Gameweeks (Initial broad filter)
data_full_range = allesesonger[(allesesonger['GW'] >= START_GAMEWEEK) & (allesesonger['GW'] <= MAX_GAMEWEEK)].copy()
if data_full_range.empty:
    print(f"ERROR: No data found for GW range {START_GAMEWEEK}-{MAX_GAMEWEEK}.")
    exit()
print(f"Filtered data for GW {START_GAMEWEEK}-{MAX_GAMEWEEK}. Shape: {data_full_range.shape}")

# Clean categorical columns
for col in ['team', 'position', 'name']:
    if col in data_full_range.columns:
        data_full_range[col] = data_full_range[col].astype(str).str.strip()

# --- 1. Define Initial FULL Sets and Perform Data Cleaning/Validation ---
print("\n--- 1. Data Cleaning & FULL Set Definition ---")

# Define initial FULL sets
T_setofgameweeks_full = sorted(data_full_range['GW'].unique())
P_setofplayers_initial = sorted(data_full_range['player_id'].unique())
C_setofteams_initial = sorted(data_full_range['team'].dropna().unique())
L_substitution = list(range(1, 4)) # Priorities 1, 2, 3

n_T_full = len(T_setofgameweeks_full)
n_P_initial = len(P_setofplayers_initial)

if n_T_full == 0 or n_P_initial == 0:
    print("ERROR: Initial Gameweek or Player set is empty after filtering.")
    exit()
print(f"Initial FULL sets: {n_T_full} Gameweeks, {n_P_initial} Players, {len(C_setofteams_initial)} Teams.")

# Create all player-GW combinations for the full range
player_gw_combos_full = pd.MultiIndex.from_product([P_setofplayers_initial, T_setofgameweeks_full],
                                                     names=['player_id', 'GW'])
data_complete_full = pd.DataFrame(index=player_gw_combos_full).reset_index()

# Merge and fill missing data for the full range
print("Merging and filling missing data for full range...")
data_merged_full = pd.merge(data_complete_full, data_full_range, on=['player_id', 'GW'], how='left', suffixes=('', '_discard'))
data_merged_full = data_merged_full[[col for col in data_merged_full.columns if not col.endswith('_discard')]]

data_merged_full.sort_values(by=['player_id', 'GW'], inplace=True)
essential_info_cols = ['name', 'position', 'team']
for col in essential_info_cols:
     data_merged_full[col] = data_merged_full.groupby('player_id')[col].transform(lambda x: x.ffill().bfill())

data_merged_full['total_points'].fillna(0, inplace=True)
data_merged_full['value'].fillna(50.0, inplace=True) # Use float

# Filter out rows missing essential info or having invalid teams
print("Filtering invalid rows for full range...")
initial_rows_full = data_merged_full.shape[0]
data_filtered_full = data_merged_full.dropna(subset=essential_info_cols).copy()
data_filtered_full['team'] = data_filtered_full['team'].astype(str)
# Ensure C_setofteams_initial contains strings before filtering
C_setofteams_initial_str = [str(team) for team in C_setofteams_initial]
data_filtered_full = data_filtered_full[data_filtered_full['team'].isin(C_setofteams_initial_str)]


# Remove duplicates
rows_before_dedup_full = data_filtered_full.shape[0]
data_cleaned_full = data_filtered_full.drop_duplicates(subset=['player_id', 'GW'], keep='first').copy()
rows_after_dedup_full = data_cleaned_full.shape[0]
print(f"Removed {initial_rows_full - rows_after_dedup_full} rows during cleaning/filtering.")

if data_cleaned_full.empty:
    print("ERROR: No data remaining after cleaning.")
    exit()
print(f"Cleaned full data shape: {data_cleaned_full.shape}")

# Define FINAL FULL Sets (p remains constant)
p = sorted(data_cleaned_full['player_id'].unique())
l = L_substitution
C_setofteams_final = sorted(data_cleaned_full['team'].unique()) # Should be strings now
n_P = len(p)
n_C_final = len(C_setofteams_final)
n_L = len(l)
print(f"FINAL FULL sets: {n_P} Players, {n_C_final} Teams.")

# Define Subsets (based on full player list p)
print("Defining player subsets...")
final_player_info = data_cleaned_full.drop_duplicates(subset=['player_id'], keep='first')
player_name_map = final_player_info.set_index('player_id')['name'].to_dict() # Player name lookup
pos_map = pd.Series(final_player_info['position'].values, index=final_player_info['player_id'])

Pgk = [p_ for p_ in p if pos_map.get(p_) == "GK"]
Pdef = [p_ for p_ in p if pos_map.get(p_) == "DEF"]
Pmid = [p_ for p_ in p if pos_map.get(p_) == "MID"]
Pfwd = [p_ for p_ in p if pos_map.get(p_) == "FWD"]
P_not_gk = [p_ for p_ in p if p_ not in Pgk]
print(f"  Positions: {len(Pgk)} GK, {len(Pdef)} DEF, {len(Pmid)} MID, {len(Pfwd)} FWD")

# Ensure P_c uses the final string team names
P_c = data_cleaned_full[data_cleaned_full['player_id'].isin(p)].groupby('team')['player_id'].unique().apply(list).to_dict()
P_c = {team: players for team, players in P_c.items() if team in C_setofteams_final and players}
C_setofteams = sorted(list(P_c.keys())) # This will be the final list of teams *with players*
n_C = len(C_setofteams) # Update n_C based on actual teams with players
print(f"  Defined player lists for {n_C} teams.")

# Define GW subsets (for chip rules, based on full range)
medianavgameweeks = np.median(T_setofgameweeks_full) if n_T_full > 0 else (T_setofgameweeks_full[0] if T_setofgameweeks_full else 1)
T_FH_overall = [t_ for t_ in T_setofgameweeks_full if t_ <= medianavgameweeks]
T_SH_overall = [t_ for t_ in T_setofgameweeks_full if t_ > medianavgameweeks]
print(f"  Overall Gameweek halves: FH <= {medianavgameweeks}, SH > {medianavgameweeks}")

# Define Parameters (Constants)
print("Defining model parameters...")
R = 4; MK = 2; MD = 5; MM = 5; MF = 3; MC = 3; E = 11; EK = 1
ED = 3; EM = 2; EF = 1; BS = 1000.0 # Scaled
phi = (MK + MD + MM + MF) - E; phi_K = MK - EK
Q_bar = 2; Q_under_bar = 1
epsilon = 0.1; kappa = {1: 0.01, 2: 0.005, 3: 0.001}
beta = n_P + 1; alpha_bar = n_P + 1 # Sufficiently large constants
print("Parameters defined.")

# Prepare Coefficient Data Structures (for full range)
print("Preparing full coefficient matrices...")
try:
    points_matrix_df = data_cleaned_full.pivot(index='player_id', columns='GW', values='total_points')
    value_matrix_df = data_cleaned_full.pivot(index='player_id', columns='GW', values='value')
    # Reindex to ensure all players/GWs are present
    points_matrix_df = points_matrix_df.reindex(index=p, columns=T_setofgameweeks_full, fill_value=0.0)
    value_matrix_df = value_matrix_df.reindex(index=p, columns=T_setofgameweeks_full, fill_value=50.0) # Use a default value
    print("Full coefficient matrices ready.")
except Exception as e:
    print(f"ERROR creating full pivot tables: {e}")
    exit()

# --- Initialize State Variables for Rolling Horizon ---
master_results = [] # List to store results dict for each implemented GW
previous_squad_dict = {} # Dict: {player_id: 1} for players in squad at end of previous GW
previous_budget = BS # Budget available at the START of the current_gw
previous_ft = 1 # Free transfers available at the START of the current_gw (Rule for GW1)
used_chips_tracker = {'wc1': False, 'wc2': False, 'bb': False, 'tc': False, 'fh': False}

# --- Solver Configuration ---
try:
    num_cores = os.cpu_count()
    print(f"Detected {num_cores} logical CPU cores.")
    # Use slightly fewer cores than total to leave resources for OS/other tasks
    threads_to_use = max(1, num_cores - 1 if num_cores and num_cores > 1 else 1)
    print(f"Setting CBC solver to use {threads_to_use} threads.")
except NotImplementedError:
    print("Could not detect number of CPU cores. Setting threads to 4.")
    threads_to_use = 4
# Ensure timeLimit is integer or None
solver_time_limit_int = int(SOLVER_TIME_LIMIT) if SOLVER_TIME_LIMIT is not None else None
solver = pulp.PULP_CBC_CMD(threads=threads_to_use, msg=True, timeLimit=solver_time_limit_int)
#solver = pulp.GLPK_CMD(msg=True, timeLimit=solver_time_limit_int)

# --- Rolling Horizon Loop ---
for current_gw in range(START_GAMEWEEK, MAX_GAMEWEEK + 1):
    print(f"\n{'='*15} Solving for Gameweek {current_gw} {'='*15}")
    loop_start_time = time.time()

    # 1. Define Sub-Horizon Gameweeks
    t_sub = list(range(current_gw, min(current_gw + SUB_HORIZON_LENGTH, MAX_GAMEWEEK + 1)))
    if not t_sub:
        print("Reached end of horizon.")
        break
    n_T_sub = len(t_sub)
    print(f"Sub-horizon: {t_sub}")

    # 2. Create New Model Instance
    model = pulp.LpProblem(f"FPL_Opt_GW{current_gw}_Sub", pulp.LpMaximize)

    # 3. Define Variables for Sub-Horizon (p, t_sub)
    print("Defining variables for sub-horizon...")
    var_start = time.time()
    x = pulp.LpVariable.dicts("Squad", (p, t_sub), cat='Binary')
    x_freehit = pulp.LpVariable.dicts("Squad_FH", (p, t_sub), cat='Binary')
    y = pulp.LpVariable.dicts("Lineup", (p, t_sub), cat='Binary')
    f = pulp.LpVariable.dicts("Captain", (p, t_sub), cat='Binary')
    h = pulp.LpVariable.dicts("ViceCaptain", (p, t_sub), cat='Binary')
    is_tc = pulp.LpVariable.dicts("TripleCaptainChipActive", (p, t_sub), cat='Binary')
    g = {}
    if P_not_gk and l: # Only define if player list and priority list are non-empty
        g = pulp.LpVariable.dicts("Substitution", (P_not_gk, t_sub, l), cat='Binary')
    u = pulp.LpVariable.dicts("TransferOut", (p, t_sub), cat='Binary')
    e = pulp.LpVariable.dicts("TransferIn", (p, t_sub), cat='Binary')
    w = pulp.LpVariable.dicts("WildcardChipActive", t_sub, cat='Binary')
    b = pulp.LpVariable.dicts("BenchBoostChipActive", t_sub, cat='Binary')
    r = pulp.LpVariable.dicts("FreeHitChipActive", t_sub, cat='Binary')
    lambda_var = pulp.LpVariable.dicts("Aux_LineupInSquad", (p, t_sub), cat='Binary')
    v = pulp.LpVariable.dicts("RemainingBudget", t_sub, lowBound=0, cat='Continuous')
    q = pulp.LpVariable.dicts("FreeTransfersAvailable", t_sub, lowBound=0, upBound=Q_bar, cat='Integer')
    alpha = pulp.LpVariable.dicts("PenalizedTransfers", t_sub, lowBound=0, cat='Integer')
    ft_carried_over_nonneg = pulp.LpVariable.dicts("FT_Carry", t_sub, lowBound=0)
    print(f"Variables defined in {time.time() - var_start:.2f}s")

    # 4. Filter Parameters for Sub-Horizon
    # Ensure t_sub elements exist as columns before indexing
    valid_t_sub = [t for t in t_sub if t in points_matrix_df.columns]
    if not valid_t_sub:
        print(f"ERROR: No valid gameweeks {t_sub} found in data columns for GW {current_gw}.")
        break # Stop if no data for the sub-horizon
    points_sub = points_matrix_df.loc[p, valid_t_sub]
    value_sub = value_matrix_df.loc[p, valid_t_sub]
    # Update t_sub to only include valid gameweeks found in the data
    t_sub = valid_t_sub
    n_T_sub = len(t_sub)
    if n_T_sub == 0: # Double check after filtering
        print(f"ERROR: Sub-horizon became empty after data validation for GW {current_gw}.")
        break

    # 5. Define Objective for Sub-Horizon
    print("Defining objective function...")
    obj_start = time.time()
    points_from_lineup = pulp.lpSum(points_sub.loc[p_, t_] * y[p_][t_] for p_ in p for t_ in t_sub)
    points_from_captain = pulp.lpSum(points_sub.loc[p_, t_] * f[p_][t_] for p_ in p for t_ in t_sub)
    points_from_vice = pulp.lpSum(epsilon * points_sub.loc[p_, t_] * h[p_][t_] for p_ in p for t_ in t_sub)
    points_from_tc = pulp.lpSum(2 * points_sub.loc[p_, t_] * is_tc[p_][t_] for p_ in p for t_ in t_sub)
    points_from_subs = 0
    if g: # Only add if 'g' variables were defined
        points_from_subs = pulp.lpSum(kappa[l_] * points_sub.loc[p_ngk][t_] * g[p_ngk][t_][l_]
                                      for p_ngk in P_not_gk for t_ in t_sub for l_ in l)

    transfer_penalty = 0
    if t_sub: # Ensure t_sub is not empty
        # Penalize transfers for all GWs in sub-horizon *except potentially the first*
        transfer_penalty = pulp.lpSum(R * alpha[t_] for t_ in t_sub if t_ > t_sub[0])
        # Add penalty for the first GW *if it's not the absolute start*
        if current_gw > START_GAMEWEEK:
            transfer_penalty += R * alpha[t_sub[0]]

    objective = (points_from_lineup + points_from_captain + points_from_vice +
                 points_from_tc + points_from_subs - transfer_penalty)
    model += objective, "Total_Expected_Points_Sub"
    print(f"Objective defined in {time.time() - obj_start:.2f}s")

    # 6. Define Constraints for Sub-Horizon
    print("Adding constraints...")
    cons_start = time.time()

    # --- Gamechips ---
    # Check overall tracker and apply limits within the sub-horizon
    valid_T_FH = [t for t in t_sub if t in T_FH_overall]
    valid_T_SH = [t for t in t_sub if t in T_SH_overall]

    if not used_chips_tracker['wc1']:
        if valid_T_FH: model += pulp.lpSum(w[t_] for t_ in valid_T_FH) <= 1, "WC_Limit_FH_Sub"
    else:
         if valid_T_FH: model += pulp.lpSum(w[t_] for t_ in valid_T_FH) == 0, "WC1_Used"

    if not used_chips_tracker['wc2']:
        if valid_T_SH: model += pulp.lpSum(w[t_] for t_ in valid_T_SH) <= 1, "WC_Limit_SH_Sub"
    else:
        if valid_T_SH: model += pulp.lpSum(w[t_] for t_ in valid_T_SH) == 0, "WC2_Used"

    # Flatten the is_tc structure for summation if needed, or ensure it's correct
    tc_sum_terms = [is_tc[p_][t_] for p_ in p for t_ in t_sub]
    if not used_chips_tracker['tc']:
        if tc_sum_terms: model += pulp.lpSum(tc_sum_terms) <= 1, "TC_Limit_Sub"
    else:
        if tc_sum_terms: model += pulp.lpSum(tc_sum_terms) == 0, "TC_Used"

    if not used_chips_tracker['bb']:
        if t_sub: model += pulp.lpSum(b[t_] for t_ in t_sub) <= 1, "BB_Limit_Sub"
    else:
        if t_sub: model += pulp.lpSum(b[t_] for t_ in t_sub) == 0, "BB_Used"

    if not used_chips_tracker['fh']:
        if t_sub: model += pulp.lpSum(r[t_] for t_ in t_sub) <= 1, "FH_Limit_Sub"
    else:
        if t_sub: model += pulp.lpSum(r[t_] for t_ in t_sub) == 0, "FH_Used"

    for t_ in t_sub:
         # Ensure is_tc sum is valid here too
         tc_sum_terms_t = [is_tc[p_][t_] for p_ in p]
         model += w[t_] + pulp.lpSum(tc_sum_terms_t) + b[t_] + r[t_] <= 1, f"GC_Per_Week_{t_}"

    # --- Squad, FH Squad, Lineup, Captain, Subs (Iterate over t_sub) ---
    for t_ in t_sub:
        # Selected Squad (4.8 - 4.12)
        if Pgk: model += pulp.lpSum(x[p_gk][t_] for p_gk in Pgk) == MK, f"Squad_GK_{t_}"
        if Pdef: model += pulp.lpSum(x[p_def][t_] for p_def in Pdef) == MD, f"Squad_DEF_{t_}"
        if Pmid: model += pulp.lpSum(x[p_mid][t_] for p_mid in Pmid) == MM, f"Squad_MID_{t_}"
        if Pfwd: model += pulp.lpSum(x[p_fwd][t_] for p_fwd in Pfwd) == MF, f"Squad_FWD_{t_}"
        for c_team in C_setofteams: # Use the filtered C_setofteams
            players_in_team = P_c.get(c_team, [])
            if players_in_team:
                 model += pulp.lpSum(x[p_tm][t_] for p_tm in players_in_team) <= MC, f"Squad_TeamLimit_{c_team}_{t_}"

        # Free Hit Squad (4.13 - 4.17)
        if Pgk: model += pulp.lpSum(x_freehit[p_gk][t_] for p_gk in Pgk) <= MK, f"FH_Squad_GK_Upper_{t_}" # Use <= M * r[t] form
        if Pgk: model += pulp.lpSum(x_freehit[p_gk][t_] for p_gk in Pgk) >= MK * r[t_] - MK * (1 - r[t_]), f"FH_Squad_GK_Lower_{t_}"
        if Pdef: model += pulp.lpSum(x_freehit[p_def][t_] for p_def in Pdef) <= MD, f"FH_Squad_DEF_Upper_{t_}"
        if Pdef: model += pulp.lpSum(x_freehit[p_def][t_] for p_def in Pdef) >= MD * r[t_] - MD * (1 - r[t_]), f"FH_Squad_DEF_Lower_{t_}"
        if Pmid: model += pulp.lpSum(x_freehit[p_mid][t_] for p_mid in Pmid) <= MM, f"FH_Squad_MID_Upper_{t_}"
        if Pmid: model += pulp.lpSum(x_freehit[p_mid][t_] for p_mid in Pmid) >= MM * r[t_] - MM * (1 - r[t_]), f"FH_Squad_MID_Lower_{t_}"
        if Pfwd: model += pulp.lpSum(x_freehit[p_fwd][t_] for p_fwd in Pfwd) <= MF, f"FH_Squad_FWD_Upper_{t_}"
        if Pfwd: model += pulp.lpSum(x_freehit[p_fwd][t_] for p_fwd in Pfwd) >= MF * r[t_] - MF * (1 - r[t_]), f"FH_Squad_FWD_Lower_{t_}"
        total_squad_size = MK + MD + MM + MF
        model += pulp.lpSum(x_freehit[p_][t_] for p_ in p) <= total_squad_size * r[t_], f"FH_Squad_TotalSize_Upper_{t_}"
        model += pulp.lpSum(x_freehit[p_][t_] for p_ in p) >= total_squad_size * r[t_] - total_squad_size * (1-r[t_]), f"FH_Squad_TotalSize_Lower_{t_}"
        for c_team in C_setofteams:
            players_in_team = P_c.get(c_team, [])
            if players_in_team:
                 model += pulp.lpSum(x_freehit[p_tm][t_] for p_tm in players_in_team) <= MC, f"FH_Squad_TeamLimit_{c_team}_Upper_{t_}"
                 # You might not need a lower bound here if the upper bound and total size enforce enough structure

        # Starting Line-up (4.18 - 4.26)
        model += pulp.lpSum(y[p_][t_] for p_ in p) == E + phi * b[t_], f"Start_Size_{t_}"
        if Pgk: model += pulp.lpSum(y[p_gk][t_] for p_gk in Pgk) == EK + phi_K * b[t_], f"Start_GK_{t_}"
        if Pdef: model += pulp.lpSum(y[p_def][t_] for p_def in Pdef) >= ED, f"Start_MinDEF_{t_}"
        if Pmid: model += pulp.lpSum(y[p_mid][t_] for p_mid in Pmid) >= EM, f"Start_MinMID_{t_}"
        if Pfwd: model += pulp.lpSum(y[p_fwd][t_] for p_fwd in Pfwd) >= EF, f"Start_MinFWD_{t_}"
        for p_ in p: # Linearization 4.24-4.26 (Ensure y <= x or y <= x_fh)
            model += y[p_][t_] <= x[p_][t_] + x_freehit[p_][t_], f"Start_InSquad_Or_FH_{p_}_{t_}" # Simpler link: must be in one squad type
            # Additionally, link y based on r[t] active status (original formulation)
            model += y[p_][t_] <= x_freehit[p_][t_] + lambda_var[p_][t_], f"Start_InSquad_LinkA_{p_}_{t_}" # y <= x_fh + lambda
            model += lambda_var[p_][t_] <= x[p_][t_], f"Start_InSquad_LinkB_{p_}_{t_}"             # lambda <= x
            model += lambda_var[p_][t_] <= 1 - r[t_], f"Start_InSquad_LinkC_{p_}_{t_}"           # lambda <= (1-r) -> lambda=0 if r=1

        # Captain/Vice (4.27 - 4.29)
        model += pulp.lpSum(f[p_][t_] for p_ in p) + pulp.lpSum(is_tc[p_][t_] for p_ in p) == 1, f"Captain_Or_TC_Unique_{t_}"
        model += pulp.lpSum(h[p_][t_] for p_ in p) == 1, f"ViceCaptain_Unique_{t_}"
        for p_ in p:
            model += f[p_][t_] + is_tc[p_][t_] + h[p_][t_] <= y[p_][t_], f"Captaincy_In_Lineup_{p_}_{t_}"

        # Substitution (4.30 - 4.32)
        if g: # Check if g exists
            for p_ngk in P_not_gk:
                model += y[p_ngk][t_] + pulp.lpSum(g[p_ngk][t_][l_] for l_ in l) <= \
                         x[p_ngk][t_] + x_freehit[p_ngk][t_], f"Sub_If_Benched_{p_ngk}_{t_}" # Simplified link: must be in one squad type
                # Also apply the original lambda formulation if needed for stricter logic
                model += y[p_ngk][t_] + pulp.lpSum(g[p_ngk][t_][l_] for l_ in l) <= \
                         x_freehit[p_ngk][t_] + lambda_var[p_ngk][t_], f"Sub_If_Benched_Lambda_{p_ngk}_{t_}"


            for l_ in l:
                model += pulp.lpSum(g[p_ngk][t_][l_] for p_ngk in P_not_gk) <= 1, f"Sub_Priority_Unique_{t_}_{l_}"

        # Transfer In/Out Limit (4.36)
        for p_ in p:
            model += e[p_][t_] + u[p_][t_] <= 1, f"Transfer_In_Out_Limit_{p_}_{t_}"

    # --- Linking Constraints & Evolution within Sub-Horizon ---
    if not t_sub: # Skip if horizon is empty
        print(f"WARNING: Skipping linking constraints for GW {current_gw} due to empty sub-horizon.")
    else:
        t1_sub = t_sub[0] # The current gameweek being implemented

        # --- Link Free Transfers (q[t1_sub]) ---
        model += q[t1_sub] == previous_ft, f"FT_Link_{t1_sub}"

        # --- Link Budget & Squad (for t1_sub) ---
        if current_gw == START_GAMEWEEK:
            # Initial budget constraint for the very first GW
            model += v[t1_sub] + pulp.lpSum(value_sub.loc[p_, t1_sub] * x[p_][t1_sub] for p_ in p) <= BS, "Budget_Initial_Upper" # Use <= if BS is max
            # Potentially add lower bound if needed:
            # model += v[t1_sub] + pulp.lpSum(value_sub.loc[p_, t1_sub] * x[p_][t1_sub] for p_ in p) >= BS - some_tolerance , "Budget_Initial_Lower"
        else:
            # Link Budget to previous state
            sales_value_t1 = pulp.lpSum(value_sub.loc[p_, t1_sub] * u[p_][t1_sub] for p_ in p)
            purchase_cost_t1 = pulp.lpSum(value_sub.loc[p_, t1_sub] * e[p_][t1_sub] for p_ in p)
            model += v[t1_sub] == previous_budget + sales_value_t1 - purchase_cost_t1, f"Budget_Link_{t1_sub}"

            # Link Squad to previous state
            for p_ in p:
                # x[p][current] = x[p][previous] - u[p][current] + e[p][current]
                model += x[p_][t1_sub] == previous_squad_dict.get(p_, 0) - u[p_][t1_sub] + e[p_][t1_sub], f"Squad_Link_{p_}_{t1_sub}"

        # --- Penalized Transfers for t1_sub ---
        # alpha[t] >= sum(e) - q[t]
        model += alpha[t1_sub] >= pulp.lpSum(e[p_][t1_sub] for p_ in p) - q[t1_sub], f"PenalizedTransfers_Calc_{t1_sub}"
        # alpha[t] <= M * (1 - w[t])  -- No penalty if WC active
        model += alpha[t1_sub] <= alpha_bar * (1 - w[t1_sub]), f"PenalizedTransfers_WC_Override_{t1_sub}"
        # alpha[t] <= M * (1 - r[t]) -- No penalty if FH active
        model += alpha[t1_sub] <= alpha_bar * (1 - r[t1_sub]), f"PenalizedTransfers_FH_Override_{t1_sub}" # Added FH override


        # --- Budget, Squad, Transfers Evolution & FH rules for t_ > t1_sub ---
        if n_T_sub > 1:
            for gw_idx in range(1, n_T_sub):
                t_curr_sub = t_sub[gw_idx]
                t_prev_sub = t_sub[gw_idx-1]

                # Budget Evolution (4.34)
                sales_value = pulp.lpSum(value_sub.loc[p_, t_curr_sub] * u[p_][t_curr_sub] for p_ in p)
                purchase_cost = pulp.lpSum(value_sub.loc[p_, t_curr_sub] * e[p_][t_curr_sub] for p_ in p)
                model += v[t_curr_sub] == v[t_prev_sub] + sales_value - purchase_cost, f"Budget_Evolution_{t_curr_sub}"

                # Squad Continuity (4.35) - Link based on FH status
                for p_ in p:
                    # If FH was active in t_prev_sub, squad in t_curr_sub comes from original t_prev_sub squad
                    # If FH is active in t_curr_sub, transfers are blocked anyway
                    # If neither FH active, normal continuity applies
                    # x[t] = x[t-1] - u[t] + e[t]  (Only when r[t] = 0)
                    # Need to link x[t] back to x[t-1] even if r[t-1] was active
                    # Let x_orig[t-1] be the squad if FH wasn't active

                    # This part is tricky. The original paper might imply squad reverts automatically.
                    # Let's stick to the simpler: transfer constraints handle FH.
                    # If r[t]=1, u[t] and e[t] are forced to 0 by FH_NoTransfers constraints.
                    # Then x[t] = x[t-1]. This seems correct according to 4.35.
                     model += x[p_][t_curr_sub] == x[p_][t_prev_sub] - u[p_][t_curr_sub] + e[p_][t_curr_sub], f"Squad_Continuity_{p_}_{t_curr_sub}"


                # Free Hit Budget Limit (4.37) - Use value from t_prev_sub for BOTH parts
                # Cost of FH squad (at t_curr_sub prices) <= Budget[t-1] + Value of NON-FH squad[t-1] (at t_prev_sub prices)
                # Ensure t_prev_sub is a valid column index in the full value matrix
                if t_prev_sub in value_matrix_df.columns:
                    value_prev_squad_t_prev_val = pulp.lpSum(value_matrix_df.loc[p_, t_prev_sub] * x[p_][t_prev_sub] for p_ in p)
                    cost_fh_squad_t_curr_val = pulp.lpSum(value_sub.loc[p_, t_curr_sub] * x_freehit[p_][t_curr_sub] for p_ in p)
                    M_fh_budget = BS + total_squad_size * 200 # Max possible squad value + budget (use generous M)

                    # cost_fh <= (v[t-1] + value_prev_squad) + M*(1-r[t])
                    model += cost_fh_squad_t_curr_val <= v[t_prev_sub] + value_prev_squad_t_prev_val + M_fh_budget * (1 - r[t_curr_sub]), f"FH_Budget_Limit_Upper_{t_curr_sub}"
                    # cost_fh >= (v[t-1] + value_prev_squad) - M*(1-r[t]) # Optional lower bound if needed tight
                    # cost_fh >= 0 is implicit

                else:
                     print(f"WARNING: Skipping FH Budget constraint for {t_curr_sub} as previous GW {t_prev_sub} not in value matrix columns.")


                # Free Hit Transfer Restriction (4.38 / 4.39)
                M_fh_transfer = total_squad_size # Max number of players in squad
                model += pulp.lpSum(u[p_][t_curr_sub] for p_ in p) <= M_fh_transfer * (1 - r[t_curr_sub]), f"FH_NoTransfersOut_{t_curr_sub}"
                model += pulp.lpSum(e[p_][t_curr_sub] for p_ in p) <= M_fh_transfer * (1 - r[t_curr_sub]), f"FH_NoTransfersIn_{t_curr_sub}"

                # Penalized Transfers Calculation (for t_curr_sub)
                model += alpha[t_curr_sub] >= pulp.lpSum(e[p_][t_curr_sub] for p_ in p) - q[t_curr_sub], f"PenalizedTransfers_Calc_{t_curr_sub}"
                model += alpha[t_curr_sub] <= alpha_bar * (1 - w[t_curr_sub]), f"PenalizedTransfers_WC_Override_{t_curr_sub}"
                model += alpha[t_curr_sub] <= alpha_bar * (1 - r[t_curr_sub]), f"PenalizedTransfers_FH_Override_{t_curr_sub}" # Added FH override

                # Free Transfer Evolution (q[t_next_sub] based on q[t_curr_sub])
                # Check if there IS a next gameweek within the sub-horizon
                if gw_idx < n_T_sub - 1:
                    t_next_sub = t_sub[gw_idx+1]

                    # Calculate potential carry-over from t_curr_sub
                    # Effective FT used = Transfers In - Penalized Transfers (ensure non-negative)
                    # This calculation is tricky. Let's use the formulation from the paper:
                    # q[t+1] = min(Q_bar, max(0, q[t] - sum(e[t]) + alpha[t]) + Q_under_bar) ... IF w[t]=0 and r[t]=0
                    # q[t+1] = Q_under_bar ... IF w[t]=1 or r[t]=1

                    # Auxiliary variable for carry-over: carry = max(0, q[t] - sum(e[t]) + alpha[t])
                    # Let ft_used_effectively = sum(e[t]) - alpha[t] (transfers paid for with FT)
                    # carry = max(0, q[t] - ft_used_effectively)
                    ft_used_effectively = pulp.lpSum(e[p_][t_curr_sub] for p_ in p) - alpha[t_curr_sub]
                    model += ft_carried_over_nonneg[t_curr_sub] >= q[t_curr_sub] - ft_used_effectively, f"FT_Carry_Calc_{t_curr_sub}"

                    # Calculate q[t+1] based on carry-over and chips
                    # Case 1: Normal evolution (no WC/FH in t_curr_sub)
                    # q[t+1] <= carry + Q_under_bar
                    # q[t+1] <= Q_bar
                    M_q = Q_bar + 1 # Big M for disabling constraints
                    model += q[t_next_sub] <= ft_carried_over_nonneg[t_curr_sub] + Q_under_bar + M_q * (w[t_curr_sub] + r[t_curr_sub]), f"Transfer_Evo_Normal_Upper1_{t_next_sub}"
                    model += q[t_next_sub] <= Q_bar + M_q * (w[t_curr_sub] + r[t_curr_sub]), f"Transfer_Evo_Normal_Upper2_{t_next_sub}"
                    # Ensure q[t+1] >= carry + Q_under_bar (if result < Q_bar) ? No, min operation handles this.

                    # Case 2: WC/FH active in t_curr_sub, q[t+1] = Q_under_bar
                    # q[t+1] <= Q_under_bar + M*(1-w[t])*(1-r[t])  -- If either chip is 1, M term is 0
                    model += q[t_next_sub] <= Q_under_bar + M_q * (1 - w[t_curr_sub]), f"Transfer_Reset_WC_Upper_{t_next_sub}"
                    model += q[t_next_sub] <= Q_under_bar + M_q * (1 - r[t_curr_sub]), f"Transfer_Reset_FH_Upper_{t_next_sub}"
                    # q[t+1] >= Q_under_bar - M*(1-w[t])*(1-r[t])
                    model += q[t_next_sub] >= Q_under_bar - M_q * (1 - w[t_curr_sub]), f"Transfer_Reset_WC_Lower_{t_next_sub}"
                    model += q[t_next_sub] >= Q_under_bar - M_q * (1 - r[t_curr_sub]), f"Transfer_Reset_FH_Lower_{t_next_sub}"


                    # Constraint 4.42: No penalized transfers if max FT are carried over (q[t+1] == Q_bar implies alpha[t] = 0)
                    # alpha[t] = 0 if q[t+1] >= Q_bar
                    # alpha[t] <= M * (1 - indicator(q[t+1] >= Q_bar))
                    # alpha[t] <= M * (Q_bar - q[t+1]) # If q[t+1]=Q_bar, RHS=0. If q[t+1]<Q_bar, RHS is positive.
                    # Need integer tolerance epsilon_q = 0.1
                    model += alpha[t_curr_sub] <= alpha_bar * (Q_bar - q[t_next_sub] + epsilon), f"Transfer_NoPaidIfMaxFT_{t_curr_sub}"


    print(f"Constraints added in {time.time() - cons_start:.2f}s")

    # 7. Solve Sub-Problem
    print("Solving model...")
    solve_start_time = time.time()

    # --- Threading using Dictionary for results ---
    thread_results = {"status": None, "exception": None} # Use a dictionary

    def solve_in_thread_dict(results_dict): # Pass the dict
        """ Function executed in a separate thread to solve the PuLP model. """
        try:
            print(f"[{threading.get_ident()}] Solver thread started...")
            # Ensure solver uses the class variable 'solver'
            solve_status_code = model.solve(solver)
            results_dict["status"] = solve_status_code # Modify the dict
            print(f"[{threading.get_ident()}] Solver thread finished with status: {results_dict['status']}.")
        except Exception as thread_e:
            results_dict["exception"] = thread_e # Modify the dict
            print(f"[{threading.get_ident()}] Solver thread encountered an error: {thread_e}")

    solver_thread = threading.Thread(target=solve_in_thread_dict, args=(thread_results,))
    solver_thread.start()

    print(f"Main thread waiting for solver (updates every {PROGRESS_UPDATE_INTERVAL}s)...")
    wait_total = 0
    while solver_thread.is_alive():
        elapsed_thread_time = time.time() - solve_start_time
        print(f"[{time.strftime('%H:%M:%S')}] Solver running... (Elapsed: {elapsed_thread_time:.0f}s)")

        # Wait smartly, checking frequently if the thread finished
        wait_chunk = 1 # Check every second
        time.sleep(wait_chunk)
        wait_total += wait_chunk

        # Check against overall time limit (if set)
        if SOLVER_TIME_LIMIT and elapsed_thread_time > SOLVER_TIME_LIMIT + 5: # Add small buffer
             print("Solver likely hit primary time limit, loop will break.")
             # No need to break here, rely on thread finishing or join timeout

    # Wait for thread to complete, with a timeout slightly longer than the limit
    join_timeout = (solver_time_limit_int + 15) if solver_time_limit_int is not None else None
    solver_thread.join(timeout=join_timeout)
    solve_time = time.time() - solve_start_time

    # Check results from the dictionary
    solver_exception = thread_results["exception"]
    solve_status = thread_results["status"]

    if solver_exception:
        print("\n--- Solver Error ---")
        print(f"An error occurred in the solver thread: {solver_exception}")
        status = -1000 # Custom error code for thread exception
    elif solve_status is None:
        # Check if the thread is still alive (likely timed out by .join())
        if solver_thread.is_alive():
            print("\nERROR: Solver thread timed out or did not complete.")
            # You might try to terminate it, but it's generally difficult/unsafe
            # pulp.solvers.CPLEX_CMD().killProcess() # Example - specific to solver/OS
            status = -1001 # Custom error code for timeout
        else:
            print("\nERROR: Solver status was not captured, but thread finished. Unknown issue.")
            status = -1002 # Custom error code for unknown state
    else:
        status = solve_status # Use the status code returned by the solver

    print(f"\n--- Solve Complete for GW {current_gw} ---")
    print(f"Final Solver Status Code: {status}")
    # Check if status is a known PuLP status code
    if isinstance(status, int) and status in pulp.LpStatus:
         print(f"Final Solver Status: {pulp.LpStatus[status]}") # Use dict lookup
    elif isinstance(status, int):
         print(f"Final Solver Status: Unknown Status Code ({status})")
    else:
         print(f"Final Solver Status: Invalid Status ({status})")

    print(f"Sub-problem Solve Time: {solve_time:.2f} seconds")


    # 8. Implement Decision & Store Results (for current_gw = t1_sub)
    # Check if optimal OR if not solved but a time limit was hit (meaning a feasible solution might exist)
    solution_acceptable = (status == pulp.LpStatusOptimal) or \
                          (status == pulp.LpStatusNotSolved and SOLVER_TIME_LIMIT is not None and model.solutionTime > 0)

    if solution_acceptable:
        if status == pulp.LpStatusNotSolved:
             print("WARNING: Using potentially non-optimal feasible solution due to time limit.")

        # Check if t1_sub is valid before extracting
        if not t_sub:
             print(f"ERROR: Cannot extract results for GW {current_gw}, sub-horizon is empty.")
             break

        t1_sub = t_sub[0] # Ensure t1_sub is defined for extraction

        try:
            print(f"Extracting results for GW {current_gw} (t={t1_sub})...")
            gw_results = {'gameweek': current_gw}

            # Helper function to safely get variable values
            def get_var_val(var):
                try:
                    return var.varValue
                except AttributeError:
                    # print(f"Warning: Could not get value for variable {var.name}. Using 0.")
                    return 0.0 # Return default if variable value isn't available (e.g., infeasible)

            # Extract decisions for current_gw (t1_sub)
            gw_results['squad'] = sorted([p_ for p_ in p if get_var_val(x[p_][t1_sub]) > 0.99])
            gw_results['lineup'] = sorted([p_ for p_ in p if get_var_val(y[p_][t1_sub]) > 0.99])
            gw_results['captain'] = [p_ for p_ in p if get_var_val(f[p_][t1_sub]) > 0.99]
            # Handle case where TC might be active instead of regular captain
            if not gw_results['captain']:
                 gw_results['captain'] = [p_ for p_ in p if get_var_val(is_tc[p_][t1_sub]) > 0.99]

            gw_results['vice_captain'] = [p_ for p_ in p if get_var_val(h[p_][t1_sub]) > 0.99]
            gw_results['transfers_in'] = sorted([p_ for p_ in p if get_var_val(e[p_][t1_sub]) > 0.99])
            gw_results['transfers_out'] = sorted([p_ for p_ in p if get_var_val(u[p_][t1_sub]) > 0.99])
            gw_results['budget_end'] = get_var_val(v[t1_sub])
            # Round alpha value after getting it
            alpha_val = get_var_val(alpha[t1_sub])
            gw_results['alpha'] = round(alpha_val) if alpha_val is not None else 0
            gw_results['q_start'] = previous_ft # FT available at start of this GW
            # Safely get objective value
            try:
                 gw_results['objective_value'] = model.objective.value()
            except AttributeError:
                 gw_results['objective_value'] = None


            # Record chip usage for this GW
            wc_active = get_var_val(w[t1_sub]) > 0.99
            bb_active = get_var_val(b[t1_sub]) > 0.99
            fh_active = get_var_val(r[t1_sub]) > 0.99
            tc_player_list = [p_ for p_ in p if get_var_val(is_tc[p_][t1_sub]) > 0.99]
            gw_results['chip_played'] = None
            if wc_active: gw_results['chip_played'] = 'WC'
            if bb_active: gw_results['chip_played'] = 'BB'
            if fh_active: gw_results['chip_played'] = 'FH'
            if tc_player_list:
                 tc_player_id = tc_player_list[0]
                 tc_player_name = player_name_map.get(tc_player_id, str(tc_player_id))
                 gw_results['chip_played'] = f'TC_{tc_player_name}' # Use name if available
                 # Overwrite captain if TC is active
                 gw_results['captain'] = tc_player_list


            master_results.append(gw_results)

            # 9. Update State for Next Iteration
            previous_squad_dict = {p_: 1 for p_ in gw_results['squad']}
            previous_budget = gw_results['budget_end'] if gw_results['budget_end'] is not None else previous_budget # Handle None budget

            # Calculate free transfers for next GW (q[current_gw + 1])
            q_current_start = previous_ft # FT available at start of current_gw
            alpha_current = gw_results['alpha']
            e_current_sum = len(gw_results['transfers_in'])

            if wc_active or fh_active:
                previous_ft = Q_under_bar # Reset FT for next GW
            else:
                # Effective FT used = Transfers In - Penalized Transfers (ensure non-negative)
                ft_used_current = max(0, e_current_sum - alpha_current)
                # Carry over = Available at start - Used (must be >= 0)
                ft_carry_current = max(0, q_current_start - ft_used_current)
                # Next FT = min(Max FT, Carry Over + Base FT)
                # Use math.ceil or round based on how FPL rules work (usually round down, so floor might be better? Using round for now)
                previous_ft = min(Q_bar, round(ft_carry_current + Q_under_bar))


            print(f"End of GW {current_gw}: Budget={previous_budget:.1f}, Next FT={previous_ft}")

            # Update used chips tracker
            if wc_active:
                 if current_gw in T_FH_overall and not used_chips_tracker['wc1']:
                     used_chips_tracker['wc1'] = True
                     print("--- WC1 activated ---")
                 elif current_gw in T_SH_overall and not used_chips_tracker['wc2']:
                     used_chips_tracker['wc2'] = True
                     print("--- WC2 activated ---")
            if bb_active and not used_chips_tracker['bb']:
                 used_chips_tracker['bb'] = True
                 print("--- BB activated ---")
            if fh_active and not used_chips_tracker['fh']:
                 used_chips_tracker['fh'] = True
                 print("--- FH activated ---")
            if tc_player_list and not used_chips_tracker['tc']:
                 used_chips_tracker['tc'] = True
                 tc_player_id = tc_player_list[0]
                 tc_player_name = player_name_map.get(tc_player_id, str(tc_player_id))
                 print(f"--- TC activated on {tc_player_name} ---")

        except Exception as extract_err:
            print(f"!!! Error extracting results for GW {current_gw}: {extract_err} !!!")
            # Optionally print more details about variables if extraction fails
            # for p_ in p: print(f"x[{p_}][{t1_sub}] = {get_var_val(x[p_][t1_sub])}")
            print("Stopping rolling horizon due to extraction error.")
            break

    else:
        print(f"!!! Optimization failed or stopped without an acceptable solution for GW {current_gw}. Status: {status} ({pulp.LpStatus.get(status, 'Unknown')}). Stopping rolling horizon. !!!")
        # You could try saving the model state here for debugging
        # model.writeLP(f"failed_model_gw{current_gw}.lp")
        break

    print(f"Total time for GW {current_gw} loop: {time.time() - loop_start_time:.2f}s")


# --- Post-Loop: Process and Display Results ---
print("\n" + "="*20 + " Final Rolling Horizon Results " + "="*20)
if not master_results:
    print("No results were generated.")
else:
    results_df = pd.DataFrame(master_results)

    # Add player names for readability
    def map_ids_to_names(id_list):
        return sorted([player_name_map.get(p_id, f"ID:{p_id}") for p_id in id_list])

    # Display summary per GW
    for index, row in results_df.iterrows():
        gw = row['gameweek']
        print(f"\n--- GW {gw} Summary ---")
        print(f"  Chip Played: {row['chip_played']}")
        transfers_in_names = map_ids_to_names(row['transfers_in'])
        transfers_out_names = map_ids_to_names(row['transfers_out'])
        captain_name = map_ids_to_names(row['captain'])
        vice_captain_name = map_ids_to_names(row['vice_captain'])
        print(f"  Transfers In ({len(transfers_in_names)}): {transfers_in_names}")
        print(f"  Transfers Out ({len(transfers_out_names)}): {transfers_out_names}")
        print(f"  FT Available (Start): {row['q_start']}")
        print(f"  Penalized Transfers (Hits): {row['alpha']}")
        print(f"  Captain: {captain_name[0] if captain_name else 'None'}")
        print(f"  Vice-Captain: {vice_captain_name[0] if vice_captain_name else 'None'}")
        print(f"  Budget End: {row['budget_end']:.1f}")
        print(f"  Objective Value (Sub-problem): {row['objective_value']:.2f}" if row['objective_value'] is not None else "Objective Value: N/A")
        # Optional: Display full squad/lineup
        squad_names = map_ids_to_names(row['squad'])
        lineup_names = map_ids_to_names(row['lineup'])
        print(f"  Squad ({len(squad_names)}): {squad_names}")
        print(f"  Lineup ({len(lineup_names)}): {lineup_names}")


    # Save results if needed
    try:
        results_df.to_csv("rolling_horizon_results.csv", index=False)
        print("\nResults saved to rolling_horizon_results.csv")
    except Exception as save_e:
        print(f"\nERROR: Could not save results to CSV. Reason: {save_e}")

print("\n--- Script Finished ---")