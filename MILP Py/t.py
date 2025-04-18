import pandas as pd
import pulp
import numpy as np
import time
import sys
import os
import math # Used for rounding FT
# Removed threading import

# --- Input Parameters ---
START_GAMEWEEK = 77 # Example: Start of the 3rd season (1+38+38)
MAX_GAMEWEEK = 77+28 # Example: Run for a few GWs
SUB_HORIZON_LENGTH = 28 # Look ahead N weeks
# *** IMPORTANT: Update this path to your actual file location ***
CSV_FILE_PATH = "C:/Users/peram/Documents/test/Stigende GW, alle tre sesonger(22-24).csv" # <<< POINT TO RAW, NON-AGGREGATED FILE
# --- SET TIMELIMIT HERE ---
SOLVER_TIME_LIMIT = None  # Seconds (e.g., 600 for 10 mins) or None for no limit
# PROGRESS_UPDATE_INTERVAL = 60 # Removed

# --- Define Target Gameweeks for Chips ---
# *** ADJUST THESE BASED ON THE SEASON YOU ARE ANALYZING ***
SEASON_START_GW = 77 # GW number corresponding to GW1 of the target season
TARGET_GW_WC1_OPPORTUNITY = None #SEASON_START_GW + 9 - 1 # GW9 of the season or None
TARGET_GW_TC = None# SEASON_START_GW + 22 - 1 # GW22 of the season or None
TARGET_GW_FH =None# SEASON_START_GW + 31 - 1 # GW31 of the season or None
TARGET_GW_WC2_OPPORTUNITY = None#SEASON_START_GW + 33 - 1 # GW33 of the season or None
TARGET_GW_BB = None#SEASON_START_GW + 34 - 1 # GW34 of the season or None
print(f"Chip Opportunity GWs (Absolute): WC1={TARGET_GW_WC1_OPPORTUNITY}, TC={TARGET_GW_TC}, FH={TARGET_GW_FH}, WC2={TARGET_GW_WC2_OPPORTUNITY}, BB={TARGET_GW_BB}")


# --- Configure Solver (Default Threads) ---
solver_to_use = pulp.PULP_CBC_CMD(
    msg=True, # Show solver output
    timeLimit=SOLVER_TIME_LIMIT # Pass timeLimit if set
)
print(f"--- Using Solver: {solver_to_use.name} (Default Threads) ---")
if SOLVER_TIME_LIMIT:
    print(f"--- Solver Time Limit per Subproblem: {SOLVER_TIME_LIMIT}s ---")
else:
    print("--- Solver Time Limit per Subproblem: None ---")


# --- Check Versions ---
print(f"--- Running on Python {sys.version} ---")
print(f"--- PuLP Version: {pulp.__version__} ---")

# --- 0. Setup & Data Loading ---
print("\n--- 0. Setup & Data Loading ---")
try:
    allesesonger = pd.read_csv(CSV_FILE_PATH)
    print(f"Raw CSV '{CSV_FILE_PATH}' loaded successfully.")
except FileNotFoundError:
    print(f"ERROR: Raw CSV file '{CSV_FILE_PATH}' not found.")
    sys.exit()
except Exception as e:
    print(f"ERROR: Failed to load raw CSV file. Reason: {e}")
    sys.exit()

print("Initial raw data shape:", allesesonger.shape)

# --- Data Pre-processing and Filtering ---
print("\n--- Pre-processing Raw Data ---")
essential_input_cols = ['player_id', 'GW', 'name', 'position', 'team', 'total_points', 'value']
missing_cols = [col for col in essential_input_cols if col not in allesesonger.columns]
if missing_cols: print(f"ERROR: Missing essential columns: {missing_cols}"); sys.exit()
try:
    allesesonger['GW'] = pd.to_numeric(allesesonger['GW'])
    allesesonger['value'] = pd.to_numeric(allesesonger['value'])
    allesesonger['value'] = allesesonger['value'].fillna(50.0)
except ValueError as e: print(f"ERROR: Could not convert GW or value: {e}"); sys.exit()
data_load_start_gw = START_GAMEWEEK - 1 if START_GAMEWEEK > 1 else START_GAMEWEEK
data_full_range_raw = allesesonger[(allesesonger['GW'] >= data_load_start_gw) & (allesesonger['GW'] <= MAX_GAMEWEEK)].copy()
if data_full_range_raw.empty: print(f"ERROR: No raw data for GW range {data_load_start_gw}-{MAX_GAMEWEEK}."); sys.exit()
print(f"Filtered raw data for GW {data_load_start_gw}-{MAX_GAMEWEEK}. Shape: {data_full_range_raw.shape}")
for col in ['team', 'position', 'name']:
    if col in data_full_range_raw.columns: data_full_range_raw[col] = data_full_range_raw[col].astype(str).str.strip()

# --- 1. Data Cleaning, Aggregation & FULL Set Definition ---
print("\n--- 1. Data Cleaning, Aggregation & FULL Set Definition ---")
T_setofgameweeks_full = sorted(data_full_range_raw['GW'].unique())
P_setofplayers_initial = sorted(data_full_range_raw['player_id'].unique())
C_setofteams_initial = sorted(data_full_range_raw['team'].dropna().unique())
n_T_full = len(T_setofgameweeks_full); n_P_initial = len(P_setofplayers_initial)
if n_T_full == 0 or n_P_initial == 0: print("ERROR: Initial GW or Player set empty."); sys.exit()
print(f"Initial FULL sets: {n_T_full} GWs, {n_P_initial} Players, {len(C_setofteams_initial)} Teams.")
player_gw_combos_full = pd.MultiIndex.from_product([P_setofplayers_initial, T_setofgameweeks_full], names=['player_id', 'GW'])
data_complete_full = pd.DataFrame(index=player_gw_combos_full).reset_index()
print("Merging and filling missing data...")
data_merged_full = pd.merge(data_complete_full, data_full_range_raw, on=['player_id', 'GW'], how='left', suffixes=('', '_discard'))
data_merged_full = data_merged_full[[col for col in data_merged_full.columns if not col.endswith('_discard')]]
data_merged_full.sort_values(by=['player_id', 'GW'], inplace=True)
essential_info_cols = ['name', 'position', 'team']
for col in essential_info_cols: data_merged_full[col] = data_merged_full.groupby('player_id')[col].transform(lambda x: x.ffill().bfill())
data_merged_full['total_points'] = data_merged_full['total_points'].fillna(0)
data_merged_full['value'] = data_merged_full.groupby('player_id')['value'].transform(lambda x: x.ffill().bfill())
data_merged_full['value'] = data_merged_full['value'].fillna(50.0)
print("Aggregating data for Double Gameweeks...")
sum_cols = ['total_points', 'minutes', 'goals_scored', 'assists', 'bonus', 'bps', 'saves', 'penalties_saved']
first_cols = ['value', 'name', 'position', 'team']
group_cols = ['player_id', 'GW']; agg_funcs = {}
for col in sum_cols:
    if col in data_merged_full.columns: agg_funcs[col] = 'sum'
for col in first_cols:
    if col in data_merged_full.columns: agg_funcs[col] = 'first'
data_aggregated = data_merged_full.groupby(group_cols, as_index=False).agg(agg_funcs)
print(f"Data shape after aggregation: {data_aggregated.shape}")
data_cleaned_full = data_aggregated.copy()
print("Filtering invalid rows post-aggregation...")
initial_rows_agg = data_cleaned_full.shape[0]
data_cleaned_full = data_cleaned_full.dropna(subset=essential_info_cols).copy()
data_cleaned_full['team'] = data_cleaned_full['team'].astype(str)
C_setofteams_initial_str = [str(team) for team in C_setofteams_initial]
data_cleaned_full = data_cleaned_full[data_cleaned_full['team'].isin(C_setofteams_initial_str)]
rows_after_filter_agg = data_cleaned_full.shape[0]
print(f"Removed {initial_rows_agg - rows_after_filter_agg} rows post-aggregation.")
if data_cleaned_full.empty: print("ERROR: No data remaining after aggregation/cleaning."); sys.exit()
print(f"Cleaned aggregated data shape: {data_cleaned_full.shape}")

print("Defining final sets and parameters...")
T_setofgameweeks_full = sorted(data_cleaned_full['GW'].unique())
if START_GAMEWEEK not in T_setofgameweeks_full:
     available_gws_after_start = [gw for gw in T_setofgameweeks_full if gw >= START_GAMEWEEK]
     if not available_gws_after_start: print(f"ERROR: START_GAMEWEEK {START_GAMEWEEK} not in cleaned data range."); sys.exit()
     actual_start_gw = min(available_gws_after_start); print(f"Warning: START_GAMEWEEK {START_GAMEWEEK} adjusted to actual start {actual_start_gw}."); START_GAMEWEEK = actual_start_gw
     T_setofgameweeks_full = sorted([gw for gw in T_setofgameweeks_full if gw >= START_GAMEWEEK])
     if not T_setofgameweeks_full: print("ERROR: No gameweeks left after adjusting START_GAMEWEEK."); sys.exit()
p = sorted(data_cleaned_full['player_id'].unique())
l = list(range(1, 4))
final_player_info = data_cleaned_full.drop_duplicates(subset=['player_id'], keep='first')
player_name_map = final_player_info.set_index('player_id')['name'].to_dict()
pos_map = pd.Series(final_player_info['position'].values, index=final_player_info['player_id'])
C_setofteams_final = sorted(final_player_info['team'].unique())
Pgk = sorted([p_ for p_ in p if pos_map.get(p_) == "GK"]); Pdef = sorted([p_ for p_ in p if pos_map.get(p_) == "DEF"])
Pmid = sorted([p_ for p_ in p if pos_map.get(p_) == "MID"]); Pfwd = sorted([p_ for p_ in p if pos_map.get(p_) == "FWD"])
P_not_gk = sorted([p_ for p_ in p if p_ not in Pgk])
P_c_all = data_cleaned_full[data_cleaned_full['player_id'].isin(p)].groupby('team')['player_id'].unique().apply(list).to_dict()
P_c = {team: sorted(players) for team, players in P_c_all.items() if team in C_setofteams_final and players}
C_setofteams = sorted(list(P_c.keys()))
n_P = len(p); n_C = len(C_setofteams); n_L = len(l)
print(f"Sets defined for run: {len(T_setofgameweeks_full)} GWs ({min(T_setofgameweeks_full)}-{max(T_setofgameweeks_full)}), {n_P} Players, {n_C} Teams.")
print(f"  Positions: {len(Pgk)} GK, {len(Pdef)} DEF, {len(Pmid)} MID, {len(Pfwd)} FWD")
season_num = math.ceil(START_GAMEWEEK / 38); gw1_of_this_season = (season_num - 1) * 38 + 1
mid_season_split_gw = gw1_of_this_season + 19 -1
T_FH_overall = [t_ for t_ in T_setofgameweeks_full if t_ <= mid_season_split_gw]
T_SH_overall = [t_ for t_ in T_setofgameweeks_full if t_ > mid_season_split_gw]
print(f"  Season {season_num} Halves (Absolute GWs): FH <= {mid_season_split_gw}, SH > {mid_season_split_gw}")
R_penalty = 4; MK = 2; MD = 5; MM = 5; MF = 3; MC = 3; E = 11; EK = 1; ED = 3; EM = 2; EF = 1; BS = 1000.0
phi = (MK + MD + MM + MF) - E; phi_K = MK - EK; Q_bar = 2; Q_under_bar = 1; epsilon = 0.1; kappa = {1: 0.01, 2: 0.005, 3: 0.001}
M_transfer = MK + MD + MM + MF; M_budget = BS + M_transfer * 200; M_alpha = M_transfer + Q_bar; M_q = Q_bar + 1; epsilon_q = 0.1
print("Parameters defined.")

print("Preparing full coefficient matrices...")
try:
    points_matrix_df = data_cleaned_full.pivot(index='player_id', columns='GW', values='total_points')
    value_matrix_df = data_cleaned_full.pivot(index='player_id', columns='GW', values='value')
    points_matrix_df = points_matrix_df.reindex(index=p, columns=T_setofgameweeks_full, fill_value=0.0)
    value_matrix_df = value_matrix_df.reindex(index=p, columns=T_setofgameweeks_full)
    for player_id in p: value_matrix_df.loc[player_id] = value_matrix_df.loc[player_id].ffill().bfill()
    value_matrix_df.fillna(50.0, inplace=True)
    print("Full coefficient matrices ready.")
except Exception as e: print(f"ERROR creating full pivot tables: {e}"); sys.exit()

# --- Initialize State Variables ---
master_results = []
previous_squad_dict = {}
previous_budget = BS
previous_ft = 1
used_chips_tracker = {'wc1': False, 'wc2': False, 'bb': False, 'tc': False, 'fh': False}

# --- Rolling Horizon Loop ---
print("\n--- Starting Rolling Horizon ---")
for current_gw in range(START_GAMEWEEK, MAX_GAMEWEEK + 1):
    if current_gw not in T_setofgameweeks_full:
        print(f"Skipping Gameweek {current_gw} as it's not in the loaded data range.")
        continue

    print(f"\n{'='*15} Solving for Gameweek {current_gw} {'='*15}")
    loop_start_time = time.time()

    # 1. Define Sub-Horizon Gameweeks
    t_sub = sorted([gw for gw in T_setofgameweeks_full if gw >= current_gw and gw < current_gw + SUB_HORIZON_LENGTH])
    if not t_sub: print(f"Sub-horizon empty for GW {current_gw}."); break
    n_T_sub = len(t_sub); print(f"Sub-horizon GWs: {t_sub}"); t1_sub = t_sub[0]
    if t1_sub not in points_matrix_df.columns or t1_sub not in value_matrix_df.columns: print(f"ERROR: Missing coefficient data for GW {t1_sub}. Stopping."); break
    if not all(player_id in points_matrix_df.index for player_id in p): missing_p = [pid for pid in p if pid not in points_matrix_df.index]; print(f"ERROR: Players {missing_p} missing. Stopping."); break

    # 2. Create New Model Instance
    model = pulp.LpProblem(f"FPL_Opt_GW{current_gw}_Sub{SUB_HORIZON_LENGTH}", pulp.LpMaximize)

    # 3. Define Variables for Sub-Horizon
    print("Defining variables for sub-horizon...")
    var_start = time.time()
    x = pulp.LpVariable.dicts("Squad", (p, t_sub), cat='Binary')
    x_freehit = pulp.LpVariable.dicts("Squad_FH", (p, t_sub), cat='Binary')
    y = pulp.LpVariable.dicts("Lineup", (p, t_sub), cat='Binary')
    f = pulp.LpVariable.dicts("Captain", (p, t_sub), cat='Binary')
    h = pulp.LpVariable.dicts("ViceCaptain", (p, t_sub), cat='Binary')
    is_tc = pulp.LpVariable.dicts("TripleCaptainChipActive", (p, t_sub), cat='Binary')
    u = pulp.LpVariable.dicts("TransferOut", (p, t_sub), cat='Binary')
    e = pulp.LpVariable.dicts("TransferIn", (p, t_sub), cat='Binary')
    lambda_var = pulp.LpVariable.dicts("Aux_LineupInSquad", (p, t_sub), cat='Binary')
    g = {}
    if P_not_gk and l: g = pulp.LpVariable.dicts("Substitution", (P_not_gk, t_sub, l), cat='Binary')
    w = pulp.LpVariable.dicts("WildcardChipActive", t_sub, cat='Binary')
    b = pulp.LpVariable.dicts("BenchBoostChipActive", t_sub, cat='Binary')
    r = pulp.LpVariable.dicts("FreeHitChipActive", t_sub, cat='Binary')
    v = pulp.LpVariable.dicts("RemainingBudget", t_sub, lowBound=0, cat='Continuous')
    q = pulp.LpVariable.dicts("FreeTransfersAvailable", t_sub, lowBound=0, upBound=Q_bar, cat='Integer')
    alpha = pulp.LpVariable.dicts("PenalizedTransfers", t_sub, lowBound=0, upBound=M_alpha, cat='Integer')
    ft_carried_over_nonneg = pulp.LpVariable.dicts("FT_Carry", t_sub, lowBound=0)
    print(f"Variables defined in {time.time() - var_start:.2f}s")

    # 4. Filter Parameters for Sub-Horizon
    points_sub = points_matrix_df.loc[p, t_sub]
    value_sub = value_matrix_df.loc[p, t_sub]

    # 5. Define Objective for Sub-Horizon
    print("Defining objective function...")
    obj_start = time.time()
    points_from_lineup = pulp.lpSum(points_sub.loc[p_, t_] * y[p_][t_] for p_ in p for t_ in t_sub)
    points_from_captain = pulp.lpSum(points_sub.loc[p_, t_] * f[p_][t_] for p_ in p for t_ in t_sub)
    points_from_vice = pulp.lpSum(epsilon * points_sub.loc[p_, t_] * h[p_][t_] for p_ in p for t_ in t_sub)
    points_from_tc = pulp.lpSum(2 * points_sub.loc[p_, t_] * is_tc[p_][t_] for p_ in p for t_ in t_sub)
    points_from_subs = 0
    if g: points_from_subs = pulp.lpSum(kappa[l_] * points_sub.loc[p_ngk][t_] * g[p_ngk][t_][l_] for p_ngk in P_not_gk for t_ in t_sub for l_ in l)
    transfer_penalty = pulp.lpSum(R_penalty * alpha[t_] for t_ in t_sub)
    objective = (points_from_lineup + points_from_captain + points_from_vice +
                 points_from_tc + points_from_subs - transfer_penalty)
    model += objective, "Total_Expected_Points_Sub"
    print(f"Objective defined in {time.time() - obj_start:.2f}s")

    # 6. Define Constraints for Sub-Horizon
    print("Adding constraints...")
    cons_start = time.time()

    # --- Gamechips (with Timing Restrictions) ---
    print("  Adding Gamechip constraints (with timing)...")
    # ... (Chip timing logic using TARGET_GW_... and used_chips_tracker - Correct) ...
    wc1_available = not used_chips_tracker['wc1']; wc2_available = not used_chips_tracker['wc2']
    tc_available = not used_chips_tracker['tc']; bb_available = not used_chips_tracker['bb']; fh_available = not used_chips_tracker['fh']
    for t_ in t_sub:
        wc1_target_is_none = TARGET_GW_WC1_OPPORTUNITY is None; wc2_target_is_none = TARGET_GW_WC2_OPPORTUNITY is None
        tc_target_is_none = TARGET_GW_TC is None; bb_target_is_none = TARGET_GW_BB is None; fh_target_is_none = TARGET_GW_FH is None
        is_in_first_half = t_ <= mid_season_split_gw; is_in_second_half = t_ > mid_season_split_gw
        # WC1
        if is_in_first_half:
            if not wc1_available or (not wc1_target_is_none and t_ != TARGET_GW_WC1_OPPORTUNITY): model += w[t_] == 0, f"WC1_NotAvailableOrNotTarget_{t_}"
        elif is_in_second_half: # WC2
             if not wc2_available or (not wc2_target_is_none and t_ != TARGET_GW_WC2_OPPORTUNITY): model += w[t_] == 0, f"WC2_NotAvailableOrNotTarget_{t_}"
        else: model += w[t_] == 0, f"WC_NotInSeasonHalf_{t_}"
        # TC
        if not tc_available or (not tc_target_is_none and t_ != TARGET_GW_TC): model += pulp.lpSum(is_tc[p_][t_] for p_ in p) == 0, f"TC_NotAvailableOrNotTarget_{t_}"
        else: model += pulp.lpSum(is_tc[p_][t_] for p_ in p) <= 1, f"TC_Possible_{t_}"
        # BB
        if not bb_available or (not bb_target_is_none and t_ != TARGET_GW_BB): model += b[t_] == 0, f"BB_NotAvailableOrNotTarget_{t_}"
        # FH
        if not fh_available or (not fh_target_is_none and t_ != TARGET_GW_FH): model += r[t_] == 0, f"FH_NotAvailableOrNotTarget_{t_}"
        # One Chip Per Week
        model += w[t_] + pulp.lpSum(is_tc[p_][t_] for p_ in p) + b[t_] + r[t_] <= 1, f"GC_OneChipPerWeek_{t_}"
    # Overall Chip Limits
    t_sub_in_fh_overall = [t for t in t_sub if t <= mid_season_split_gw]; t_sub_in_sh_overall = [t for t in t_sub if t > mid_season_split_gw]
    if t_sub_in_fh_overall: model += pulp.lpSum(w[t_] for t_ in t_sub_in_fh_overall) <= (1 if wc1_available else 0), "WC_Limit_FH_Sub"
    if t_sub_in_sh_overall: model += pulp.lpSum(w[t_] for t_ in t_sub_in_sh_overall) <= (1 if wc2_available else 0), "WC_Limit_SH_Sub"


    # --- Squad, FH Squad, Lineup, Captain, Subs (Iterate over t_sub) ---
    print("  Adding Squad, Lineup, Captain, Sub constraints...")
    # ... (These constraints remain the same - Correct) ...
    squad_size_total = MK + MD + MM + MF
    for t_ in t_sub:
        if Pgk: model += pulp.lpSum(x[p_gk][t_] for p_gk in Pgk) == MK, f"Squad_GK_{t_}"
        if Pdef: model += pulp.lpSum(x[p_def][t_] for p_def in Pdef) == MD, f"Squad_DEF_{t_}"
        if Pmid: model += pulp.lpSum(x[p_mid][t_] for p_mid in Pmid) == MM, f"Squad_MID_{t_}"
        if Pfwd: model += pulp.lpSum(x[p_fwd][t_] for p_fwd in Pfwd) == MF, f"Squad_FWD_{t_}"
        for c_team in C_setofteams:
            players_in_team = P_c.get(c_team, [])
            if players_in_team: model += pulp.lpSum(x[p_tm][t_] for p_tm in players_in_team) <= MC, f"Squad_TeamLimit_{c_team}_{t_}"
        if Pgk: model += pulp.lpSum(x_freehit[p_gk][t_] for p_gk in Pgk) == MK * r[t_], f"FH_Squad_GK_{t_}"
        if Pdef: model += pulp.lpSum(x_freehit[p_def][t_] for p_def in Pdef) == MD * r[t_], f"FH_Squad_DEF_{t_}"
        if Pmid: model += pulp.lpSum(x_freehit[p_mid][t_] for p_mid in Pmid) == MM * r[t_], f"FH_Squad_MID_{t_}"
        if Pfwd: model += pulp.lpSum(x_freehit[p_fwd][t_] for p_fwd in Pfwd) == MF * r[t_], f"FH_Squad_FWD_{t_}"
        model += pulp.lpSum(x_freehit[p_][t_] for p_ in p) == squad_size_total * r[t_], f"FH_Squad_TotalSize_{t_}"
        for c_team in C_setofteams:
            players_in_team = P_c.get(c_team, [])
            if players_in_team: model += pulp.lpSum(x_freehit[p_tm][t_] for p_tm in players_in_team) <= MC * r[t_], f"FH_Squad_TeamLimit_{c_team}_{t_}"
        model += pulp.lpSum(y[p_][t_] for p_ in p) == E + phi * b[t_], f"Start_Size_{t_}"
        if Pgk: model += pulp.lpSum(y[p_gk][t_] for p_gk in Pgk) == EK + phi_K * b[t_], f"Start_GK_{t_}"
        if Pdef: model += pulp.lpSum(y[p_def][t_] for p_def in Pdef) >= ED, f"Start_MinDEF_{t_}"
        if Pmid: model += pulp.lpSum(y[p_mid][t_] for p_mid in Pmid) >= EM, f"Start_MinMID_{t_}"
        if Pfwd: model += pulp.lpSum(y[p_fwd][t_] for p_fwd in Pfwd) >= EF, f"Start_MinFWD_{t_}"
        for p_ in p:
            model += y[p_][t_] <= x_freehit[p_][t_] + lambda_var[p_][t_], f"Start_InSquad_LinkA_{p_}_{t_}"
            model += lambda_var[p_][t_] <= x[p_][t_], f"Start_InSquad_LinkB_{p_}_{t_}"
            model += lambda_var[p_][t_] <= 1 - r[t_], f"Start_InSquad_LinkC_{p_}_{t_}"
        model += pulp.lpSum(f[p_][t_] for p_ in p) + pulp.lpSum(is_tc[p_][t_] for p_ in p) == 1, f"Captain_Or_TC_Unique_{t_}"
        model += pulp.lpSum(h[p_][t_] for p_ in p) == 1, f"ViceCaptain_Unique_{t_}"
        for p_ in p:
            model += f[p_][t_] + is_tc[p_][t_] + h[p_][t_] <= y[p_][t_], f"Captaincy_In_Lineup_{p_}_{t_}"
            model += f[p_][t_] + h[p_][t_] <= 1, f"Cap_Not_Vice_{p_}_{t_}"
        if g:
            for p_ngk in P_not_gk:
                is_sub = pulp.lpSum(g[p_ngk][t_][l_] for l_ in l)
                model += y[p_ngk][t_] + is_sub <= x_freehit[p_ngk][t_] + lambda_var[p_ngk][t_], f"Sub_If_Benched_A_{p_ngk}_{t_}"
                model += is_sub <= 1 - y[p_ngk][t_], f"Sub_Only_If_Benched_B_{p_ngk}_{t_}"
            for l_ in l: model += pulp.lpSum(g[p_ngk][t_][l_] for p_ngk in P_not_gk) <= 1, f"Sub_Priority_Unique_{t_}_{l_}"
        for p_ in p: model += e[p_][t_] + u[p_][t_] <= 1, f"Transfer_In_Out_Limit_{p_}_{t_}"
        model += pulp.lpSum(e[p_][t_] for p_ in p) == pulp.lpSum(u[p_][t_] for p_ in p), f"Transfer_Balance_{t_}"


    # --- Linking Constraints & Evolution ---
    print("  Adding Linking and Evolution constraints...")
    t1_sub = t_sub[0] # First GW of this sub-problem
    model += q[t1_sub] == previous_ft, f"FT_Link_{t1_sub}"
    if current_gw == START_GAMEWEEK:
        model += v[t1_sub] + pulp.lpSum(value_sub.loc[p_, t1_sub] * x[p_][t1_sub] for p_ in p) <= BS, f"Budget_Initial_{t1_sub}"
        model += pulp.lpSum(e[p_][t1_sub] for p_ in p) == 0, f"No_Transfers_In_GW1"
        model += pulp.lpSum(u[p_][t1_sub] for p_ in p) == 0, f"No_Transfers_Out_GW1"
    else:
        sales_value_t1 = pulp.lpSum(value_sub.loc[p_, t1_sub] * u[p_][t1_sub] for p_ in p)
        purchase_cost_t1 = pulp.lpSum(value_sub.loc[p_, t1_sub] * e[p_][t1_sub] for p_ in p)
        model += v[t1_sub] == previous_budget + sales_value_t1 - purchase_cost_t1, f"Budget_Link_{t1_sub}"
        for p_ in p: model += x[p_][t1_sub] == previous_squad_dict.get(p_, 0) - u[p_][t1_sub] + e[p_][t1_sub], f"Squad_Link_{p_}_{t1_sub}"
    model += alpha[t1_sub] >= pulp.lpSum(e[p_][t1_sub] for p_ in p) - q[t1_sub], f"PenalizedTransfers_Calc_{t1_sub}"
    model += alpha[t1_sub] <= M_alpha * (1 - w[t1_sub]), f"PenalizedTransfers_WC_Override_{t1_sub}"
    model += alpha[t1_sub] <= M_alpha * (1 - r[t1_sub]), f"PenalizedTransfers_FH_Override_{t1_sub}"
    for gw_idx in range(n_T_sub - 1):
        t_curr_sub = t_sub[gw_idx + 1]; t_prev_sub = t_sub[gw_idx]
        sales_value = pulp.lpSum(value_sub.loc[p_, t_curr_sub] * u[p_][t_curr_sub] for p_ in p)
        purchase_cost = pulp.lpSum(value_sub.loc[p_, t_curr_sub] * e[p_][t_curr_sub] for p_ in p)
        model += v[t_curr_sub] == v[t_prev_sub] + sales_value - purchase_cost, f"Budget_Evolution_{t_curr_sub}"
        for p_ in p: model += x[p_][t_curr_sub] == x[p_][t_prev_sub] - u[p_][t_curr_sub] + e[p_][t_curr_sub], f"Squad_Continuity_{p_}_{t_curr_sub}"
        cost_fh_squad_t = pulp.lpSum(value_sub.loc[p_, t_curr_sub] * x_freehit[p_][t_curr_sub] for p_ in p)
        value_nonfh_squad_prev = pulp.lpSum(value_sub.loc[p_, t_prev_sub] * x[p_][t_prev_sub] for p_ in p)
        model += cost_fh_squad_t <= v[t_prev_sub] + value_nonfh_squad_prev + M_budget * (1 - r[t_curr_sub]), f"FH_Budget_Limit_Upper_{t_curr_sub}"
        model += pulp.lpSum(u[p_][t_curr_sub] for p_ in p) <= M_transfer * (1 - r[t_curr_sub]), f"FH_NoTransfersOut_{t_curr_sub}"
        model += pulp.lpSum(e[p_][t_curr_sub] for p_ in p) <= M_transfer * (1 - r[t_curr_sub]), f"FH_NoTransfersIn_{t_curr_sub}"
        model += alpha[t_curr_sub] >= pulp.lpSum(e[p_][t_curr_sub] for p_ in p) - q[t_curr_sub], f"PenalizedTransfers_Calc_{t_curr_sub}"
        model += alpha[t_curr_sub] <= M_alpha * (1 - w[t_curr_sub]), f"PenalizedTransfers_WC_Override_{t_curr_sub}"
        model += alpha[t_curr_sub] <= M_alpha * (1 - r[t_curr_sub]), f"PenalizedTransfers_FH_Override_{t_curr_sub}"
        ft_used_effectively_prev = pulp.lpSum(e[p_][t_prev_sub] for p_ in p) - alpha[t_prev_sub]
        model += ft_carried_over_nonneg[t_prev_sub] >= q[t_prev_sub] - ft_used_effectively_prev, f"FT_Carry_Calc_{t_prev_sub}"
        model += ft_carried_over_nonneg[t_prev_sub] <= Q_bar - Q_under_bar, f"FT_Carry_Cap_{t_prev_sub}"
        chip_active_prev = w[t_prev_sub] + r[t_prev_sub]
        q_normal_calc = ft_carried_over_nonneg[t_prev_sub] + Q_under_bar
        model += q[t_curr_sub] <= q_normal_calc + M_q * chip_active_prev, f"FT_Evo_Normal_Upper_{t_curr_sub}"
        model += q[t_curr_sub] >= q_normal_calc - M_q * chip_active_prev, f"FT_Evo_Normal_Lower_{t_curr_sub}"
        model += q[t_curr_sub] <= Q_under_bar + M_q * (1 - chip_active_prev), f"FT_Evo_Chip_Reset_Upper_{t_curr_sub}"
        model += q[t_curr_sub] >= Q_under_bar - M_q * (1 - chip_active_prev), f"FT_Evo_Chip_Reset_Lower_{t_curr_sub}"
        model += alpha[t_prev_sub] + M_alpha * q[t_curr_sub] <= M_alpha * Q_bar, f"Transfer_NoPaidIfMaxFT_{t_prev_sub}"

    print(f"Constraints added in {time.time() - cons_start:.2f}s")

    # 7. Solve Sub-Problem
    print(f"[{time.strftime('%H:%M:%S')}] Starting solve for GW {current_gw} (Solver Internal Limit: {SOLVER_TIME_LIMIT}s)...")
    print(f"[{time.strftime('%H:%M:%S')}] Solver output (msg=True) should appear below:")
    solve_start_time = time.time()
    solve_status = None; solver_exception = None
    try:
        print("Solver starting...")
        status = model.solve(solver_to_use) # Direct call
        print("Solver finished.")
    except Exception as e:
        solver_exception = e; print(f"Solver encountered an error: {e}"); status = pulp.LpStatusUndefined
    solve_time = time.time() - solve_start_time
    if solver_exception: print("\n--- Solver Error ---"); print(f"Error: {solver_exception}"); status = -1000
    else: status = status
    if status is None: print("\nERROR: Solver status not captured."); status = -1000
    print(f"\n--- Solve Complete for GW {current_gw} ---")
    print(f"Final Solver Status Code: {status}")
    status_str = pulp.LpStatus.get(status, 'Unknown Status'); print(f"Final Solver Status: {status_str}")
    print(f"Sub-problem Solve Time: {solve_time:.2f} seconds")
    time_limit_hit = SOLVER_TIME_LIMIT is not None and solve_time >= SOLVER_TIME_LIMIT * 0.98

    # 8. Extract Results and Update State
    objective_value_extracted = None; use_fallback = False
    try:
        if model.objective is not None: objective_value_extracted = model.objective.value()
    except AttributeError: pass
    solution_truly_optimal = (status == pulp.LpStatusOptimal)
    solution_acceptable_timeout = (status == pulp.LpStatusNotSolved and time_limit_hit and objective_value_extracted is not None)
    solution_acceptable = solution_truly_optimal or solution_acceptable_timeout
    if not solution_acceptable:
        print(f"!!! Solver failed/timed out without acceptable solution for GW {current_gw} (Status: {status_str}). Implementing FALLBACK. !!!")
        use_fallback = True
        try: lp_filename = f"failed_model_gw{current_gw}.lp"; model.writeLP(lp_filename); print(f"Model written to {lp_filename}.")
        except Exception as write_err: print(f"Could not write LP file: {write_err}")

    if not use_fallback:
        t1_sub = t_sub[0]
        try:
            print(f"Extracting results for GW {current_gw} (t={t1_sub})...")
            gw_results = {'gameweek': current_gw}
            def get_var_val(var, default=0.0):
                try: val = var.varValue; return val if val is not None else default
                except AttributeError: return default
            if current_gw == START_GAMEWEEK:
                 previous_squad_dict = {p_: 1 for p_ in p if get_var_val(x[p_][t1_sub]) > 0.9}
                 print(f"Initialized previous_squad_dict with {len(previous_squad_dict)} players for GW{current_gw+1}")
            gw_results['squad'] = sorted([p_ for p_ in p if get_var_val(x[p_][t1_sub]) > 0.9])
            gw_results['lineup'] = sorted([p_ for p_ in p if get_var_val(y[p_][t1_sub]) > 0.9])
            captain_id = None; tc_active_gw = False; tc_player_id = None
            potential_captains = [p_ for p_ in p if get_var_val(f[p_][t1_sub]) > 0.9]
            potential_tc = [p_ for p_ in p if get_var_val(is_tc[p_][t1_sub]) > 0.9]
            if potential_tc: captain_id = potential_tc[0]; tc_active_gw = True; tc_player_id = captain_id
            elif potential_captains: captain_id = potential_captains[0]
            gw_results['captain'] = [captain_id] if captain_id is not None else []
            gw_results['vice_captain'] = sorted([p_ for p_ in p if get_var_val(h[p_][t1_sub]) > 0.9])
            gw_results['transfers_in'] = sorted([p_ for p_ in p if get_var_val(e[p_][t1_sub]) > 0.9])
            gw_results['transfers_out'] = sorted([p_ for p_ in p if get_var_val(u[p_][t1_sub]) > 0.9])
            gw_results['budget_end'] = get_var_val(v[t1_sub], default=previous_budget)
            alpha_val = get_var_val(alpha[t1_sub]); gw_results['alpha'] = round(alpha_val)
            gw_results['q_start'] = previous_ft
            gw_results['objective_value'] = objective_value_extracted
            wc_active_gw = get_var_val(w[t1_sub]) > 0.9; bb_active_gw = get_var_val(b[t1_sub]) > 0.9; fh_active_gw = get_var_val(r[t1_sub]) > 0.9
            chip_name_display = None
            if wc_active_gw: chip_name_display = 'WC'
            if bb_active_gw: chip_name_display = 'BB'
            if fh_active_gw: chip_name_display = 'FH'
            if tc_active_gw and tc_player_id is not None: tc_player_name = player_name_map.get(tc_player_id, f"ID:{tc_player_id}"); chip_name_display = f'TC_{tc_player_name}'
            gw_results['chip_played'] = chip_name_display
            gw_results['budget_start'] = previous_budget

            # --- Calculate and Store Weekly Objective Contributions ---
            weekly_objectives_in_sub = {}
            for t_calc in t_sub:
                points_lineup_t = sum(get_var_val(y[p_][t_calc]) * points_sub.loc[p_, t_calc] for p_ in p)
                points_cap_t = sum(get_var_val(f[p_][t_calc]) * points_sub.loc[p_, t_calc] for p_ in p)
                points_vice_t = sum(get_var_val(h[p_][t_calc]) * epsilon * points_sub.loc[p_, t_calc] for p_ in p)
                points_tc_t = sum(get_var_val(is_tc[p_][t_calc]) * 2 * points_sub.loc[p_, t_calc] for p_ in p)
                points_subs_t = 0
                if g: points_subs_t = sum(kappa[l_] * get_var_val(g[p_ngk][t_calc][l_]) * points_sub.loc[p_ngk, t_calc] for p_ngk in P_not_gk for l_ in l)
                alpha_t_val = get_var_val(alpha[t_calc]); penalty_t = R_penalty * alpha_t_val
                weekly_obj_val = points_lineup_t + points_cap_t + points_vice_t + points_tc_t + points_subs_t - penalty_t
                weekly_objectives_in_sub[t_calc] = weekly_obj_val
                # Print immediately
                print(f"  GW {t_calc} Calculated Objective: {weekly_obj_val:.4f} (Penalty: {penalty_t:.1f})")
            # Store the dictionary in the main results for the implemented GW
            gw_results['weekly_sub_objectives'] = weekly_objectives_in_sub
            # Store only the implemented week's objective for easier access later
            gw_results['objective_this_gw'] = weekly_objectives_in_sub.get(t1_sub, None)

            master_results.append(gw_results) # Append results AFTER calculations

            # --- State Update Logic ---
            chip_played_this_gw = gw_results['chip_played']
            next_ft_value = previous_ft
            try:
                if chip_played_this_gw == 'WC' or chip_played_this_gw == 'FH': next_ft_value = Q_under_bar
                else:
                    q_current_val = gw_results['q_start']; alpha_current_val = gw_results['alpha']
                    e_current_sum = len(gw_results['transfers_in'])
                    ft_used_current_eff = max(0, e_current_sum - alpha_current_val)
                    ft_carry_current = max(0, q_current_val - ft_used_current_eff)
                    next_ft_value = min(Q_bar, math.floor(ft_carry_current + Q_under_bar))
            except Exception as ft_err: print(f"Warning: Error calculating next FT: {ft_err}. Using default: {next_ft_value}")
            if fh_active_gw:
                print(f"FH played in GW {current_gw}. State (Squad/Budget) for next GW reverts.")
                previous_budget = gw_results['budget_start']
            else:
                previous_squad_dict = {p_: 1 for p_ in gw_results['squad']}
                previous_budget = gw_results['budget_end']
            previous_ft = next_ft_value
            if chip_played_this_gw:
                 if chip_played_this_gw == 'WC':
                     if current_gw <= mid_season_split_gw and not used_chips_tracker['wc1']: used_chips_tracker['wc1'] = True; print(f"--- WC1 activated in GW {current_gw} ---")
                     elif current_gw > mid_season_split_gw and not used_chips_tracker['wc2']: used_chips_tracker['wc2'] = True; print(f"--- WC2 activated in GW {current_gw} ---")
                 elif chip_played_this_gw == 'BB' and not used_chips_tracker['bb']: used_chips_tracker['bb'] = True; print(f"--- BB activated in GW {current_gw} ---")
                 elif chip_played_this_gw == 'FH' and not used_chips_tracker['fh']: used_chips_tracker['fh'] = True; print(f"--- FH activated in GW {current_gw} ---")
                 elif chip_played_this_gw.startswith('TC_') and not used_chips_tracker['tc']: used_chips_tracker['tc'] = True; print(f"--- {chip_played_this_gw} activated ---")
            print(f"End of GW {current_gw}: Budget={previous_budget:.1f}, Next FT={previous_ft}")

        except Exception as extract_err:
            print(f"!!! Error extracting results for GW {current_gw}: {extract_err} !!!"); import traceback; traceback.print_exc()
            print("Stopping rolling horizon due to extraction error."); break

    else: # --- Handle Fallback Case ---
        print(f"Applying Fallback for GW {current_gw}: No transfers made.")
        next_ft_value = min(Q_bar, math.floor(previous_ft + Q_under_bar))
        gw_results = {'gameweek': current_gw, 'squad': sorted(list(previous_squad_dict.keys())), 'lineup': [], 'captain': [], 'vice_captain': [],
                      'transfers_in': [], 'transfers_out': [], 'budget_end': previous_budget, 'alpha': 0, 'q_start': previous_ft,
                      'objective_value': None, 'chip_played': 'FALLBACK_NO_TRANSFERS', 'budget_start': previous_budget,
                      'weekly_sub_objectives': {}, 'objective_this_gw': None } # Add placeholders
        master_results.append(gw_results)
        previous_ft = next_ft_value # State update: budget/squad same, FT updates
        print(f"End of GW {current_gw} (Fallback): Budget={previous_budget:.1f}, Next FT={previous_ft}")

    print(f"Total time for GW {current_gw} loop: {time.time() - loop_start_time:.2f}s"); print("-" * 50)

# --- Post-Loop: Process and Display Results ---
print("\n" + "="*20 + " Final Rolling Horizon Results " + "="*20)
if not master_results: print("No results were generated.")
else:
    results_df = pd.DataFrame(master_results)
    def map_ids_to_names(id_list):
        if not isinstance(id_list, (list, tuple, set)): return id_list
        if not id_list: return []
        return sorted([player_name_map.get(p_id, f"ID:{p_id}") for p_id in id_list])
    id_list_columns = ['squad', 'lineup', 'captain', 'vice_captain', 'transfers_in', 'transfers_out']
    print("\nConverting player IDs to names in results DataFrame...")
    results_df_named = results_df.copy()
    for col in id_list_columns:
        if col in results_df_named.columns: results_df_named[col] = results_df_named[col].apply(map_ids_to_names)
    print("ID to Name conversion complete.")
    for index, row in results_df_named.iterrows():
        gw = row['gameweek']; print(f"\n--- GW {gw} Summary ---")
        print(f"  Chip Played: {row['chip_played'] if row['chip_played'] else 'None'}")
        transfers_in_str = ', '.join(map(str, row['transfers_in'])); transfers_out_str = ', '.join(map(str, row['transfers_out']))
        print(f"  Transfers In ({len(row['transfers_in'])}): {transfers_in_str}")
        print(f"  Transfers Out ({len(row['transfers_out'])}): {transfers_out_str}")
        print(f"  FT Available (Start): {row['q_start']}")
        print(f"  Penalized Transfers (Hits): {row['alpha']}")
        cap_name = row['captain'][0] if row['captain'] else 'None'; vice_name = row['vice_captain'][0] if row['vice_captain'] else 'None'
        print(f"  Captain: {cap_name}"); print(f"  Vice-Captain: {vice_name}")
        print(f"  Budget End: {row['budget_end']:.1f}")
        # --- MODIFIED TO SHOW IMPLEMENTED GW OBJECTIVE ---
        obj_val_gw_str = f"{row['objective_this_gw']:.2f}" if row['objective_this_gw'] is not None else "N/A"
        print(f"  Objective Value (This GW's Contribution): {obj_val_gw_str}")
        obj_val_sub_str = f"{row['objective_value']:.2f}" if row['objective_value'] is not None else "N/A"
        print(f"  Objective Value (Full Sub-problem): {obj_val_sub_str}")
        # Optionally print the breakdown for the sub-horizon again
        # if 'weekly_sub_objectives' in row and row['weekly_sub_objectives']:
        #    print("  Sub-Horizon Weekly Objectives:")
        #    for sub_gw, sub_obj in row['weekly_sub_objectives'].items():
        #        print(f"    GW {sub_gw}: {sub_obj:.4f}")

    try:
        output_csv_name = "rolling_horizon_results_with_names.csv"
        results_df_to_save = results_df_named.copy() # Use named df for saving
        # Convert lists to strings for CSV saving
        for col in id_list_columns + ['weekly_sub_objectives']: # Add the new dict column if saving it
             if col in results_df_to_save.columns:
                 results_df_to_save[col] = results_df_to_save[col].astype(str) # Convert lists/dicts to string
        results_df_to_save.to_csv(output_csv_name, index=False); print(f"\nResults saved to {output_csv_name}")
    except Exception as save_e: print(f"\nERROR: Could not save results to CSV. Reason: {save_e}")


print("\n--- Script Finished ---")