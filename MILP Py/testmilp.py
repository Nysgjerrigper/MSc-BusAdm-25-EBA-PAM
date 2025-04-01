import pandas as pd
import pulp
import numpy as np
import time
import sys # Import sys to check Python version

# --- Input Parameters ---
# Set the maximum gameweek to include in the analysis
# Smaller number = faster testing, larger number = full analysis
MAX_GAMEWEEK = 38 # Adjust this value as needed (e.g., 38 for a full season)
CSV_FILE_PATH = "C:/Users/peram/Documents/test/Differensiert gw alle tre sesonger(22-24), heltall.csv"

# --- Check Python Version ---
# PuLP often works best with specific solver interactions depending on the version
print(f"--- Running on Python {sys.version} ---")
print(f"--- PuLP Version: {pulp.__version__} ---")


print("\n--- 0. Setup & Data Loading ---")
# Load Data using Pandas
try:
    allesesonger = pd.read_csv(CSV_FILE_PATH)
    print(f"CSV '{CSV_FILE_PATH}' loaded successfully.")
except FileNotFoundError:
    print(f"ERROR: CSV file '{CSV_FILE_PATH}' not found.")
    print("Please ensure the file is in the same directory as the script or provide the full path.")
    exit()
except Exception as e:
    print(f"ERROR: Failed to load CSV file. Reason: {e}")
    exit()

# Basic Data Inspection
print("Initial data shape:", allesesonger.shape)
print("Initial columns:", allesesonger.columns.tolist())
print("Sample of initial data:")
print(allesesonger.head())

# Filter for the desired gameweeks
if 'GW' not in allesesonger.columns:
    print("ERROR: 'GW' column not found in the CSV. Please check the column names.")
    exit()

# Ensure GW column is numeric
try:
    allesesonger['GW'] = pd.to_numeric(allesesonger['GW'])
except ValueError:
    print("ERROR: Could not convert 'GW' column to numeric. Check data for non-numeric values.")
    exit()

data_raw = allesesonger[allesesonger['GW'] <= MAX_GAMEWEEK].copy()
if data_raw.empty:
    print(f"ERROR: No data found for GW <= {MAX_GAMEWEEK}. Check MAX_GAMEWEEK or the CSV content.")
    exit()
print(f"Filtered data for GW <= {MAX_GAMEWEEK}. Filtered data shape: {data_raw.shape}")


# --- 1. Define Initial Sets and Perform Data Cleaning/Validation ---
print("\n--- 1. Data Cleaning & Set Definition ---")

# Check for essential columns
essential_input_cols = ['player_id', 'GW', 'name', 'position', 'team', 'total_points', 'value']
missing_cols = [col for col in essential_input_cols if col not in data_raw.columns]
if missing_cols:
    print(f"ERROR: Missing essential columns in the CSV: {missing_cols}")
    exit()

# Handle potential whitespace issues in categorical columns
data_raw['team'] = data_raw['team'].astype(str).str.strip()
data_raw['position'] = data_raw['position'].astype(str).str.strip()
data_raw['name'] = data_raw['name'].astype(str).str.strip()


# Define initial sets from RAW filtered data
T_setofgameweeks = sorted(data_raw['GW'].unique())
P_setofplayers_initial = sorted(data_raw['player_id'].unique())
C_setofteams_initial = sorted(data_raw['team'].dropna().unique())
L_substitution = list(range(1, 4)) # Priorities 1, 2, 3

n_T = len(T_setofgameweeks)
n_P_initial = len(P_setofplayers_initial)
n_C_initial = len(C_setofteams_initial)
n_L = len(L_substitution)

if n_T == 0 or n_P_initial == 0 or n_C_initial == 0:
    print("ERROR: Initial sets (Gameweeks, Players, or Teams) are empty after filtering.")
    print(f"Gameweeks found: {T_setofgameweeks}")
    print(f"Initial Players found: {n_P_initial}")
    print(f"Initial Teams found: {C_setofteams_initial}")
    exit()

print(f"Initial sets defined: {n_T} Gameweeks, {n_P_initial} Players, {n_C_initial} Teams.")

# --- Data Validation and Cleaning ---
print("Performing data validation and cleaning (filling missing values)...")

# Create all player-GW combinations for the initial players and selected gameweeks
player_gw_combos = pd.MultiIndex.from_product([P_setofplayers_initial, T_setofgameweeks],
                                              names=['player_id', 'GW'])
data_complete = pd.DataFrame(index=player_gw_combos).reset_index()

# Merge with original raw data
data_merged = pd.merge(data_complete, data_raw, on=['player_id', 'GW'], how='left', suffixes=('', '_discard'))
# Drop potentially duplicated columns from the merge
data_merged = data_merged[[col for col in data_merged.columns if not col.endswith('_discard')]]


# Fill missing info (name, position, team) using forward/backward fill per player
# Sort first to ensure fill direction is meaningful
data_merged.sort_values(by=['player_id', 'GW'], inplace=True)
essential_info_cols = ['name', 'position', 'team']
print("Filling missing player info (name, position, team)...")
for col in essential_info_cols:
     # Using transform with ffill().bfill() is generally robust
     # Group by player_id and fill within each player's timeline
     data_merged[col] = data_merged.groupby('player_id')[col].transform(lambda x: x.ffill().bfill())

# Fill missing points and value
print("Filling missing points with 0 and value with 5.0 (scaled to 50)...")
data_merged['total_points'].fillna(0, inplace=True)
data_merged['value'].fillna(50, inplace=True) # Assuming 5.0 default value * 10 if missing

# --- Filter out rows where essential info couldn't be filled or team is invalid ---
print("Filtering rows with missing essential info or invalid teams...")
initial_rows = data_merged.shape[0]
# Drop rows where name, position, or team is still NaN after filling
data_filtered = data_merged.dropna(subset=essential_info_cols).copy()
rows_after_dropna = data_filtered.shape[0]
print(f"  Removed {initial_rows - rows_after_dropna} rows due to missing essential info.")

# Ensure 'team' column is string type before filtering
data_filtered['team'] = data_filtered['team'].astype(str)
# Keep only rows where the team is one of the initially identified teams
rows_before_team_filter = data_filtered.shape[0]
data_filtered = data_filtered[data_filtered['team'].isin(C_setofteams_initial)]
rows_after_team_filter = data_filtered.shape[0]
print(f"  Removed {rows_before_team_filter - rows_after_team_filter} rows due to invalid teams.")

# --- Handle potential duplicates (take first entry per player/GW) ---
# Sorting was done earlier, ensures 'first' is consistent if duplicates exist
rows_before_dedup = data_filtered.shape[0]
data_cleaned = data_filtered.drop_duplicates(subset=['player_id', 'GW'], keep='first').copy()
rows_after_dedup = data_cleaned.shape[0]
print(f"  Removed {rows_before_dedup - rows_after_dedup} duplicate player-GW entries.")

if data_cleaned.empty:
    print("ERROR: No data remaining after cleaning process. Check input data quality and filtering logic.")
    exit()

print(f"Data cleaning finished. Final data shape: {data_cleaned.shape}")


# --- Define FINAL Sets based on CLEANED data ---
p = sorted(data_cleaned['player_id'].unique()) # FINAL player set 'p'
t = T_setofgameweeks                          # FINAL gameweek set 't'
l = L_substitution                            # FINAL substitution set 'l'
C_setofteams_final = sorted(data_cleaned['team'].unique()) # FINAL team set
n_P = len(p)
n_T = len(t)
n_C = len(C_setofteams_final)
n_L = len(l)

if n_P == 0 or n_C == 0:
    print("ERROR: Final player set or team set is empty after cleaning.")
    exit()

print(f"FINAL sets defined: {n_T} Gameweeks, {n_P} Players, {n_C} Teams.")

data = data_cleaned # Use the final cleaned data from now on

# --- Define Subsets using FINAL 'p' ---
print("Defining player subsets (position, team)...")
# Create maps using only players in the final set 'p'
# Use drop_duplicates based on player_id to get one row per player for mapping
final_player_info = data.drop_duplicates(subset=['player_id'], keep='first')
pos_map = pd.Series(final_player_info['position'].values, index=final_player_info['player_id'])
team_map = pd.Series(final_player_info['team'].values, index=final_player_info['player_id'])

# Define subsets using the FINAL 'p'
Pgk = [p_ for p_ in p if pos_map.get(p_) == "GK"]
Pdef = [p_ for p_ in p if pos_map.get(p_) == "DEF"]
Pmid = [p_ for p_ in p if pos_map.get(p_) == "MID"]
Pfwd = [p_ for p_ in p if pos_map.get(p_) == "FWD"]
P_not_gk = [p_ for p_ in p if p_ not in Pgk]
n_P_not_gk = len(P_not_gk)
print(f"  Positions: {len(Pgk)} GK, {len(Pdef)} DEF, {len(Pmid)} MID, {len(Pfwd)} FWD")

# Define P_c using FINAL 'p' and 'C_setofteams_final'
# Group the cleaned data by team and list the players (ensure players are in final set 'p')
P_c = data[data['player_id'].isin(p)].groupby('team')['player_id'].unique().apply(list).to_dict()
# Filter P_c for teams actually present in the final team set and have players
P_c = {team: players for team, players in P_c.items() if team in C_setofteams_final and players}
C_setofteams = sorted(list(P_c.keys())) # Update C_setofteams based on actual data in P_c
n_C = len(C_setofteams)
print(f"  Defined player lists for {n_C} teams.")

# Define GW subsets
if not t: # Check if t is empty
    print("ERROR: Gameweek set 't' is empty.")
    exit()
medianavgameweeks = np.median(t) if n_T > 0 else t[0] # Handle case of 1 GW
T_FH = [t_ for t_ in t if t_ <= medianavgameweeks]
T_SH = [t_ for t_ in t if t_ > medianavgameweeks]
print(f"  Gameweek halves: FH={T_FH}, SH={T_SH}")


# --- Define Parameters ---
print("Defining model parameters...")
R = 4     # Points penalty for transfers
MK = 2    # Required GKs in squad
MD = 5    # Required DEFs in squad
MM = 5    # Required MIDs in squad
MF = 3    # Required FWDs in squad
MC = 3    # Max players from one team in squad
E = 11    # Required players in starting lineup
EK = 1    # Required GKs in starting lineup
ED = 3    # Min required DEFs in starting lineup
EM = 2    # Min required MIDs in starting lineup (Thesis says 3? Using 2 as per Ch4 text desc.)
EF = 1    # Min required FWDs in starting lineup
BS = 1000.0 # Starting budget (scaled, 100.0m * 10)
phi = (MK + MD + MM + MF) - E # Number of substitutes on bench
phi_K = MK - EK             # Number of substitute GKs
Q_bar = 2                   # Max storable free transfers
Q_under_bar = 1             # Free transfers gained per week
epsilon = 0.1               # Vice-captain coefficient factor
# Kappa values for substitution priorities
kappa = {1: 0.01, 2: 0.005, 3: 0.001} # Using dict for clarity
beta = n_P + 1              # Big M for substitution constraints
alpha_bar = n_P + 1         # Big M for transfer constraints
print("Parameters defined.")


# --- Prepare Coefficient Data Structures (using Pandas pivot) ---
print("Preparing coefficient matrices (points, value)...")
try:
    # Pivot the cleaned data
    points_matrix_df = data.pivot(index='player_id', columns='GW', values='total_points')
    value_matrix_df = data.pivot(index='player_id', columns='GW', values='value')

    # Reindex to ensure they match the FINAL player/GW sets exactly
    # Fill missing points with 0, missing values with a default (e.g., 50)
    points_matrix_df = points_matrix_df.reindex(index=p, columns=t, fill_value=0.0)
    value_matrix_df = value_matrix_df.reindex(index=p, columns=t, fill_value=50.0) # Use float fill_value

    print("Coefficient matrices prepared successfully.")
    print("Points matrix shape:", points_matrix_df.shape)
    print("Value matrix shape:", value_matrix_df.shape)
except KeyError as e:
    print(f"ERROR: KeyError during pivot/reindex. Missing column: {e}")
    print("Check if 'total_points' or 'value' columns exist and have correct names.")
    exit()
except Exception as e:
    print(f"ERROR creating pivot tables or reindexing: {e}")
    print("This might happen if 'data_cleaned' still contains duplicates for player_id/GW.")
    print("Data sample before pivot:")
    print(data.head())
    duplicates = data[data.duplicated(subset=['player_id', 'GW'], keep=False)]
    if not duplicates.empty:
        print("\nDuplicate player_id/GW entries found before pivot:")
        print(duplicates)
    exit()


# --- 2. Define PuLP Model ---
print("\n--- 2. Defining PuLP Model ---")
model = pulp.LpProblem("FPL_Optimization", pulp.LpMaximize)


# --- 3. Define Variables ---
print("Defining PuLP variables...")
start_vars = time.time()
# Using LpVariable.dicts for easier indexing and creation
# Binary variables
x = pulp.LpVariable.dicts("Squad", (p, t), cat='Binary')
x_freehit = pulp.LpVariable.dicts("Squad_FH", (p, t), cat='Binary')
y = pulp.LpVariable.dicts("Lineup", (p, t), cat='Binary')
f = pulp.LpVariable.dicts("Captain", (p, t), cat='Binary')
h = pulp.LpVariable.dicts("ViceCaptain", (p, t), cat='Binary')
is_tc = pulp.LpVariable.dicts("TripleCaptainChipActive", (p, t), cat='Binary') # Renamed 'c' for clarity
# Substitution priority only defined for non-keepers
g = pulp.LpVariable.dicts("Substitution", (P_not_gk, t, l), cat='Binary') if P_not_gk and l else {} # Handle empty P_not_gk or l
u = pulp.LpVariable.dicts("TransferOut", (p, t), cat='Binary')
e = pulp.LpVariable.dicts("TransferIn", (p, t), cat='Binary')
w = pulp.LpVariable.dicts("WildcardChipActive", t, cat='Binary')
b = pulp.LpVariable.dicts("BenchBoostChipActive", t, cat='Binary')
r = pulp.LpVariable.dicts("FreeHitChipActive", t, cat='Binary')
lambda_var = pulp.LpVariable.dicts("Aux_LineupInSquad", (p, t), cat='Binary') # Renamed lambda

# Continuous and Integer variables
v = pulp.LpVariable.dicts("RemainingBudget", t, lowBound=0, cat='Continuous')
# Free transfers available *at the start* of gameweek t (or end of t-1)
q = pulp.LpVariable.dicts("FreeTransfersAvailable", t, lowBound=0, upBound=Q_bar, cat='Integer')
# Number of penalized transfers *made in* gameweek t
alpha = pulp.LpVariable.dicts("PenalizedTransfers", t, lowBound=0, cat='Integer')
print(f"Variables defined in {time.time() - start_vars:.2f} seconds.")


# --- 4. Define Objective Function ---
print("Defining objective function...")
start_obj = time.time()

# Deconstruct the objective for clarity and easier debugging
points_from_lineup = pulp.lpSum(points_matrix_df.loc[p_, t_] * y[p_][t_] for p_ in p for t_ in t)
points_from_captain = pulp.lpSum(points_matrix_df.loc[p_, t_] * f[p_][t_] for p_ in p for t_ in t)
points_from_vice = pulp.lpSum(epsilon * points_matrix_df.loc[p_, t_] * h[p_][t_] for p_ in p for t_ in t)
points_from_tc = pulp.lpSum(2 * points_matrix_df.loc[p_, t_] * is_tc[p_][t_] for p_ in p for t_ in t)
# Check if g exists before summing
points_from_subs = pulp.lpSum(kappa[l_] * points_matrix_df.loc[p_ngk][t_] * g[p_ngk][t_][l_]
                               for p_ngk in P_not_gk for t_ in t for l_ in l) if g else 0

# Transfer penalty: Apply only for gameweeks where transfers are possible (t>=2)
transfer_penalty = pulp.lpSum(R * alpha[t_] for t_ in t if t_ > min(t)) if n_T > 1 else 0

objective = (points_from_lineup +
             points_from_captain +
             points_from_vice +
             points_from_tc +
             points_from_subs -
             transfer_penalty)

model += objective, "Total_Expected_Points"
print(f"Objective function defined in {time.time() - start_obj:.2f} seconds.")


# --- 5. Define Constraints ---
print("Adding constraints...")
start_cons = time.time()

# -- Gamechips (4.2 - 4.7) --
print("  Adding Gamechip constraints (4.2-4.7)...")
if T_FH: model += pulp.lpSum(w[t_] for t_ in T_FH) <= 1, "WC_Limit_FH"
if T_SH: model += pulp.lpSum(w[t_] for t_ in T_SH) <= 1, "WC_Limit_SH"
model += pulp.lpSum(is_tc[p_][t_] for p_ in p for t_ in t) <= 1, "TC_Limit_Total" # Total uses over horizon
model += pulp.lpSum(b[t_] for t_ in t) <= 1, "BB_Limit_Total"
model += pulp.lpSum(r[t_] for t_ in t) <= 1, "FH_Limit_Total"
# Constraint 4.7: Only one chip per gameweek
for t_ in t:
    model += w[t_] + pulp.lpSum(is_tc[p_][t_] for p_ in p) + b[t_] + r[t_] <= 1, f"GC_Per_Week_{t_}"

# -- Selected Squad (4.8 - 4.12) --
print("  Adding Selected Squad constraints (4.8-4.12)...")
for t_ in t:
    if Pgk: model += pulp.lpSum(x[p_gk][t_] for p_gk in Pgk) == MK, f"Squad_GK_{t_}"
    if Pdef: model += pulp.lpSum(x[p_def][t_] for p_def in Pdef) == MD, f"Squad_DEF_{t_}"
    if Pmid: model += pulp.lpSum(x[p_mid][t_] for p_mid in Pmid) == MM, f"Squad_MID_{t_}"
    if Pfwd: model += pulp.lpSum(x[p_fwd][t_] for p_fwd in Pfwd) == MF, f"Squad_FWD_{t_}"
    for c_team in C_setofteams:
        players_in_team = P_c.get(c_team, []) # Get player list for the team
        if players_in_team: # Only add constraint if the team has players in our final set
             model += pulp.lpSum(x[p_tm][t_] for p_tm in players_in_team) <= MC, f"Squad_TeamLimit_{c_team}_{t_}"

# -- Free Hit Squad (4.13 - 4.17) --
# These define the squad *only* when the Free Hit chip (r) is active
print("  Adding Free Hit Squad constraints (4.13-4.17)...")
for t_ in t:
    # Link x_freehit to r[t_] using Big-M (MK, MD etc. act as M)
    if Pgk: model += pulp.lpSum(x_freehit[p_gk][t_] for p_gk in Pgk) == MK * r[t_], f"FH_Squad_GK_{t_}"
    if Pdef: model += pulp.lpSum(x_freehit[p_def][t_] for p_def in Pdef) == MD * r[t_], f"FH_Squad_DEF_{t_}"
    if Pmid: model += pulp.lpSum(x_freehit[p_mid][t_] for p_mid in Pmid) == MM * r[t_], f"FH_Squad_MID_{t_}"
    if Pfwd: model += pulp.lpSum(x_freehit[p_fwd][t_] for p_fwd in Pfwd) == MF * r[t_], f"FH_Squad_FWD_{t_}"
    # Ensure x_freehit sums to 15 only when r=1
    model += pulp.lpSum(x_freehit[p_][t_] for p_ in p) == (MK + MD + MM + MF) * r[t_], f"FH_Squad_TotalSize_{t_}"
    for c_team in C_setofteams:
        players_in_team = P_c.get(c_team, [])
        if players_in_team:
             # This formulation directly links the sum to r[t_]
             model += pulp.lpSum(x_freehit[p_tm][t_] for p_tm in players_in_team) <= MC * r[t_], f"FH_Squad_TeamLimit_{c_team}_{t_}"

# -- Starting Line-up (4.18 - 4.26) --
print("  Adding Starting Line-up constraints (4.18-4.26)...")
for t_ in t:
    # 4.18: Total lineup size (accounts for Bench Boost)
    model += pulp.lpSum(y[p_][t_] for p_ in p) == E + phi * b[t_], f"Start_Size_{t_}"
    # 4.19: Goalkeeper in lineup (accounts for Bench Boost GK sub)
    if Pgk: model += pulp.lpSum(y[p_gk][t_] for p_gk in Pgk) == EK + phi_K * b[t_], f"Start_GK_{t_}"
    # 4.20-4.22: Minimum players per position
    if Pdef: model += pulp.lpSum(y[p_def][t_] for p_def in Pdef) >= ED, f"Start_MinDEF_{t_}"
    if Pmid: model += pulp.lpSum(y[p_mid][t_] for p_mid in Pmid) >= EM, f"Start_MinMID_{t_}" # Thesis Table says EM=3?
    if Pfwd: model += pulp.lpSum(y[p_fwd][t_] for p_fwd in Pfwd) >= EF, f"Start_MinFWD_{t_}"

    # 4.24-4.26: Lineup player must be in the active squad (handles Free Hit)
    # y[p,t] <= x_freehit[p,t] + lambda_var[p,t]
    # lambda_var[p,t] <= x[p,t]
    # lambda_var[p,t] <= 1 - r[t]
    # This ensures y <= x_freehit if r=1, and y <= x if r=0
    for p_ in p:
        model += y[p_][t_] <= x_freehit[p_][t_] + lambda_var[p_][t_], f"Start_InSquad_LinkA_{p_}_{t_}"
        model += lambda_var[p_][t_] <= x[p_][t_], f"Start_InSquad_LinkB_{p_}_{t_}"
        model += lambda_var[p_][t_] <= 1 - r[t_], f"Start_InSquad_LinkC_{p_}_{t_}"

# -- Captain/Vice (4.27 - 4.29) --
print("  Adding Captain/Vice constraints (4.27-4.29)...")
for t_ in t:
    # 4.27: Exactly one captain OR triple captain selected
    model += pulp.lpSum(f[p_][t_] for p_ in p) + pulp.lpSum(is_tc[p_][t_] for p_ in p) == 1, f"Captain_Or_TC_Unique_{t_}"
    # 4.28: Exactly one vice-captain selected
    model += pulp.lpSum(h[p_][t_] for p_ in p) == 1, f"ViceCaptain_Unique_{t_}"
    # 4.29: Captain/TC/Vice must be in the starting lineup
    for p_ in p:
        model += f[p_][t_] + is_tc[p_][t_] + h[p_][t_] <= y[p_][t_], f"Captaincy_In_Lineup_{p_}_{t_}"

# -- Substitution (4.30 - 4.32) --
print("  Adding Substitution constraints (4.30-4.32)...")
if P_not_gk and l and g: # Check if lists/dict are not empty
    for t_ in t:
        # 4.30 & 4.31 combined: Substitute (g=1) only if player is in squad (x or x_fh) but not starting (y=0)
        # We can formulate this as: y[p] + sum(g[p,l]) <= "in_active_squad"
        # where "in_active_squad" = x_freehit[p] if r=1, and x[p] if r=0.
        # Let's use the linearized form of "in_active_squad" which is x_freehit + lambda_var
        # So, y[p] + sum(g[p,l]) <= x_freehit[p] + lambda_var[p] (this implies <= x if r=0, <= x_fh if r=1)
        for p_ngk in P_not_gk:
            model += y[p_ngk][t_] + pulp.lpSum(g[p_ngk][t_][l_] for l_ in l) <= \
                     x_freehit[p_ngk][t_] + lambda_var[p_ngk][t_], f"Sub_If_Benched_{p_ngk}_{t_}"

        # 4.32: Max one player per substitution priority level
        for l_ in l:
            model += pulp.lpSum(g[p_ngk][t_][l_] for p_ngk in P_not_gk) <= 1, f"Sub_Priority_Unique_{t_}_{l_}"

# -- Budget (4.33 - 4.39) --
print("  Adding Budget constraints (4.33-4.39)...")
# 4.33: Initial Budget constraint for the first gameweek
t1 = min(t)
model += v[t1] + pulp.lpSum(value_matrix_df.loc[p_, t1] * x[p_][t1] for p_ in p) == BS, "Budget_Initial"

# 4.36: Cannot transfer a player in and out in the same gameweek
for t_ in t:
    for p_ in p:
        model += e[p_][t_] + u[p_][t_] <= 1, f"Transfer_In_Out_Limit_{p_}_{t_}"

# Constraints linking gameweeks (t >= 2)
if n_T > 1:
    for gw_idx in range(1, n_T): # Iterate from the second gameweek (index 1)
        t_curr = t[gw_idx]   # Current gameweek (t in thesis notation)
        t_prev = t[gw_idx-1] # Previous gameweek (t-1 in thesis notation)

        # 4.34: Budget Evolution (v[t] = v[t-1] + Sales - Purchases)
        # Assuming CS=CB=value_matrix_df.loc[p, t_curr] for simplicity as per thesis
        sales_value = pulp.lpSum(value_matrix_df.loc[p_, t_curr] * u[p_][t_curr] for p_ in p)
        purchase_cost = pulp.lpSum(value_matrix_df.loc[p_, t_curr] * e[p_][t_curr] for p_ in p)
        model += v[t_curr] == v[t_prev] + sales_value - purchase_cost, f"Budget_Evolution_{t_curr}"

        # 4.35: Squad Continuity (x[t] = x[t-1] - u[t] + e[t])
        for p_ in p:
            model += x[p_][t_curr] == x[p_][t_prev] - u[p_][t_curr] + e[p_][t_curr], f"Squad_Continuity_{p_}_{t_curr}"

        # 4.37: Free Hit Budget Limit (Cost of FH squad <= Budget[t-1] + Value of squad[t-1])
        # cost_fh_squad = lpSum(value_matrix_df.loc[p_,t_curr] * x_freehit[p_][t_curr] for p_ in p)
        # value_prev_squad = lpSum(value_matrix_df.loc[p_,t_prev] * x[p_][t_prev] for p_ in p) # Use t_prev value? Thesis formula uses t value? Lets use t_curr for FH cost and t_prev for prev squad value.
        # model += cost_fh_squad <= v[t_prev] + value_prev_squad # This seems complex, maybe simplify?
        # The thesis formula 4.37 seems to compare value(t-1) squad + budget(t-1) vs value(t) freehit squad. Let's use that.
        value_prev_squad_t_prev_val = pulp.lpSum(value_matrix_df.loc[p_,t_prev] * x[p_][t_prev] for p_ in p)
        cost_fh_squad_t_curr_val = pulp.lpSum(value_matrix_df.loc[p_,t_curr] * x_freehit[p_][t_curr] for p_ in p)
        # Constraint: cost_fh_squad <= v[t-1] + value_prev_squad(t-1 value)
        # This constraint only needs to be active when r[t_curr]=1. Use Big-M.
        # cost_fh_squad - M*r[t] <= v[t-1] + value_prev_squad
        # Choose M large enough, BS should suffice.
        model += cost_fh_squad_t_curr_val - (BS + 15*150)*r[t_curr] <= v[t_prev] + value_prev_squad_t_prev_val, f"FH_Budget_Limit_{t_curr}"


        # 4.38 / 4.39: Free Hit Transfer Restriction (No normal transfers in FH week)
        # sum(u[p,t]) <= E * (1 - r[t])
        # sum(e[p,t]) <= E * (1 - r[t])  (Using E=11 as a loose upper bound on transfers)
        model += pulp.lpSum(u[p_][t_curr] for p_ in p) <= (MK+MD+MM+MF) * (1 - r[t_curr]), f"FH_NoTransfersOut_{t_curr}"
        model += pulp.lpSum(e[p_][t_curr] for p_ in p) <= (MK+MD+MM+MF) * (1 - r[t_curr]), f"FH_NoTransfersIn_{t_curr}"

# -- Transfers (4.40 - 4.44 + alpha calculation) --
print("  Adding Transfer constraints (4.40-4.44)...")
if n_T > 1:
    t2 = t[1] # Second gameweek (index 1)
    # 4.40: Initial free transfers for GW2 (start of GW2)
    model += q[t2] == Q_under_bar, "Transfer_Initial_Q_GW2"

    # Iterate from the second gameweek (t=2) onwards for transfer logic
    for gw_idx in range(1, n_T):
        t_curr = t[gw_idx]   # Gameweek t where transfers *are made*
        t_prev = t[gw_idx-1] # Previous gameweek

        # Calculate penalized transfers (alpha) for gameweek t_curr
        # alpha[t] >= sum(e[p,t]) - q[t]  (where q[t] is FT available *before* transfers)
        model += alpha[t_curr] >= pulp.lpSum(e[p_][t_curr] for p_ in p) - q[t_curr], f"PenalizedTransfers_Calculation_{t_curr}"
        # Ensure alpha is zero if Wildcard is active in t_curr
        model += alpha[t_curr] <= alpha_bar * (1 - w[t_curr]), f"PenalizedTransfers_WC_Override_{t_curr}"
        # Ensure alpha is non-negative (already done by lowBound=0)

        # Transfer evolution logic only needed if there's a *next* gameweek
        if gw_idx < n_T - 1:
            t_next = t[gw_idx+1] # Gameweek t+1

            # 4.41: Free Transfer Evolution for q[t+1]
            # q[t+1] = min(Q_bar, q[t] - sum(e[t]) + alpha[t] + Q_under_bar) if WC=0 and FH=0
            # Simplified: q[t+1] <= q[t] - sum(e[t]) + alpha[t] + Q_under_bar (upper bound by Q_bar is variable definition)
            # This needs to handle WC and FH cases where q[t+1] resets to Q_under_bar
            # Let's use the thesis approach with Big-M term Ewt which simplifies to alpha_bar*w[t]
            # Original: Ewt + qt - sum(ept) + Q_ + alpha_t >= qt+1
            # Here Ewt ensures constraint is loose if w=1.
            # Let's rewrite based on logic:
            # FT_used = sum(e[t]) - alpha[t]
            # FT_carryover = q[t] - FT_used
            # q[t+1] = min(Q_bar, FT_carryover + Q_under_bar)
            # This is hard to linearize directly with min/max. Let's try the thesis eq 4.41/4.44 logic again.

            # Eq 4.41: Ewt + qt - sum(ept) + Q_ + alphat >= qt+1 (Seems incorrect, alpha should reduce needed FT)
            # Let's try: qt+1 <= Q_bar (from variable bound)
            #            qt+1 <= q[t] - (sum(e[p,t]) - alpha[t]) + Q_under_bar + Q_bar * w[t_curr] + Q_bar * r[t_curr] # Carryover + new FT, relaxed if WC/FH
            #            qt+1 >= Q_under_bar # Minimum FT after WC/FH? Yes.
            # Eq 4.44 implies q[t+1] is forced to Q_under_bar after WC/FH.
            # q[t+1] <= Q_ + (Q_bar - Q_)*w[t] + (Q_bar - Q_)*r[t]
            # q[t+1] >= Q_ - (Q_bar - Q_)*w[t] - (Q_bar - Q_)*r[t] (approx)

            # Let's try simpler:
            # 1. Calculate free transfers used: ft_used = max(0, sum(e) - alpha) -- non-linear
            # 2. Calculate transfers paid for: alpha = max(0, sum(e) - q)
            # 3. Next week's FT: q_next = min(Q_bar, q_current - sum(e) + alpha + Q_under_bar) if not WC/FH else Q_under_bar

            # Using the thesis linearized constraints 4.41-4.44:
            # 4.41 (Reformulated to be <=, assuming Ewt = alpha_bar*w[t]):
            # q[t+1] <= q[t] - pulp.lpSum(e[p_][t_curr] for p_ in p) + Q_under_bar + alpha[t_curr] + alpha_bar * w[t_curr] + alpha_bar * r[t_curr] # Seems complex, might be wrong interpretation
            # Let's stick to 4.43 and 4.44 as primary drivers of q[t+1] logic combined with bounds.

            # 4.43: Ensure q[t+1] is at least Q_under_bar (the new FT received)
            # This might be too strict if you could theoretically carry over 0 and not make transfers.
            # Let's assume q is transfers available *before* making transfers for t+1.
            # q[t+1] is determined at the *end* of period t.
            ft_carried_over = q[t_curr] - (pulp.lpSum(e[p_][t_curr] for p_ in p) - alpha[t_curr]) # Can be negative if paid transfers > available FT, needs lower bound 0?
            ft_carried_over_nonneg = pulp.LpVariable(f"FT_Carry_{t_curr}", lowBound=0)
            model += ft_carried_over_nonneg >= ft_carried_over, f"FT_Carry_NonNeg_{t_curr}"

            # q[t+1] = min(Q_bar, ft_carried_over_nonneg + Q_under_bar)
            # Linearize min: q[t+1] <= Q_bar (bound), q[t+1] <= ft_carried_over_nonneg + Q_under_bar
            model += q[t_next] <= ft_carried_over_nonneg + Q_under_bar, f"Transfer_Evo_Max_{t_next}"

            # 4.44 (Simplified): Force q[t+1] = Q_under_bar if WC or FH used in t_curr
            # q[t+1] <= Q_under_bar + Q_bar * (1 - w[t_curr]) + Q_bar * (1 - r[t_curr]) # If w=1 or r=1, forces q[t+1] <= Q_
            # q[t+1] >= Q_under_bar - Q_bar * w[t_curr] - Q_bar * r[t_curr] # If w=1 or r=1, allows q[t+1] < Q_
            model += q[t_next] <= Q_under_bar + Q_bar * (1 - w[t_curr]), f"Transfer_Reset_WC_Upper_{t_next}"
            model += q[t_next] >= Q_under_bar * w[t_curr] , f"Transfer_Reset_WC_Lower_{t_next}" # If w=1, q_next >= Q_
            model += q[t_next] <= Q_under_bar + Q_bar * (1 - r[t_curr]), f"Transfer_Reset_FH_Upper_{t_next}"
            model += q[t_next] >= Q_under_bar * r[t_curr] , f"Transfer_Reset_FH_Lower_{t_next}" # If r=1, q_next >= Q_

            # 4.42: If q[t+1] = Q_bar (max FT), then alpha[t] must be 0 (no paid transfers made)
            # alpha[t] <= M * (1 - "is_q_max") where "is_q_max" = 1 if q[t+1]=Q_bar
            # Linearize: alpha[t] + alpha_bar * q[t+1] <= alpha_bar * Q_bar (From thesis)
            # This ensures if q[t+1] is Q_bar (or close to it), alpha[t] is forced towards 0.
            model += alpha[t_curr] + alpha_bar * q[t_next] <= alpha_bar * Q_bar, f"Transfer_NoPaidIfMaxFT_{t_curr}"


print(f"Constraints added in {time.time() - start_cons:.2f} seconds.")

# --- 6. Solve ---
print("\n--- 6. Solving Model ---")
print(f"Number of variables: {model.numVariables()}")
print(f"Number of constraints: {model.numConstraints()}")
start_solve = time.time()

# --- Solver Selection ---
# Default is CBC. Other options: pulp.GLPK_CMD(), pulp.CPLEX_CMD(), pulp.GUROBI_CMD()
# Example: solver = pulp.GUROBI_CMD(msg=True, timeLimit=600) # Gurobi with log and 10 min limit
# You might need to install solvers and ensure PuLP can find them.
solver = None # Use default CBC

# Determine the number of cores to use (e.g., use all available, or a specific number)
import os
try:
    # os.cpu_count() usually gives the total number of logical cores
    num_cores = os.cpu_count()
    print(f"Detected {num_cores} logical CPU cores.")
    # You might want to use slightly fewer than the total to leave resources for the OS
    threads_to_use = max(1, num_cores - 1 if num_cores else 1) # Use n-1 cores, but at least 1
    print(f"Setting CBC solver to use {threads_to_use} threads.")
except NotImplementedError:
    print("Could not detect number of CPU cores automatically. Setting threads to 4.")
    threads_to_use = 4 # Default to 4 if detection fails

# Specify the solver with the threads option
solver = pulp.PULP_CBC_CMD(threads=threads_to_use, msg=True, timeLimit=None) # msg=True for solver output, timeLimit=None for no limit

try:
    status = model.solve(solver) # Pass the configured solver object
    solve_time = time.time() - start_solve
    print(f"\n--- Solve Complete ---")
    print(f"Status: {pulp.LpStatus[status]}")
    print(f"Solve Time: {solve_time:.2f} seconds")

except pulp.PulpSolverError as pse:
    print(f"ERROR: PuLP Solver Error. Could not find or execute the solver.")
    print(f"Details: {pse}")
    print("Ensure the solver (like CBC, GLPK) is installed and accessible in your system's PATH.")
    exit()
except Exception as e:
    print(f"ERROR: An unexpected error occurred during solving.")
    print(f"Details: {e}")
    exit()

# --- 7. Extract and Display Results ---
print("\n--- 7. Results ---")
if status == pulp.LpStatusOptimal or status == pulp.LpStatusNotSolved : # Also show results if stopped by timelimit etc.
    obj_value = pulp.value(model.objective)
    print(f"Objective value: {obj_value:.4f}")

    if abs(obj_value) < 1e-6 and status == pulp.LpStatusOptimal:
         print("WARNING: Objective is zero or near-zero. Model might be overly constrained or parameters might be zero.")

    # --- Create Player Name Lookup ---
    player_name_map = final_player_info.set_index('player_id')['name'].to_dict()

    # --- Function to get names and details for selected players ---
    def get_selected_players_with_value(var_dict, gw, player_map, value_df):
        selected_ids = [p_ for p_ in var_dict if p_ in player_map and t_ in var_dict[p_] and var_dict[p_][gw].varValue > 0.99]
        names = [player_map.get(p_id, f"Unknown ID: {p_id}") for p_id in selected_ids]
        # Get value for the specific gameweek, handle potential KeyError if player/gw combo missing in value_df
        values = [value_df.loc[p_id, gw] if p_id in value_df.index else 'N/A' for p_id in selected_ids]
        return pd.DataFrame({'player_id': selected_ids, 'name': names, f'value_gw{gw}': values}).sort_values(by='player_id').reset_index(drop=True) if selected_ids else pd.DataFrame(columns=['player_id', 'name', f'value_gw{gw}'])


    # --- Display Key Decisions for Each Gameweek ---
    # Store previous budget for verification
    prev_budget = {}
    prev_budget[min(t)] = pulp.value(v[min(t)]) # Store initial remaining budget

    for t_ in t:
        print(f"\n" + "="*20 + f" Gameweek {t_} Decisions " + "="*20) # Separator

        # Determine active chip
        is_fh_active = r[t_].varValue > 0.99
        is_bb_active = b[t_].varValue > 0.99
        is_wc_active = w[t_].varValue > 0.99
        tc_active_player_df = get_selected_players_with_value(is_tc, t_, player_name_map, value_matrix_df) # Check TC chip specifically
        is_tc_active = not tc_active_player_df.empty

        # Get lineup first (always relevant)
        lineup_df = get_selected_players_with_value(y, t_, player_name_map, value_matrix_df)

        # Print Squad based on active chip
        if is_fh_active:
            print(f"!!! FREE HIT ACTIVE (GW{t_}) !!!")
            active_squad_df = get_selected_players_with_value(x_freehit, t_, player_name_map, value_matrix_df)
            print(f"\nFree Hit Squad (Temporary for GW{t_}): {len(active_squad_df)} players")
            if not active_squad_df.empty: print(active_squad_df.to_string(index=False))

            # Also show the underlying persistent squad for clarity
            persistent_squad_df = get_selected_players_with_value(x, t_, player_name_map, value_matrix_df)
            print(f"\nPersistent Squad (GW{t_} - Underlying/Carries Over): {len(persistent_squad_df)} players")
            if not persistent_squad_df.empty: print(persistent_squad_df.to_string(index=False))

        else: # Not Free Hit (could be WC, BB, TC, or normal)
            active_squad_df = get_selected_players_with_value(x, t_, player_name_map, value_matrix_df)
            print(f"\nSquad (GW{t_}): {len(active_squad_df)} players")
            if not active_squad_df.empty: print(active_squad_df.to_string(index=False))

        # Print Lineup
        print(f"\nStarting Lineup (GW{t_}): {len(lineup_df)} players")
        if is_bb_active:
             print("  (Bench Boost Active - All squad players start)")
        elif is_fh_active:
             print("  (Selected from Free Hit Squad)")
        if not lineup_df.empty: print(lineup_df.to_string(index=False))


        # Print Captaincy
        captain_df = get_selected_players_with_value(f, t_, player_name_map, value_matrix_df)
        if not captain_df.empty: print(f"Captain: {captain_df['name'].iloc[0]} (ID: {captain_df['player_id'].iloc[0]})")

        vc_df = get_selected_players_with_value(h, t_, player_name_map, value_matrix_df)
        if not vc_df.empty: print(f"Vice-Captain: {vc_df['name'].iloc[0]} (ID: {vc_df['player_id'].iloc[0]})")

        # Print Chip Status Summary
        if is_wc_active: print("WILDCARD ACTIVE")
        if is_bb_active: print("BENCH BOOST ACTIVE")
        # FH already printed above
        if is_tc_active: print(f"TRIPLE CAPTAIN ACTIVE on: {tc_active_player_df['name'].iloc[0]} (ID: {tc_active_player_df['player_id'].iloc[0]})")


        # --- Transfers and Budget Verification (for t >= 2) ---
        if t_ > min(t):
            print("\n--- Transfers & Budget ---")
            transfers_in_df = get_selected_players_with_value(e, t_, player_name_map, value_matrix_df)
            transfers_out_df = get_selected_players_with_value(u, t_, player_name_map, value_matrix_df)

            cost_in = transfers_in_df[f'value_gw{t_}'].sum() if not transfers_in_df.empty else 0
            value_out = transfers_out_df[f'value_gw{t_}'].sum() if not transfers_out_df.empty else 0
            net_transfer_value = value_out - cost_in

            t_prev = t[t.index(t_) - 1] # Get previous gameweek index
            budget_start = prev_budget[t_prev] # Budget at start of this GW (end of last)
            budget_end_calc = budget_start + net_transfer_value
            budget_end_model = pulp.value(v[t_])

            print(f"Budget (Start of GW{t_}): {budget_start:.1f}")

            if not transfers_in_df.empty:
                print(f"Transfers IN ({len(transfers_in_df)}): Cost = {cost_in:.1f}")
                print(transfers_in_df.to_string(index=False))
            else:
                 print(f"Transfers IN: 0")

            if not transfers_out_df.empty:
                print(f"Transfers OUT ({len(transfers_out_df)}): Value = {value_out:.1f}")
                print(transfers_out_df.to_string(index=False))
            else:
                 print(f"Transfers OUT: 0")

            print(f"Net Transfer Value: {net_transfer_value:.1f}")
            print(f"Calculated Budget (End of GW{t_}): {budget_start:.1f} + {net_transfer_value:.1f} = {budget_end_calc:.1f}")
            print(f"Model Budget (End of GW{t_}): {budget_end_model:.1f}")
            if abs(budget_end_calc - budget_end_model) > 0.01: # Check for discrepancy
                 print("!!! BUDGET DISCREPANCY DETECTED !!!")

            print(f"Free Transfers Available (Start of GW{t_}): {round(q[t_].varValue)}")
            print(f"Penalized Transfers Made: {round(alpha[t_].varValue)}")

        # Store budget for next iteration's check
        current_budget_end = pulp.value(v[t_])
        prev_budget[t_] = current_budget_end
        print(f"\nRemaining Budget (End of GW{t_}): {current_budget_end:.1f}") # Scaled value

elif status == pulp.LpStatusInfeasible:
    print("ERROR: Model is infeasible. Check constraints and data.")
elif status == pulp.LpStatusUnbounded:
    print("ERROR: Model is unbounded. Check objective function and constraints (especially bounds).")
else:
    print(f"Model did not solve to optimality. Status: {pulp.LpStatus[status]}")

print("\n--- Script Finished ---")