
#Oversatt med Gemini 2.5 Pro
import pandas as pd
import numpy as np
import tensorflow as tf
from tensorflow import keras
from tensorflow.keras import layers
from sklearn.preprocessing import StandardScaler
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LassoCV, Lasso
import warnings

# Optional: Suppress SettingWithCopyWarning from pandas if needed, though it's better to address the root cause if possible.
# warnings.filterwarnings('ignore', category=pd.errors.SettingWithCopyWarning)

print(tf.constant("Hello, TensorFlow!"))

# Load the data
url = "https://raw.githubusercontent.com/Nysgjerrigper/test/refs/heads/main/Datasett/Ekstra%20kolonner%2C%20stigende%20GW%2C%20alle%20tre%20sesonger(22-24)%2C%20heltall.csv"
df = pd.read_csv(url)
alternativsammensatt = df.copy() # Use .copy() to avoid modifying the original df later

#----------------------------
# 1. Extract GK Data
#----------------------------
gk = alternativsammensatt[alternativsammensatt['position'] == 'GK'].copy()

# Add a unique row_id if it doesn't exist, useful for joining later
# Reset index to ensure it's unique after filtering
gk = gk.reset_index(drop=True)
# Create a 'row_id' column from the new index
gk['row_id'] = gk.index

#----------------------------
# 2. Define Features, Target & Scale
#----------------------------
# Define numerical features (ensure these column names exist in your CSV)
numF_original = [
    "assists", "creativity", "minutes", "goals_conceded", "saves",
    "bonus", "bps", "expected_assists", "expected_goal_involvements",
    "expected_goals", "ict_index", "own_goals", "red_cards",
    "threat", "transfers_in", "transfers_out", "yellow_cards",
    "expected_goals_conceded", "penalties_saved", "value",
    "selected", "transfers_balance", "starts", "influence",
    "clean_sheets"
]
# Define categorical features
catF = ["player_id", "tID", "oID", "hID"]
# Define target variable
tar = "total_points"

# Check if all defined columns exist
missing_num = [col for col in numF_original if col not in gk.columns]
missing_cat = [col for col in catF if col not in gk.columns]
missing_tar = [tar] if tar not in gk.columns else []

if missing_num or missing_cat or missing_tar:
    print(f"Error: Missing columns in the dataframe:")
    if missing_num: print(f"  Numerical: {missing_num}")
    if missing_cat: print(f"  Categorical: {missing_cat}")
    if missing_tar: print(f"  Target: {missing_tar}")
    # Handle the error appropriately (e.g., exit or try to fix)
    exit()

# Fill potential NaNs before scaling (e.g., with 0 or mean/median if appropriate)
# Keras/TF often doesn't handle NaNs well in input data.
gk[numF_original] = gk[numF_original].fillna(0)
gk[catF] = gk[catF].fillna(0) # Assuming 0 is a sensible fill for IDs if missing
gk[tar] = gk[tar].fillna(0)

# --- Scaling ---
# Scaler for numerical features
num_scaler = StandardScaler()
gk[numF_original] = num_scaler.fit_transform(gk[numF_original])

# Scaler for the target variable
target_scaler = StandardScaler()
# Use double brackets [[]] to keep 'tar' as a DataFrame for the scaler
gk[[tar]] = target_scaler.fit_transform(gk[[tar]])

# Store mean and std dev (sigma) from the target scaler for later inversion
mu = target_scaler.mean_[0]
sigma = target_scaler.scale_[0]

print(f"Target Scaled: Mean={mu:.4f}, Sigma={sigma:.4f}")

#----------------------------
# 3. Build Rolling Windows
#----------------------------
# Define window size
ws = 6

# Function to create rolling windows for a single player
def create_player_windows(group, ws, num_features, cat_features, target_col):
    # Ensure the group is sorted by gameweek ('GW') if time order matters
    # group = group.sort_values('GW') # Uncomment if GW column exists and sorting is needed

    # Check if enough data points exist
    if len(group) <= ws:
        return None

    num_windows_list = []
    cat_current_list = []
    target_list = []
    row_id_list = []

    # Iterate through the dataframe starting from the point where a full window is available
    for i in range(ws, len(group)):
        # Indices for the feature window (ws points before the target)
        feature_indices = range(i - ws, i)
        # Index for the target value (the current point i)
        target_index = i

        # Extract numerical features for the window
        num_window = group.iloc[feature_indices][num_features].values
        # Extract *current* categorical features (at the target_index)
        cat_current = group.iloc[target_index][cat_features].values
        # Extract the target value
        target_val = group.iloc[target_index][target_col]
        # Extract the row_id for the target row
        row_id_val = group.iloc[target_index]['row_id']

        num_windows_list.append(num_window)
        cat_current_list.append(cat_current)
        target_list.append(target_val)
        row_id_list.append(row_id_val)

    # Create a temporary DataFrame for this player's windows
    player_df = pd.DataFrame({
        'num_window': num_windows_list,
        'cat_current': cat_current_list,
        'target': target_list,
        'row_id': row_id_list
    })
    return player_df

# Apply the window creation function to each player group
# Filter groups that are too small *before* applying
processed_groups = gk.groupby('player_id').filter(lambda x: len(x) > ws)
rolling_windows_df = processed_groups.groupby('player_id', group_keys=False).apply(
    lambda x: create_player_windows(x, ws, numF_original, catF, tar)
)

# Check if rolling_windows_df is empty
if rolling_windows_df is None or rolling_windows_df.empty:
     print("Error: No rolling windows could be generated. Check data and window size.")
     exit()

# Extract data into NumPy arrays
# Numerical data: shape [nSamp, ws, nNumF_original]
num_array_original = np.array(rolling_windows_df['num_window'].tolist())
# Categorical data (current values): shape [nSamp, nCatF]
cat_array = np.array(rolling_windows_df['cat_current'].tolist()).astype(np.int32) # Ensure integer type
# Target data: shape [nSamp, 1]
targets = np.array(rolling_windows_df['target'].tolist()).reshape(-1, 1)
# Row IDs: shape [nSamp,]
row_ids_array = np.array(rolling_windows_df['row_id'].tolist())

print(f"Generated {len(rolling_windows_df)} samples.")
print("Original Numerical Array Shape:", num_array_original.shape)
print("Categorical Array Shape:", cat_array.shape)
print("Targets Shape:", targets.shape)
print("Row IDs Shape:", row_ids_array.shape)


#----------------------------
# 4. Lasso regression for Feature Selection
#----------------------------

# Aggregate the numerical windows by taking the mean across the time steps (axis 1)
# Shape becomes [nSamp, nNumF_original]
x_agg = np.mean(num_array_original, axis=1)
y_lasso = targets.flatten() # LassoCV expects a 1D target array

# Handle potential NaNs/Infs in aggregated data (e.g., if a window contained only NaNs)
x_agg = np.nan_to_num(x_agg)

# Perform K-fold Cross-Validation to find the best alpha (lambda)
# cv=5 means 5-fold CV. Adjust if needed. n_jobs=-1 uses all CPU cores.
print("Running LassoCV for feature selection...")
lasso_cv = LassoCV(cv=5, random_state=123, n_jobs=-1, tol=0.001, max_iter=2000) # Increased tol/max_iter slightly
lasso_cv.fit(x_agg, y_lasso)
best_alpha = lasso_cv.alpha_
print(f"LassoCV finished. Best alpha (lambda): {best_alpha:.6f}")

# Fit the final Lasso model using the best alpha
lasso_final = Lasso(alpha=best_alpha)
lasso_final.fit(x_agg, y_lasso)

# Get the selected features
selected_features_mask = lasso_final.coef_ != 0
numF_selected = [f for f, selected in zip(numF_original, selected_features_mask) if selected]

# If no features are selected, Lasso might be too aggressive or data might lack signal.
# Handle this case - maybe exit, or use a default small set, or skip Lasso.
if not numF_selected:
    print("Warning: Lasso selected 0 features. Check data or Lasso alpha.")
    print("Proceeding with ALL original numerical features.")
    numF = numF_original # Fallback to all features
    num_array = num_array_original # Use the original array
else:
    print(f"Selected {len(numF_selected)} features via Lasso: {numF_selected}")
    numF = numF_selected # Update numF to only include selected features

    # Filter the original num_array to keep only selected features
    selected_indices = [numF_original.index(f) for f in numF]
    num_array = num_array_original[:, :, selected_indices]
    print("Filtered Numerical Array Shape:", num_array.shape)

#============================
# 5. Convert Rolling Windows to Arrays & Extract Targets (Already Done in Steps 3 & 4)
#============================
# We now have:
# num_array: The 3D NumPy array of numerical features (Lasso-selected), shape [nSamp, ws, len(numF)]
# cat_array: The 2D NumPy array of *current* categorical features, shape [nSamp, len(catF)]
# targets: The 2D NumPy array of scaled targets, shape [nSamp, 1]
# row_ids_array: The 1D NumPy array of row IDs, shape [nSamp,]
nSamp = num_array.shape[0]
nNumF = num_array.shape[2] # Number of selected numerical features
nCatF = cat_array.shape[1]

#============================
# 6. Split Data into Training and Validation Sets
#============================
np.random.seed(123)
tf.random.set_seed(123)

# Split all corresponding arrays together
Xnum_train, Xnum_val, \
cat_train, cat_val, \
y_train, y_val, \
row_ids_train, row_ids_val = train_test_split(
    num_array,         # Lasso-selected numerical windows
    cat_array,         # Current categorical features
    targets,           # Scaled targets
    row_ids_array,     # Corresponding row_ids
    test_size=0.2,     # 80% train, 20% validation
    random_state=123,  # For reproducibility
    shuffle=True       # Shuffle the data before splitting
)

# Split the cat_array into individual inputs required by the model
# Ensure the order matches the catF list: player_id, tID, oID, hID
cat_player_id_train = cat_train[:, 0]
cat_tID_train       = cat_train[:, 1]
cat_oID_train       = cat_train[:, 2]
cat_hID_train       = cat_train[:, 3]

cat_player_id_val = cat_val[:, 0]
cat_tID_val       = cat_val[:, 1]
cat_oID_val       = cat_val[:, 2]
cat_hID_val       = cat_val[:, 3]

print(f"Training samples: {Xnum_train.shape[0]}, Validation samples: {Xnum_val.shape[0]}")

#============================
# 7. Build the Model
#============================
# Define maximum values + 1 for embedding input_dim
# Calculate from the *original* unfiltered 'gk' data before scaling/windowing
gk_orig = alternativsammensatt[alternativsammensatt['position'] == 'GK'].copy()
# Add 1 because embedding layers expect input_dim = max_index + 1
num_players   = gk_orig['player_id'].max() + 1
num_teams     = gk_orig['tID'].max() + 1
num_opponents = gk_orig['oID'].max() + 1
# Assuming hID is binary (0 or 1), max value is 1, input_dim should be 2.
# If hID can have other values, adjust num_hID accordingly.
num_hID = 2 # Max value 1 + 1

emb_dim = 20  # Embedding dimension

# --- Input Layers ---
input_seq = keras.Input(shape=(ws, nNumF), name="input_seq", dtype='float32')
input_player_id = keras.Input(shape=(1,), name="input_player_id", dtype='int32')
input_tID = keras.Input(shape=(1,), name="input_tID", dtype='int32')
input_oID = keras.Input(shape=(1,), name="input_oID", dtype='int32')
input_hID = keras.Input(shape=(1,), name="input_hID", dtype='int32') # Keep as int32 for input

# --- Categorical Branch ---
# Embedding layers + Flatten
embedding_player_id = layers.Embedding(input_dim=num_players, output_dim=emb_dim, name="embed_player")(input_player_id)
flat_player_id = layers.Flatten()(embedding_player_id)

embedding_tID = layers.Embedding(input_dim=num_teams, output_dim=emb_dim, name="embed_team")(input_tID)
flat_tID = layers.Flatten()(embedding_tID)

embedding_oID = layers.Embedding(input_dim=num_opponents, output_dim=emb_dim, name="embed_opponent")(input_oID)
flat_oID = layers.Flatten()(embedding_oID)

# Handle hID: Cast to float and Flatten (no embedding for binary-like feature)
input_hID_float = layers.Lambda(lambda x: tf.cast(x, tf.float32), name="cast_hID")(input_hID)
flat_hID = layers.Flatten()(input_hID_float) # Shape becomes (batch_size,)

# Concatenate categorical features
cat_merged = layers.concatenate([flat_player_id, flat_tID, flat_oID, flat_hID], name="concat_cat")
cat_dense = layers.Dense(units=16, activation="relu", name="dense_cat")(cat_merged)

# --- Numerical Branch (LSTM) ---
lstm_branch = layers.LSTM(units=64, return_sequences=False, name="lstm_num")(input_seq)
lstm_dropout = layers.Dropout(rate=0.2, name="dropout_lstm")(lstm_branch)

# --- Merge Branches ---
merged = layers.concatenate([lstm_dropout, cat_dense], name="concat_merged")
dense_layer = layers.Dense(units=32, activation="relu", name="dense_merged")(merged)
output = layers.Dense(units=1, activation="linear", name="output")(dense_layer) # Linear activation for regression

# --- Define and Compile Model ---
model = keras.Model(
    inputs=[input_seq, input_player_id, input_tID, input_oID, input_hID],
    outputs=output,
    name="FPL_GK_Predictor"
)

model.compile(
    optimizer=keras.optimizers.Adam(),
    loss="mse",  # Mean Squared Error for regression
    metrics=["mae"] # Mean Absolute Error
    # run_eagerly=True # Uncomment for debugging, slows down training
)

model.summary() # Print model architecture

# Optional: Visualize the model
# tf.keras.utils.plot_model(model, show_shapes=True, rankdir="LR", to_file="model_plot.png")

#============================
# 8. Train the Model
#============================
print("\nStarting model training...")

# Prepare inputs as a dictionary matching the input layer names or order
train_inputs = {
    "input_seq": Xnum_train,
    "input_player_id": cat_player_id_train,
    "input_tID": cat_tID_train,
    "input_oID": cat_oID_train,
    "input_hID": cat_hID_train
}
val_inputs = {
    "input_seq": Xnum_val,
    "input_player_id": cat_player_id_val,
    "input_tID": cat_tID_val,
    "input_oID": cat_oID_val,
    "input_hID": cat_hID_val
}

# Add callbacks like EarlyStopping if desired
# early_stopping = keras.callbacks.EarlyStopping(monitor='val_loss', patience=5, restore_best_weights=True)

history = model.fit(
    train_inputs,
    y_train,
    epochs=20, # Adjust number of epochs as needed
    batch_size=32,
    validation_data=(val_inputs, y_val),
    # callbacks=[early_stopping], # Add callbacks here
    verbose=1 # Set to 1 or 2 for progress updates, 0 for silent
)

print("Training finished.")

# You can plot training history (loss, mae) using history.history

#============================
# 9. Generate Predictions & Invert Scaling on Validation Set
#============================
print("\nGenerating predictions on validation set...")

pred_all_scaled = model.predict(val_inputs)

# Build a predictions DataFrame
preds_df = pd.DataFrame({
    'row_id': row_ids_val, # Use the row_ids corresponding to the validation set
    'predicted_total_points_scaled': pred_all_scaled.flatten(), # Flatten Keras output
    'actual_total_points_scaled': y_val.flatten() # Flatten actual scaled targets
})

# Invert scaling to get actual point predictions
preds_df['predicted_total_points'] = preds_df['predicted_total_points_scaled'] * sigma + mu
preds_df['actual_total_points'] = preds_df['actual_total_points_scaled'] * sigma + mu

# Optionally, join additional information from the original gk dataframe
# Ensure the original gk dataframe ('gk') still exists and has the 'row_id' column
# Select columns you want to see in the final output
info_cols = ['row_id', 'GW', 'name'] + catF # Add original numerical features if desired (use numF_original)
gk_info = gk[info_cols].copy()

# Merge predictions with original info
final_preds_df = pd.merge(preds_df, gk_info, on='row_id', how='left')

# Display the first few rows of the predictions table
print("\nValidation Set Predictions (with original info):")
print(final_preds_df.head())

# You can now analyze final_preds_df (e.g., calculate MSE/MAE on actual points, compare predictions)
mae_actual = np.mean(np.abs(final_preds_df['predicted_total_points'] - final_preds_df['actual_total_points']))
mse_actual = np.mean((final_preds_df['predicted_total_points'] - final_preds_df['actual_total_points'])**2)
print(f"\nValidation MAE (actual points): {mae_actual:.4f}")
print(f"Validation MSE (actual points): {mse_actual:.4f}")