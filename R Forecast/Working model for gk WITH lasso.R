rm(list = ls(all = TRUE))


## 0: Preamble ----
library(keras)
library(tidyverse)
library(slider)
library(tensorflow)
#install.packages("glmnet")
library(glmnet)
tf$constant("Hello, TensorFlow!")

df <- read_csv("https://raw.githubusercontent.com/Nysgjerrigper/test/refs/heads/milp/Ekstra%20kolonner%2C%20stigende%20GW%2C%20alle%20tre%20sesonger(22-24)%2C%20heltall.csv")
alternativsammensatt <- df

#----------------------------
# 1. Extract GK Data
#----------------------------
gk <- alternativsammensatt %>%
  filter(position == "GK")

# (Assume that your gk data already contains a column "row_id". If not, you can add it:)
# gk <- gk %>% mutate(row_id = row_number())

#----------------------------
# 2. Define Features, Target & Scale
#----------------------------
numF <- c("assists", "creativity", "minutes", "goals_conceded", "saves",
          "bonus", "bps", "expected_assists", "expected_goal_involvements",
          "expected_goals", "ict_index", "own_goals", "red_cards",
          "threat", "transfers_in", "transfers_out", "yellow_cards",
          "expected_goals_conceded", "penalties_saved", "value",
          "selected", "transfers_balance", "starts", "influence",
          "clean_sheets")
catF <- c("player_id", "tID", "oID", "hID")
tar <- "total_points"


# Compute and store the mean and standard deviation for the target variable
mu <- mean(gk[[tar]], na.rm = TRUE)
sigma <- sd(gk[[tar]], na.rm = TRUE)

# Scale numerical features and target
gk <- gk %>%
  mutate(across(all_of(numF), ~ (.x - mean(.x, na.rm = TRUE)) / sd(.x, na.rm = TRUE)),
         !!tar := (.data[[tar]] - mu) / sigma)

#----------------------------
# 3. Build Rolling Windows
#----------------------------
# Define window size (number of past gameweeks to use)
ws <- 6

# Build numerical rolling windows
numW <- gk %>%
  group_by(player_id) %>%
  filter(n() > ws) %>%  # Only players with more than ws observations
  group_modify(~ {
    windows <- slide(.x[, numF],
                     .f = ~ as.matrix(.x),
                     .before = ws, 
                     .after = -1,
                     .complete = TRUE)
    valid_windows <- windows[(ws + 1):length(windows)]
    target <- .x[[tar]][(ws + 1):nrow(.x)]
    # Extract row_id for each valid window
    rid <- .x$row_id[(ws + 1):nrow(.x)]
    tibble(window = valid_windows, target = target, row_id = rid)
  }) %>%
  ungroup()

# Build categorical rolling windows (if needed for debugging; here we use only the current values)
catW <- gk %>%
  group_by(player_id) %>%
  filter(n() > ws) %>%
  group_modify(~ {
    df <- ungroup(.x) %>% mutate(player_id = .y$player_id)
    windows <- slide(df %>% select(all_of(catF)),
                     .f = ~ as.matrix(.x),
                     .before = ws,
                     #.after = -1,
                     .complete = TRUE)
    valid_windows <- windows[(ws + 1):length(windows)]
    target <- df[[tar]][(ws + 1):nrow(df)]
    tibble(window = valid_windows, target = target)
  }) %>%
  ungroup()

#----------------------------
# 4. Lasso regression
#----------------------------

# numW as data.matrix, tar as response variable
aggmat <- lapply(numW$window, function(flatW){
  colMeans(flatW, na.rm = TRUE)
})
x <- do.call(rbind, aggmat)
y <- numW$target

# K-fold CV
cvfit <- cv.glmnet(x,y,alpha=1)
minlambda <- cvfit$lambda.min

lassofit <- glmnet(x,y,alpha = 1, lambda = minlambda)

coef_lasso <- as.matrix(coef(lassofit))
numF <- rownames(coef_lasso)[coef_lasso[,1] != 0]
numF <- setdiff(numF, "(Intercept)")
cat("Selected features: ", numF, "\n")

# repeat rolling window, now only containing selected features
# Define window size (number of past gameweeks to use)
ws <- 6

# Build numerical rolling windows
numW <- gk %>%
  group_by(player_id) %>%
  filter(n() > ws) %>%  # Only players with more than ws observations
  group_modify(~ {
    windows <- slide(.x[, numF],
                     .f = ~ as.matrix(.x),
                     .before = ws, 
                     .after = -1,
                     .complete = TRUE)
    valid_windows <- windows[(ws + 1):length(windows)]
    target <- .x[[tar]][(ws + 1):nrow(.x)]
    # Extract row_id for each valid window
    rid <- .x$row_id[(ws + 1):nrow(.x)]
    tibble(window = valid_windows, target = target, row_id = rid)
  }) %>%
  ungroup()
#============================
# 5. Convert Rolling Windows to Arrays & Extract Targets
#============================
# Number of samples is the number of rows in numW
nSamp <- nrow(numW)

# Convert numerical windows to a 3D array of shape [nSamp, ws, length(numF)]
num_array <- array(
  unlist(numW$window),
  dim = c(nSamp, ws, length(numF))
)

# Targets (scaled): a matrix with one column
targets <- matrix(as.numeric(numW$target), ncol = 1)

# For categorical inputs, extract the current categorical values from the last row of each window.
# (If your windows were built solely from historical data, the "current" values need to come from the original data.)
# Here, we use the categorical rolling windows and extract the last row:
cat_current <- do.call(rbind, lapply(catW$window, function(mat) mat[nrow(mat), ]))
# Split into separate matrices (each of shape [nSamp, 1])
cat_player_id <- matrix(as.integer(cat_current[, "player_id"]), ncol = 1)
cat_tID <- matrix(as.integer(cat_current[, "tID"]), ncol = 1)
cat_oID <- matrix(as.integer(cat_current[, "oID"]), ncol = 1)
cat_hID <- matrix(as.integer(cat_current[, "hID"]), ncol = 1)

#============================
# 6. Split Data into Training and Validation Sets
#============================
set.seed(123)
idx <- sample(1:nSamp, size = floor(0.8 * nSamp))

# Training data
Xnum_train <- num_array[idx, , , drop = FALSE]
cat_player_id_train <- cat_player_id[idx, , drop = FALSE]
cat_tID_train <- cat_tID[idx, , drop = FALSE]
cat_oID_train <- cat_oID[idx, , drop = FALSE]
cat_hID_train <- cat_hID[idx, , drop = FALSE]
y_train <- targets[idx, , drop = FALSE]

# Validation data
Xnum_val <- num_array[-idx, , , drop = FALSE]
cat_player_id_val <- cat_player_id[-idx, , drop = FALSE]
cat_tID_val <- cat_tID[-idx, , drop = FALSE]
cat_oID_val <- cat_oID[-idx, , drop = FALSE]
cat_hID_val <- cat_hID[-idx, , drop = FALSE]
y_val <- targets[-idx, , drop = FALSE]

# Also, extract the row IDs for the validation samples (for later joining)
val_row_ids <- numW$row_id[-idx]

#============================
# 7. Build the Model
#============================
# Define maximum values for categorical features
num_players   <- max(gk$player_id)
num_teams     <- max(gk$tID)
num_opponents <- max(gk$oID)
# For hID, assume it???s binary

emb_dim <- 20  # Embedding dimension

# Create input layers for current categorical values
input_player_id <- layer_input(shape = c(1), dtype = "int32", name = "input_player_id")
input_tID <- layer_input(shape = c(1), dtype = "int32", name = "input_tID")
input_oID <- layer_input(shape = c(1), dtype = "int32", name = "input_oID")
input_hID <- layer_input(shape = c(1), dtype = "int32", name = "input_hID")

embedding_player_id <- input_player_id %>% 
  layer_embedding(input_dim = num_players + 1, output_dim = emb_dim, mask_zero = TRUE) %>% 
  layer_flatten()
embedding_tID <- input_tID %>% 
  layer_embedding(input_dim = num_teams + 1, output_dim = emb_dim, mask_zero = TRUE) %>% 
  layer_flatten()
embedding_oID <- input_oID %>% 
  layer_embedding(input_dim = num_opponents + 1, output_dim = emb_dim, mask_zero = TRUE) %>% 
  layer_flatten()
# For hID, convert to float32 to match types
input_hID_flat <- input_hID %>% 
  layer_lambda(function(x) tf$cast(x, tf$float32)) %>% 
  layer_flatten()

# Merge categorical branch
cat_merged <- layer_concatenate(list(embedding_player_id, embedding_tID, embedding_oID, input_hID_flat)) %>%
  layer_dense(units = 16, activation = "relu")

# Numerical branch: input for rolling window data
input_seq <- layer_input(shape = c(ws, length(numF)), name = "input_seq")
lstm_branch <- input_seq %>%
  layer_lstm(units = 64, return_sequences = FALSE) %>%
  layer_dropout(rate = 0.2)

# Merge both branches and build the output
merged <- layer_concatenate(list(lstm_branch, cat_merged))
dense_layer <- merged %>% layer_dense(units = 32, activation = "relu")
output <- dense_layer %>% layer_dense(units = 1, activation = "linear")

# Build and compile the model
model <- keras::keras_model(
  inputs = list(input_seq, input_player_id, input_tID, input_oID, input_hID),
  outputs = output
)
model %>% keras::compile(
  optimizer = optimizer_adam(),
  loss = "mse",
  metrics = "mae",
  run_eagerly = TRUE
)
summary(model)

#============================
# 8. Train the Model
#============================
history <- model %>% fit(
  x = list(
    input_seq = Xnum_train,
    input_player_id = cat_player_id_train,
    input_tID = cat_tID_train,
    input_oID = cat_oID_train,
    input_hID = cat_hID_train
  ),
  y = y_train,
  epochs = 20,
  batch_size = 32,
  validation_data = list(
    list(
      input_seq = Xnum_val,
      input_player_id = cat_player_id_val,
      input_tID = cat_tID_val,
      input_oID = cat_oID_val,
      input_hID = cat_hID_val
    ),
    y_val
  )
)

#============================
# 9. Generate Predictions & Invert Scaling on Validation Set
#============================
# Use the validation set for predictions
pred_all <- model %>% predict(
  list(
    input_seq = Xnum_val,
    input_player_id = cat_player_id_val,
    input_tID = cat_tID_val,
    input_oID = cat_oID_val,
    input_hID = cat_hID_val
  )
)

# Build a predictions table using the validation row IDs
preds_tbl <- tibble(
  row_id = val_row_ids,  # row IDs corresponding to the validation set
  predicted_total_points_scaled = as.vector(pred_all),
  actual_total_points_scaled = as.vector(y_val)
) %>% 
  mutate(
    predicted_total_points = predicted_total_points_scaled * sigma + mu,
    actual_total_points = actual_total_points_scaled * sigma + mu
  )

# Optionally, join additional information from gk (ensure gk has a "row_id" column)
preds_tbl <- preds_tbl %>%
  left_join(gk %>% select(row_id, GW, name, all_of(numF), all_of(catF)), by = "row_id")

# View the predictions table
print(preds_tbl)
