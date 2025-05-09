# Forecasts for FPL Script to Master thesis EBA&PAM
# To load save weights ----
# Kaggle Export: The .hdf5 weight files saved in /kaggle/working/ will appear in the "Output" section of your Kaggle notebook session after it completes. You can download them from there.
# Loading Later: Remember, when you want to use these weights later (in another notebook or locally), you MUST:
# Rebuild the exact same model architecture using keras_model().
# Compile the model using the exact same optimizer, loss, and metrics.
# Load the weights using load_model_weights_hdf5().
# Load the corresponding scaling factors (mu, sigma, numF) to prepare input data and unscale predictions.

rm(list = ls(all = TRUE))

## 0: Preamble ----
Sys.setenv(WORKON_HOME = "C:/Users/peram/Documents/test/R Forecast/.virtualenvs")
reticulate::use_virtualenv("C:/Users/peram/Documents/test/R Forecast/.virtualenvs/r-reticulate", required = TRUE)
# Adjust code to chosen place of
# 1. Virtual Python Environment
# 2. Forces R session to use the a specific virtualenv
# This way the virtual environment does not get installed
# in your onedrive\documents folder.


#install.packages("keras")
library(reticulate)
#virtualenv_create("r-reticulate", python = install_python())
library(keras)
#install_keras(envname = "r-reticulate")

# Rest of the packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggthemes, tidyverse, slider,
               slider,glmnet,httr,jsonlite,tensorflow,
               Metrics, pastecs, stats)

# Lists to store results ----
model_list       <- list()
scaling_factors  <- list()
predictions_list <- list()
metrics_list     <- list()

# Global user inputs ----
# Trainingsplit : in-sample <= split_gw < out-of-sample
split_gw <- 38+38
# LSTM input
epoker <- 50
# Windows Size Input
vindu <- 3
# Global embedding dimensions for all positions
emb_dim <- 16
# This is global control of early stopping in the callback function.
num_patience <- 10

# Metrics for LSTM Models. Needs to be outside is depreceated needs to be implemented inside the models new error
# as og 05.05 :/
# metrics_regression <- metric_root_mean_squared_error()
# metric_mean_absolute_error(),
# metric_mean_squared_error(),
# metric_mean_absolute_percentage_error(),
# metric_cosine_similarity()

# Fetch data and prelim data manipulation ----
df <- read_csv("C:/Users/peram/Documents/test/Datasett/Ekstra kolonner, stigende GW, alle tre sesonger(22-24), heltall.csv")
alternativsammensatt <- df

## Positions Partitions ----
gk <- df %>%
  filter(position == "GK") %>%
  # create a unique row ID and re-index factors
  mutate(
    row_id  = row_number(),
    pID_idx = as.integer(factor(player_id)),
    tID_idx = as.integer(factor(tID)),
    oID_idx = as.integer(factor(oID))
  )
def <- df %>%
  filter(position == "DEF") %>%
  # create a unique row ID and re-index factors
  mutate(
    row_id  = row_number(),
    pID_idx = as.integer(factor(player_id)),
    tID_idx = as.integer(factor(tID)),
    oID_idx = as.integer(factor(oID))
  )
mid <- df %>%
  filter(position == "MID") %>%
  # create a unique row ID and re-index factors
  mutate(
    row_id  = row_number(),
    pID_idx = as.integer(factor(player_id)),
    tID_idx = as.integer(factor(tID)),
    oID_idx = as.integer(factor(oID))
  )
fwd <- df %>%
  filter(position == "FWD") %>%
  # create a unique row ID and re-index factors
  mutate(
    row_id  = row_number(),
    pID_idx = as.integer(factor(player_id)),
    tID_idx = as.integer(factor(tID)),
    oID_idx = as.integer(factor(oID))
  )

posliste <- list(gk = gk, def = def, mid = mid, fwd = fwd)
lapply(posliste, head, n = 1)
lapply(posliste, tail, n = 1)

# Save for later
unscaled_gk <- gk
unscaled_def <- def
unscaled_mid <- mid
unscaled_fwd <- fwd


# 1: Goalkeepers (GK) ----

## GK 1.1 Define Features, Target & Scale ----
#numF = numerical features, tar = target
numF <- c(
  "assists","creativity","minutes","goals_conceded","saves","bonus","bps",
  "expected_assists","expected_goal_involvements","expected_goals","ict_index",
  "own_goals","red_cards","threat","transfers_in","transfers_out","yellow_cards",
  "expected_goals_conceded","penalties_saved","value","selected",
  "transfers_balance","starts","influence","clean_sheets"
)
tar <- "total_points"
initial_numF_gk <- length(numF)
cat("Numeriske features:", numF, "\n")
cat("Target variabel:", tar, "\n")
cat("Ferdig\n")

gk <- gk %>%
  arrange(pID_idx, GW) %>%
  group_by(pID_idx) %>%
  ungroup()

# Compute and store the mean and standard deviation for the target variable

mu <- mean(gk[[tar]], na.rm = TRUE)
sigma <- sd(gk[[tar]], na.rm = TRUE)

# Scale numerical features and target
gk <- gk %>%
  mutate(across(all_of(numF), 
                ~ (.x - mean(.x, na.rm = TRUE)) / sd(.x, na.rm = TRUE)),
         !!tar := (.data[[tar]] - mu) / sigma)
cat("Chunk done")

## GK 1.2: Build rolling numerical windows ----

# Define window size (number of past gameweeks to use)
ws <- vindu

# Build numerical rolling windows (numW)
numW <- gk %>%
  group_by(pID_idx) %>%
  filter(n() > ws) %>%
  group_modify(~{
    W   <- slide(.x[, numF], .f=~as.matrix(.x),
                 .before=ws, .after=-1, .complete=TRUE)
    valid <- W[(ws+1):length(W)]
    tgt   <- .x[[tar]][(ws+1):nrow(.x)]
    rid   <- .x$row_id[(ws+1):nrow(.x)]
    tibble(window=valid, target=tgt, row_id=rid)
  }) %>% ungroup() %>%
  left_join(select(gk, row_id, GW), by="row_id")

cat("Chunk done\n")

## GK 1.3: Lasso Regression ----

# 1. Aggregate each rolling window by column means (for each sample)
Xl <- do.call(rbind, lapply(numW$window, colMeans, na.rm = TRUE))
sum(is.na(Xl))

# 2. Extract the target variable for each window
yl <- numW$target

# 3. Perform cross-validated Lasso regression to select lambda
cvfit <- cv.glmnet(Xl, yl, alpha = 1)

# 4. Get the lambda value that minimizes cross-validated error
minlambda <- cvfit$lambda.min

# 5. Fit Lasso regression with the selected lambda
lassofit <- glmnet(Xl, yl, alpha = 1, lambda = minlambda)

# 6. Extract the names of features with non-zero coefficients (selected by Lasso)
keepF <- rownames(as.matrix(coef(lassofit)))[coef(lassofit)[, 1] != 0] %>%
  setdiff("(Intercept)")
numF_gk<- keepF

# 7. Update numF to only include selected features
numF <- keepF
cat("Lasso selected features:", numF_gk,"Initial number of features:", initial_numF_gk, "\n",
    "Number of selected features:",length(numF_gk), "\n")

## GK 1.4.1: Rebuild Numeric Windows (numW) with Lasso selected variables ----
ws <- vindu
# Rebuild rolling numerical windows using only Lasso-selected features (numF)
# Why: After feature selection, 
# we want to ensure that only the most relevant variables are used for modeling, 
# which can improve model performance and reduce overfitting.
numW <- gk %>%
  group_by(pID_idx) %>%
  filter(n() > ws) %>%
  group_modify(~{
    W     <- slide(.x[, numF], .f=~as.matrix(.x),
                   .before=ws, .after=-1, .complete=TRUE)
    valid <- W[(ws+1):length(W)]
    tgt   <- .x[[tar]][(ws+1):nrow(.x)]
    rid   <- .x$row_id[(ws+1):nrow(.x)]
    tibble(window=valid, target=tgt, row_id=rid)
  }) %>% ungroup() %>%
  left_join(select(gk, row_id, GW), by="row_id")
head(numW, n = 2)
cat("Chunk done\n")

### GK 1.4.2: Build & align categorical windows (by player_id!) ----

# Build rolling windows for categorical variables (pID_idx, tID_idx, oID_idx, hID) for each player
catW_all <- gk %>%
  group_by(player_id) %>%
  filter(n() > ws) %>%
  group_modify(~{
    # For each player, create rolling windows of categorical indices
    W <- slide(
      .x %>% select(pID_idx, tID_idx, oID_idx, hID),
      .f = ~as.matrix(.x), .before = ws, .after = -1, .complete = TRUE
    )
    # Keep only valid windows (those with enough history)
    valid <- W[(ws + 1):length(W)]
    # Get corresponding row_ids for alignment
    rid <- .x$row_id[(ws + 1):nrow(.x)]
    tibble(window = valid, row_id = rid)
  }) %>% ungroup()

# Align categorical windows to numeric windows using row_id
catW <- numW %>%
  select(row_id) %>%
  left_join(catW_all, by = "row_id")

### GK 1.4.3: To Arrays
# Number of samples (number of rolling windows)
nSamp <- nrow(numW)

# Convert list of rolling numerical windows to a 3D array: [samples, window size, num features]
num_array <- array(unlist(numW$window), dim = c(nSamp, ws, length(numF)))

# Extract targets (scaled total_points) as a column matrix
targets <- matrix(numW$target, ncol = 1)

# Extract historical categorical indices for each window (for possible use in sequence models)
pID_hist_array <- t(sapply(catW$window, function(m) m[, "pID_idx"]))
tID_hist_array <- t(sapply(catW$window, function(m) m[, "tID_idx"]))
oID_hist_array <- t(sapply(catW$window, function(m) m[, "oID_idx"]))
hID_hist_array <- t(sapply(catW$window, function(m) m[, "hID"]))

# Extract current (last in window) categorical values for each sample
cat_cur_mat <- do.call(rbind, lapply(catW$window, function(m) m[nrow(m), ]))
pID_cur_array <- matrix(cat_cur_mat[, "pID_idx"], ncol = 1)
tID_cur_array <- matrix(cat_cur_mat[, "tID_idx"], ncol = 1)
oID_cur_array <- matrix(cat_cur_mat[, "oID_idx"], ncol = 1)
hID_cur_array <- matrix(cat_cur_mat[, "hID"], ncol = 1)


## GK 1.5: Split Data into Training and Validation Sets ----
idx <- which(numW$GW <= split_gw)
Xnum_train <- num_array[idx,,,drop=FALSE]
Xnum_val <- num_array[-idx,,,drop=FALSE]
y_train <- targets[idx,,drop=FALSE]
y_val <- targets[-idx,,drop=FALSE]

Xcat_hist_tr <- list(
  pID_hist = pID_hist_array[idx,,drop=FALSE],
  tID_hist = tID_hist_array[idx,,drop=FALSE],
  oID_hist = oID_hist_array[idx,,drop=FALSE],
  hID_hist = hID_hist_array[idx,,drop=FALSE]
)
Xcat_hist_va <- list(
  pID_hist = pID_hist_array[-idx,,drop=FALSE],
  tID_hist = tID_hist_array[-idx,,drop=FALSE],
  oID_hist = oID_hist_array[-idx,,drop=FALSE],
  hID_hist = hID_hist_array[-idx,,drop=FALSE]
)

Xcat_cur_tr <- list(
  pID_cur = pID_cur_array[idx,,drop=FALSE],
  tID_cur = tID_cur_array[idx,,drop=FALSE],
  oID_cur = oID_cur_array[idx,,drop=FALSE],
  hID_cur = hID_cur_array[idx,,drop=FALSE]
)
Xcat_cur_va <- list(
  pID_cur = pID_cur_array[-idx,,drop=FALSE],
  tID_cur = tID_cur_array[-idx,,drop=FALSE],
  oID_cur = oID_cur_array[-idx,,drop=FALSE],
  hID_cur = hID_cur_array[-idx,,drop=FALSE]
)

cat("Chunk Done\n")

## GK 1.6: Building the Keras Neural Network Model ----
### GK Dimensions of input layers ----
# nP == Number of players, nT == Number of teams, nO == Number of opponent teams
nP <- max(gk$pID_idx, na.rm=TRUE)
nT <- max(gk$tID_idx, na.rm=TRUE)
nO <- max(gk$oID_idx, na.rm=TRUE)

### GK Input Layers ----
inp_num <- layer_input(shape=c(ws,length(numF)), name="input_seq")
inp_ph  <- layer_input(shape=c(ws), dtype="int32", name="pID_hist")
inp_th  <- layer_input(shape=c(ws), dtype="int32", name="tID_hist")
inp_oh  <- layer_input(shape=c(ws), dtype="int32", name="oID_hist")
inp_hh  <- layer_input(shape=c(ws), dtype="int32", name="hID_hist")
inp_pc  <- layer_input(shape=c(1),   dtype="int32", name="pID_cur")
inp_tc  <- layer_input(shape=c(1),   dtype="int32", name="tID_cur")
inp_oc  <- layer_input(shape=c(1),   dtype="int32", name="oID_cur")
inp_hc  <- layer_input(shape=c(1),   dtype="int32", name="hID_cur")

### GK Embedding Layers ----
emb_ph <- inp_ph %>%
  layer_embedding(input_dim = nP + 1, output_dim = emb_dim, mask_zero = TRUE)
emb_th <- inp_th %>%
  layer_embedding(input_dim = nT + 1, output_dim = emb_dim, mask_zero = TRUE)
emb_oh <- inp_oh %>%
  layer_embedding(input_dim = nO + 1, output_dim = emb_dim, mask_zero = TRUE)
emb_hh <- inp_hh %>%
  layer_lambda(function(x){
    x_f <- tf$cast(x, tf$float32)
    tf$expand_dims(x_f, axis = as.integer(-1))
  })

### GK Concatenate Historical Embeddings for LSTM Sequence ----
cat_hist_seq <- layer_concatenate(list(emb_ph,emb_th,emb_oh,emb_hh), axis=-1)

### GK LSTM Layer for Sequential Modeling ----
lstm_in  <- layer_concatenate(list(inp_num, cat_hist_seq), axis=-1)
lstm_out <- lstm_in %>%
  layer_lstm(units=64, return_sequences=FALSE) %>%
  layer_dropout(rate=0.2)

### GK Embedding Layers for Current Categorical Inputs ----
emb_pc <- inp_pc %>%
  layer_embedding(input_dim = nP + 1, output_dim = emb_dim, mask_zero = TRUE) %>%
  layer_flatten()
emb_tc <- inp_tc %>%
  layer_embedding(input_dim = nT + 1, output_dim = emb_dim, mask_zero = TRUE) %>%
  layer_flatten()
emb_oc <- inp_oc %>%
  layer_embedding(input_dim = nO + 1, output_dim = emb_dim, mask_zero = TRUE) %>%
  layer_flatten()
emb_hc <- inp_hc %>%
  layer_lambda(function(x) tf$cast(x, tf$float32))

### GK Concatenate Current Categorical Embeddings ----
cat_cur_vec <- layer_concatenate(list(emb_pc,emb_tc,emb_oc,emb_hc), axis=-1)

### GK Merge LSTM Output and Current Categorical Features ----
merged <- layer_concatenate(list(lstm_out, cat_cur_vec), axis=-1)
head   <- merged %>% layer_dense(units=32, activation="relu")
out    <- head   %>% layer_dense(units=1,  activation="linear")

### GK Build and Compile Model ----
model <- keras_model(
  inputs  = list(inp_num, inp_ph, inp_th, inp_oh, inp_hh,
                 inp_pc, inp_tc, inp_oc, inp_hc),
  outputs = out
)

model %>% compile(
  optimizer = optimizer_adam(),
  loss      = "mse",
  metrics   = metric_root_mean_squared_error()
)

### GK Model Summary and Visualization ----
summary(model)
plot(model)

### GK Save Model Plot to File ----
keras_python <- import("keras")
keras_python$utils$plot_model(
  model,
  to_file = "keras-model-gk.png",
  show_shapes = TRUE,
  show_dtype = FALSE,
  show_layer_names = TRUE,
  rankdir = "TB",
  expand_nested = FALSE,
  dpi = 200,
  show_layer_activations = FALSE
)

### GK Display Model Plot in R ----
library(png)
img <- readPNG("keras-model-gk.png")
grid::grid.raster(img)

## GK 1.7: Callback Functions and Scaling Factors ----

# Filepath for saving the BEST weights (unique for each position)
weights_filepath_gk <- "R Forecast/best_gk.hdf5"
cat("Will save best GK weights to:", weights_filepath_gk, "\n")

# 1. Tidlig stopp funksjon, laster inn siste beste loss value.
# patience: how many epochs to wait after last improvement before stopping
# restore_best_weights: load the weights from the epoch with the best val_loss
early_stopping <- callback_early_stopping(
  monitor = "val_loss",
  patience = num_patience, 
  restore_best_weights = TRUE
)

# 2. Model Checkpoint Callback (to save best weights)
model_checkpoint <- callback_model_checkpoint(
  filepath = weights_filepath_gk, # Path defined above
  monitor = "val_loss",           # Monitor the same metric as early stopping
  save_best_only = TRUE,          # IMPORTANT: Only save when val_loss improves
  save_weights_only = TRUE,       # IMPORTANT: Save ONLY the weights, not the whole model
  mode = "min",                   # We want to minimize loss
  verbose = 1                     # Optional: print message when weights are saved
)

# 3. Reduce Learning Rate on Plateau Callback
reduce_lr <- callback_reduce_lr_on_plateau(
  monitor = "val_loss",  # Monitor validation loss (same as early stopping)
  factor = 0.2,          # Reduce LR to 20% of its current value (lr * factor)
  patience = num_patience,           # Reduce LR if val_loss doesn't improve for 5 epochs
  min_lr = 1e-6,         # Don't reduce the LR below this value
  verbose = 1            # Print a message when LR is reduced
)

scaling_factors$gk <- list(mu = mu,
                           sigma = sigma,
                           numF = numF) # numF here is the one selected by Lasso for GK
cat("GK scaling factors stored.\n")

## GK 1.8: Train Goalkeeper LSTM Model ----

cat("Starting GK model training...\n")

history <- model %>% fit(
  x = c(
    list(input_seq = Xnum_train),
    Xcat_hist_tr,
    Xcat_cur_tr
  ),
  y              = y_train,
  epochs         = epoker,
  batch_size     = 16,
  validation_data= list(
    c(
      list(input_seq = Xnum_val),
      Xcat_hist_va,
      Xcat_cur_va
    ),
    y_val
  ),
  callbacks = list(early_stopping, model_checkpoint, reduce_lr)
)
cat("GK model training finished.\n")


## GK 1.9: Generate Predictions & Invert Scaling on Validation Set ----
# Use the validation set for predictions
val_row_ids <- numW$row_id[-idx]

pred_all <- model %>% predict(
  c(
    list(input_seq = Xnum_val),
    Xcat_hist_va,
    Xcat_cur_va
  )
)

preds_gk <- tibble(
  row_id                         = val_row_ids,
  predicted_total_points_scaled  = as.vector(pred_all),
  actual_total_points_scaled     = as.vector(y_val)
) %>%
  mutate(
    predicted_total_points = predicted_total_points_scaled * sigma + mu,
    actual_total_points    = actual_total_points_scaled    * sigma + mu
  ) %>%
  left_join(unscaled_gk, by = "row_id")

glimpse(preds_gk)

# Build a predictions table using the validation row IDs
preds_gk <- tibble(
  row_id = val_row_ids,  # row IDs corresponding to the validation set
  predicted_total_points_scaled = as.vector(pred_all),
  actual_total_points_scaled = as.vector(y_val)
) %>% 
  mutate(
    predicted_total_points = predicted_total_points_scaled * sigma + mu,
    actual_total_points = actual_total_points_scaled * sigma + mu
  )


preds_gk <- preds_gk %>%
  left_join(unscaled_gk, by = "row_id") # Use unscaled_gk

# View the predictions table
glimpse(preds_gk)

## GK 1.10 Save model and validation set predictions ----
# Store GK model and scaling factors 
model_list$gk <- model
cat("GK model stored in model_list$gk.\n")

# Clean up validation predictions (select essential columns)
preds_gk_clean <- preds_gk %>%
  select(row_id, GW, player_id, name, position, team,
         actual_total_points, predicted_total_points, value)
cat("Validation predictions stored in preds_gk_clean.\n")

## GK 1.11: Plots for Evaluation ----

### GK 1.11.1 Save training history ----
history_gk <- history # class() "keras_training_history"
history_df_gk <- data.frame(history_gk)

### GK 1.11.2 Training History Plot ----
plot_history_gk <- plot(history_gk)
plot_history_gk;str(history_df_gk)

### GK 1.11.3 Actual vs Predicted Points Plot ----
plot_actual_predicted_gk <- ggplot(preds_gk, aes(x = actual_total_points, y = predicted_total_points)) +
  geom_point(alpha = 0.5) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Actual vs. Predicted Total Points (Validation Set)",
       x = "Actual Total Points",
       y = "Predicted Total Points") +
  facet_wrap(~ position) +
  theme_grey() +
  coord_cartesian(xlim = range(preds_gk$actual_total_points, na.rm = TRUE),
                  ylim = range(preds_gk$predicted_total_points, na.rm = TRUE))
plot_actual_predicted_gk                  

### GK 1.11.4: Residuals vs Predicted ----
preds_gk_residuals <- preds_gk %>%
  mutate(residual = actual_total_points - predicted_total_points)

plot_residuals_predicted_gk <- ggplot(preds_gk_residuals, aes(x = predicted_total_points, y = residual)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs. Predicted Total Points (Validation Set)",
       x = "Predicted Total Points",
       y = "Residual (Actual - Predicted)") +
  facet_wrap(~ position) +
  theme_grey()  
plot_residuals_predicted_gk

### GK 1.11.5: Distribution of Residuals ----
plot_distribution_residuals_gk <- ggplot(preds_gk_residuals, aes(x = residual)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "blue", alpha = 0.7) +
  geom_density(color = "red") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Distribution of Residuals (Validation Set)",
       x = "Residual (Actual - Predicted)",
       y = "Density") +
  facet_wrap(~ position) +
  theme_grey()
plot_distribution_residuals_gk

### GK 1.11.6 Return list containing loss value and metrics ----
# Evaluate model on validation data using consistent variable names
model_evaluation <- model |> evaluate(
  c(
    list(input_seq = Xnum_val),
    Xcat_hist_va,
    Xcat_cur_va
  ),
  y_val,
  verbose = 1
)
# Print the evaluation results
print(model_evaluation)

# Store validation metrics
validation_metrics <- list()
validation_metrics$gk <- model_evaluation
cat("Validation metrics stored in validation_metrics$gk.\n")

# 2: Defenders (DEF) ----

## DEF 2.1 Define Features, Target & Scale ----
#numF = numerical features, tar = target
numF <- c("goals_scored","assists", "creativity", "minutes", "goals_conceded",
          "bonus", "bps", "expected_assists", "expected_goal_involvements",
          "expected_goals", "ict_index", "own_goals", "red_cards",
          "threat", "transfers_in", "transfers_out", "yellow_cards",
          "expected_goals_conceded", "value",
          "selected", "transfers_balance", "starts", "influence",
          "clean_sheets"
)
tar <- "total_points"
initial_numF_def <- length(numF)

cat("Numeriske features:", numF, "\n")
cat("Target variabel:", tar, "\n")
cat("Ferdig\n")

def <- def %>%
  arrange(pID_idx, GW) %>%
  group_by(pID_idx) %>%
  ungroup()

# Compute and store the mean and standard deviation for the target variable

mu <- mean(def[[tar]], na.rm = TRUE)
sigma <- sd(def[[tar]], na.rm = TRUE)

# Scale numerical features and target
def <- def %>%
  mutate(across(all_of(numF), 
                ~ (.x - mean(.x, na.rm = TRUE)) / sd(.x, na.rm = TRUE)),
         !!tar := (.data[[tar]] - mu) / sigma)
cat("Chunk done")

## DEF 2.2: Build rolling numerical windows

# Define window size (number of past gameweeks to use)
ws <- vindu

# Build numerical rolling windows (numW)
numW <- def %>%
  group_by(pID_idx) %>%
  filter(n() > ws) %>%
  group_modify(~{
    W   <- slide(.x[, numF], .f=~as.matrix(.x),
                 .before=ws, .after=-1, .complete=TRUE)
    valid <- W[(ws+1):length(W)]
    tgt   <- .x[[tar]][(ws+1):nrow(.x)]
    rid   <- .x$row_id[(ws+1):nrow(.x)]
    tibble(window=valid, target=tgt, row_id=rid)
  }) %>% ungroup() %>%
  left_join(select(def, row_id, GW), by="row_id")

cat("Chunk done\n")

## DEF 2.3: Lasso Regression ----

# 1. Aggregate each rolling window by column means (for each sample)
Xl <- do.call(rbind, lapply(numW$window, colMeans, na.rm = TRUE))
length(Xl)
sum(is.na(Xl))

# 2. Extract the target variable for each window
yl <- numW$target

# 3. Perform cross-validated Lasso regression to select lambda
cvfit <- cv.glmnet(Xl, yl, alpha = 1)

# 4. Get the lambda value that minimizes cross-validated error
minlambda <- cvfit$lambda.min

# 5. Fit Lasso regression with the selected lambda
lassofit <- glmnet(Xl, yl, alpha = 1, lambda = minlambda)

# 6. Extract the names of features with non-zero coefficients (selected by Lasso)
keepF <- rownames(as.matrix(coef(lassofit)))[coef(lassofit)[, 1] != 0] %>%
  setdiff("(Intercept)")
numF_def <- keepF
# 7. Update numF to only include selected features
numF <- keepF

cat("Lasso selected features:", numF_def,"Initial number of features:", initial_numF_def, "\n",
    "Number of selected features:",length(numF_def), "\n")

## DEF 2.4.1: Rebuild Numeric Windows (numW) with Lasso selected variables ----
ws <- vindu
# Rebuild rolling numerical windows using only Lasso-selected features (numF)
# Why: After feature selection, 
# we want to ensure that only the most relevant variables are used for modeling, 
# which can improve model performance and reduce overfitting.
numW <- def %>%
  group_by(pID_idx) %>%
  filter(n() > ws) %>%
  group_modify(~{
    W     <- slide(.x[, numF], .f=~as.matrix(.x),
                   .before=ws, .after=-1, .complete=TRUE)
    valid <- W[(ws+1):length(W)]
    tgt   <- .x[[tar]][(ws+1):nrow(.x)]
    rid   <- .x$row_id[(ws+1):nrow(.x)]
    tibble(window=valid, target=tgt, row_id=rid)
  }) %>% ungroup() %>%
  left_join(select(def, row_id, GW), by="row_id")
cat("Chunk done\n")

### DEF 2.4.2: Build & align categorical windows (by player_id!) ----

# Build rolling windows for categorical variables (pID_idx, tID_idx, oID_idx, hID) for each player
catW_all <- def %>%
  group_by(player_id) %>%
  filter(n() > ws) %>%
  group_modify(~{
    # For each player, create rolling windows of categorical indices
    W <- slide(
      .x %>% select(pID_idx, tID_idx, oID_idx, hID),
      .f = ~as.matrix(.x), .before = ws, .after = -1, .complete = TRUE
    )
    # Keep only valid windows (those with enough history)
    valid <- W[(ws + 1):length(W)]
    # Get corresponding row_ids for alignment
    rid <- .x$row_id[(ws + 1):nrow(.x)]
    tibble(window = valid, row_id = rid)
  }) %>% ungroup()

# Align categorical windows to numeric windows using row_id
catW <- numW %>%
  select(row_id) %>%
  left_join(catW_all, by = "row_id")

### DEF 2.4.3: To Arrays
# Number of samples (number of rolling windows)
nSamp <- nrow(numW)

# Convert list of rolling numerical windows to a 3D array: [samples, window size, num features]
num_array <- array(unlist(numW$window), dim = c(nSamp, ws, length(numF)))

# Extract targets (scaled total_points) as a column matrix
targets <- matrix(numW$target, ncol = 1)

# Extract historical categorical indices for each window (for possible use in sequence models)
pID_hist_array <- t(sapply(catW$window, function(m) m[, "pID_idx"]))
tID_hist_array <- t(sapply(catW$window, function(m) m[, "tID_idx"]))
oID_hist_array <- t(sapply(catW$window, function(m) m[, "oID_idx"]))
hID_hist_array <- t(sapply(catW$window, function(m) m[, "hID"]))

# Extract current (last in window) categorical values for each sample
cat_cur_mat <- do.call(rbind, lapply(catW$window, function(m) m[nrow(m), ]))
pID_cur_array <- matrix(cat_cur_mat[, "pID_idx"], ncol = 1)
tID_cur_array <- matrix(cat_cur_mat[, "tID_idx"], ncol = 1)
oID_cur_array <- matrix(cat_cur_mat[, "oID_idx"], ncol = 1)
hID_cur_array <- matrix(cat_cur_mat[, "hID"], ncol = 1)


## DEF 2.5: Split Data into Training and Validation Sets ----
idx <- which(numW$GW <= split_gw)
Xnum_train <- num_array[idx,,,drop=FALSE]
Xnum_val <- num_array[-idx,,,drop=FALSE]
y_train <- targets[idx,,drop=FALSE]
y_val <- targets[-idx,,drop=FALSE]

Xcat_hist_tr <- list(
  pID_hist = pID_hist_array[idx,,drop=FALSE],
  tID_hist = tID_hist_array[idx,,drop=FALSE],
  oID_hist = oID_hist_array[idx,,drop=FALSE],
  hID_hist = hID_hist_array[idx,,drop=FALSE]
)
Xcat_hist_va <- list(
  pID_hist = pID_hist_array[-idx,,drop=FALSE],
  tID_hist = tID_hist_array[-idx,,drop=FALSE],
  oID_hist = oID_hist_array[-idx,,drop=FALSE],
  hID_hist = hID_hist_array[-idx,,drop=FALSE]
)

Xcat_cur_tr <- list(
  pID_cur = pID_cur_array[idx,,drop=FALSE],
  tID_cur = tID_cur_array[idx,,drop=FALSE],
  oID_cur = oID_cur_array[idx,,drop=FALSE],
  hID_cur = hID_cur_array[idx,,drop=FALSE]
)
Xcat_cur_va <- list(
  pID_cur = pID_cur_array[-idx,,drop=FALSE],
  tID_cur = tID_cur_array[-idx,,drop=FALSE],
  oID_cur = oID_cur_array[-idx,,drop=FALSE],
  hID_cur = hID_cur_array[-idx,,drop=FALSE]
)

cat("Chunk Done\n")

## DEF 2.6: Building the Keras Neural Network Model ----
### DEF Dimensions of input layers ----
# nP == Number of players, nT == Number of teams, nO == Number of opponent teams
nP <- max(def$pID_idx, na.rm=TRUE)
nT <- max(def$tID_idx, na.rm=TRUE)
nO <- max(def$oID_idx, na.rm=TRUE)

### DEF Input Layers ----
inp_num <- layer_input(shape=c(ws,length(numF)), name="input_seq")
inp_ph  <- layer_input(shape=c(ws), dtype="int32", name="pID_hist")
inp_th  <- layer_input(shape=c(ws), dtype="int32", name="tID_hist")
inp_oh  <- layer_input(shape=c(ws), dtype="int32", name="oID_hist")
inp_hh  <- layer_input(shape=c(ws), dtype="int32", name="hID_hist")
inp_pc  <- layer_input(shape=c(1),   dtype="int32", name="pID_cur")
inp_tc  <- layer_input(shape=c(1),   dtype="int32", name="tID_cur")
inp_oc  <- layer_input(shape=c(1),   dtype="int32", name="oID_cur")
inp_hc  <- layer_input(shape=c(1),   dtype="int32", name="hID_cur")

### DEF Embedding Layers ----
emb_ph <- inp_ph %>%
  layer_embedding(input_dim = nP + 1, output_dim = emb_dim, mask_zero = TRUE)
emb_th <- inp_th %>%
  layer_embedding(input_dim = nT + 1, output_dim = emb_dim, mask_zero = TRUE)
emb_oh <- inp_oh %>%
  layer_embedding(input_dim = nO + 1, output_dim = emb_dim, mask_zero = TRUE)
emb_hh <- inp_hh %>%
  layer_lambda(function(x){
    x_f <- tf$cast(x, tf$float32)
    tf$expand_dims(x_f, axis = as.integer(-1))
  })

### DEF Concatenate Historical Embeddings for LSTM Sequence ----
cat_hist_seq <- layer_concatenate(list(emb_ph,emb_th,emb_oh,emb_hh), axis=-1)

### DEF LSTM Layer for Sequential Modeling ----
lstm_in  <- layer_concatenate(list(inp_num, cat_hist_seq), axis=-1)
lstm_out <- lstm_in %>%
  layer_lstm(units=64, return_sequences=FALSE) %>%
  layer_dropout(rate=0.2)

### DEF Embedding Layers for Current Categorical Inputs ----
emb_pc <- inp_pc %>%
  layer_embedding(input_dim = nP + 1, output_dim = emb_dim, mask_zero = TRUE) %>%
  layer_flatten()
emb_tc <- inp_tc %>%
  layer_embedding(input_dim = nT + 1, output_dim = emb_dim, mask_zero = TRUE) %>%
  layer_flatten()
emb_oc <- inp_oc %>%
  layer_embedding(input_dim = nO + 1, output_dim = emb_dim, mask_zero = TRUE) %>%
  layer_flatten()
emb_hc <- inp_hc %>%
  layer_lambda(function(x) tf$cast(x, tf$float32))

### DEF Concatenate Current Categorical Embeddings ----
cat_cur_vec <- layer_concatenate(list(emb_pc,emb_tc,emb_oc,emb_hc), axis=-1)

### DEF Merge LSTM Output and Current Categorical Features ----
merged <- layer_concatenate(list(lstm_out, cat_cur_vec), axis=-1)
head   <- merged %>% layer_dense(units=32, activation="relu")
out    <- head   %>% layer_dense(units=1,  activation="linear")

### DEF Build and Compile Model ----
model <- keras_model(
  inputs  = list(inp_num, inp_ph, inp_th, inp_oh, inp_hh,
                 inp_pc, inp_tc, inp_oc, inp_hc),
  outputs = out,
  name = "def_lstm_model"
)

model %>% compile(
  optimizer = optimizer_adam(),
  loss      = "mse",
  metrics   = metric_root_mean_squared_error()
)

### DEF Model Summary and Visualization ----
summary(model)
plot(model)

### DEF Save Model Plot to File ----
keras_python <- import("keras")
keras_python$utils$plot_model(
  model,
  to_file = "keras-model-def.png",
  show_shapes = TRUE,
  show_dtype = FALSE,
  show_layer_names = TRUE,
  rankdir = "TB",
  expand_nested = FALSE,
  dpi = 200,
  show_layer_activations = FALSE
)

### DEF Display Model Plot in R ----
library(png)
img <- readPNG("keras-model-def.png")
grid::grid.raster(img)

## DEF 2.7: Callback Functions and Scaling Factors ----

# Filepath for saving the BEST weights (unique for each position)
weights_filepath_def <- "R Forecast/best_def.hdf5"
cat("Will save best DEF weights to:", weights_filepath_def, "\n")

# 1. Tidlig stopp funksjon, laster inn siste beste loss value.
# patience: how many epochs to wait after last improvement before stopping
# restore_best_weights: load the weights from the epoch with the best val_loss
early_stopping <- callback_early_stopping(
  monitor = "val_loss",
  patience = num_patience, 
  restore_best_weights = TRUE
)

# 2. Model Checkpoint Callback (to save best weights)
model_checkpoint <- callback_model_checkpoint(
  filepath = weights_filepath_def, # Path defined above
  monitor = "val_loss",           # Monitor the same metric as early stopping
  save_best_only = TRUE,          # IMPORTANT: Only save when val_loss improves
  save_weights_only = TRUE,       # IMPORTANT: Save ONLY the weights, not the whole model
  mode = "min",                   # We want to minimize loss
  verbose = 1                     # Optional: print message when weights are saved
)

# 3. Reduce Learning Rate on Plateau Callback
reduce_lr <- callback_reduce_lr_on_plateau(
  monitor = "val_loss",  # Monitor validation loss (same as early stopping)
  factor = 0.2,          # Reduce LR to 20% of its current value (lr * factor)
  patience = num_patience,           # Reduce LR if val_loss doesn't improve for 5 epochs
  min_lr = 1e-6,         # Don't reduce the LR below this value
  verbose = 1            # Print a message when LR is reduced
)

scaling_factors$def <- list(mu = mu,
                            sigma = sigma,
                            numF = numF) # numF here is the one selected by Lasso for DEF
cat("DEF scaling factors stored.\n")

cat("--- Checking DEF data before fit() ---\n")
cat("Xnum_train (DEF) - NAs:", sum(is.na(Xnum_train)), " Infs:", sum(is.infinite(Xnum_train)), " Dims:", dim(Xnum_train), "\n")
cat("y_train (DEF) - NAs:", sum(is.na(y_train)), " Infs:", sum(is.infinite(y_train)), " Dims:", dim(y_train), "\n")
cat("Xnum_val (DEF) - NAs:", sum(is.na(Xnum_val)), " Infs:", sum(is.infinite(Xnum_val)), " Dims:", dim(Xnum_val), "\n")
cat("y_val (DEF) - NAs:", sum(is.na(y_val)), " Infs:", sum(is.infinite(y_val)), " Dims:", dim(y_val), "\n")
# Optional: Check categorical arrays too if you suspect issues there
# cat("Xcat_hist_tr$pID_hist (DEF) - NAs:", sum(is.na(Xcat_hist_tr$pID_hist)), "\n")
# cat("Xcat_cur_tr$pID_cur (DEF) - NAs:", sum(is.na(Xcat_cur_tr$pID_cur)), "\n")
cat("DEF mu:", scaling_factors$def$mu, " sigma:", scaling_factors$def$sigma, "\n") # Check scaling factors used
cat("--------------------------------------\n")

# Add this check too, inside section 2.5 after creating y_val
cat("DEF y_val summary (scaled):", summary(y_val), "\n")
cat("DEF y_train summary (scaled):", summary(y_train), "\n")
## DEF 2.8: Train Defender LSTM Model ----

cat("Starting DEF model training...\n")

history <- model %>% fit(
  x = c(
    list(input_seq = Xnum_train),
    Xcat_hist_tr,
    Xcat_cur_tr
  ),
  y = y_train,
  epochs = epoker,
  batch_size = 16,
  validation_data = list(
    c( # Ensure validation inputs are correctly listed here
      list(input_seq = Xnum_val),
      Xcat_hist_va,
      Xcat_cur_va
    ),
    y_val # Keep the validation target here
  ),
  callbacks = list(early_stopping, model_checkpoint, reduce_lr)
)
cat("DEF model training finished.\n")


## DEF 2.9: Generate Predictions & Invert Scaling on Validation Set ----
# Use the validation set for predictions
val_row_ids <- numW$row_id[-idx]

pred_all <- model %>% predict(
  c(
    list(input_seq = Xnum_val),
    Xcat_hist_va,
    Xcat_cur_va
  )
)

preds_def <- tibble(
  row_id                         = val_row_ids,
  predicted_total_points_scaled  = as.vector(pred_all),
  actual_total_points_scaled     = as.vector(y_val)
) %>%
  mutate(
    predicted_total_points = predicted_total_points_scaled * sigma + mu,
    actual_total_points    = actual_total_points_scaled    * sigma + mu
  ) %>%
  left_join(unscaled_def, by = "row_id")

glimpse(preds_def)
str(preds_def)

# Build a predictions table using the validation row IDs
preds_def <- tibble(
  row_id = val_row_ids,  # row IDs corresponding to the validation set
  predicted_total_points_scaled = as.vector(pred_all),
  actual_total_points_scaled = as.vector(y_val)
) %>% 
  mutate(
    predicted_total_points = predicted_total_points_scaled * sigma + mu,
    actual_total_points = actual_total_points_scaled * sigma + mu
  )


preds_def <- preds_def %>%
  left_join(unscaled_def, by = "row_id") # Use unscaled_def

# View the predictions table
glimpse(preds_def)

## DEF 2.10 Save model and validation set predictions ----
# Store DEF model and scaling factors 
model_list$def <- model
cat("DEF model stored in model_list$def.\n")

# Clean up validation predictions (select essential columns)
preds_def_clean <- preds_def %>%
  select(row_id, GW, player_id, name, position, team,
         actual_total_points, predicted_total_points, value)
cat("Validation predictions stored in preds_def_clean.\n")

## DEF 2.11: Plots for Evaluation ----

### DEF 2.11.1 Save training history ----
history_def <- history # class() "keras_training_history"
history_df_def <- data.frame(history_def)

### DEF 2.11.2 Training History Plot ----
plot_history_def <- plot(history_def)
plot_history_def;str(history_df_def)

### DEF 2.11.3 Actual vs Predicted Points Plot ----
plot_actual_predicted_def <- ggplot(preds_def, aes(x = actual_total_points, y = predicted_total_points)) +
  geom_point(alpha = 0.5) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Actual vs. Predicted Total Points (Validation Set)",
       x = "Actual Total Points",
       y = "Predicted Total Points") +
  facet_wrap(~ position) +
  theme_grey() +
  coord_cartesian(xlim = range(preds_def$actual_total_points, na.rm = TRUE),
                  ylim = range(preds_def$predicted_total_points, na.rm = TRUE))
plot_actual_predicted_def                  

### DEF 2.11.4: Residuals vs Predicted ----
preds_def_residuals <- preds_def %>%
  mutate(residual = actual_total_points - predicted_total_points)

plot_residuals_predicted_def <- ggplot(preds_def_residuals, aes(x = predicted_total_points, y = residual)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs. Predicted Total Points (Validation Set)",
       x = "Predicted Total Points",
       y = "Residual (Actual - Predicted)") +
  facet_wrap(~ position) +
  theme_grey()  
plot_residuals_predicted_def

### DEF 2.11.5: Distribution of Residuals ----
plot_distribution_residuals_def <- ggplot(preds_def_residuals, aes(x = residual)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "blue", alpha = 0.7) +
  geom_density(color = "red") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Distribution of Residuals (Validation Set)",
       x = "Residual (Actual - Predicted)",
       y = "Density") +
  facet_wrap(~ position) +
  theme_grey()
plot_distribution_residuals_def

### DEF 2.11.6 Return list containing loss value and metrics ----
# Evaluate model on validation data using consistent variable names
model_evaluation <- model |> evaluate(
  c(
    list(input_seq = Xnum_val),
    Xcat_hist_va,
    Xcat_cur_va
  ),
  y_val,
  verbose = 1
)
# Print the evaluation results
print(model_evaluation)

# Store validation metrics
validation_metrics$def <- model_evaluation
cat("Validation metrics stored in validation_metrics$def.\n")

# 3: Midfielders (MID) ----

## MID 3.1 Define Features, Target & Scale ----
#numF = numerical features, tar = target
numF <- c("goals_scored","assists", "creativity", "minutes", "goals_conceded",
          "bonus", "bps", "expected_assists", "expected_goal_involvements",
          "expected_goals", "ict_index", "own_goals", "red_cards",
          "threat", "transfers_in", "transfers_out", "yellow_cards",
          "expected_goals_conceded", "value",
          "selected", "transfers_balance", "starts", "influence",
          "clean_sheets"
)
tar <- "total_points"
initial_numF_mid <- numF
cat("Numeriske features:", numF, "\n")
cat("Target variabel:", tar, "\n")
cat("Ferdig\n")

mid <- mid %>%
  arrange(pID_idx, GW) %>%
  group_by(pID_idx) %>%
  ungroup()
# Compute and store the mean and standard deviation for the target variable

mu <- mean(mid[[tar]], na.rm = TRUE)
sigma <- sd(mid[[tar]], na.rm = TRUE)

# Scale numerical features and target
mid <- mid %>%
  mutate(across(all_of(numF), 
                ~ (.x - mean(.x, na.rm = TRUE)) / sd(.x, na.rm = TRUE)),
         !!tar := (.data[[tar]] - mu) / sigma)
cat("Chunk done")

## MID 3.2: Build rolling numerical windows ----

# Define window size (number of past gameweeks to use)
ws <- vindu

# Build numerical rolling windows (numW)
numW <- mid %>%
  group_by(pID_idx) %>%
  filter(n() > ws) %>%
  group_modify(~{
    W   <- slide(.x[, numF], .f=~as.matrix(.x),
                 .before=ws, .after=-1, .complete=TRUE)
    valid <- W[(ws+1):length(W)]
    tgt   <- .x[[tar]][(ws+1):nrow(.x)]
    rid   <- .x$row_id[(ws+1):nrow(.x)]
    tibble(window=valid, target=tgt, row_id=rid)
  }) %>% ungroup() %>%
  left_join(select(mid, row_id, GW), by="row_id")

cat("Chunk done\n")

## MID 3.3: Lasso Regression ----

# 1. Aggregate each rolling window by column means (for each sample)
Xl <- do.call(rbind, lapply(numW$window, colMeans, na.rm = TRUE))

# 2. Extract the target variable for each window
yl <- numW$target

# 3. Perform cross-validated Lasso regression to select lambda
cvfit <- cv.glmnet(Xl, yl, alpha = 1)

# 4. Get the lambda value that minimizes cross-validated error
minlambda <- cvfit$lambda.min

# 5. Fit Lasso regression with the selected lambda
lassofit <- glmnet(Xl, yl, alpha = 1, lambda = minlambda)

# 6. Extract the names of features with non-zero coefficients (selected by Lasso)
keepF <- rownames(as.matrix(coef(lassofit)))[coef(lassofit)[, 1] != 0] %>%
  setdiff("(Intercept)")
numF_mid <- keepF

# 7. Update numF to only include selected features
numF <- keepF

cat("Lasso selected features:", numF_mid,"Initial number of features:", initial_numF_mid, "\n",
    "Number of selected features:",length(numF_mid), "\n")

## MID 3.4.1: Rebuild Numeric Windows (numW) with Lasso selected variables ----
ws <- vindu
# Rebuild rolling numerical windows using only Lasso-selected features (numF)
# Why: After feature selection, 
# we want to ensure that only the most relevant variables are used for modeling, 
# which can improve model performance and reduce overfitting.
numW <- mid %>%
  group_by(pID_idx) %>%
  filter(n() > ws) %>%
  group_modify(~{
    W     <- slide(.x[, numF], .f=~as.matrix(.x),
                   .before=ws, .after=-1, .complete=TRUE)
    valid <- W[(ws+1):length(W)]
    tgt   <- .x[[tar]][(ws+1):nrow(.x)]
    rid   <- .x$row_id[(ws+1):nrow(.x)]
    tibble(window=valid, target=tgt, row_id=rid)
  }) %>% ungroup() %>%
  left_join(select(mid, row_id, GW), by="row_id")

cat("Chunk done\n")

### MID 3.4.2: Build & align categorical windows (by player_id!) ----

# Build rolling windows for categorical variables (pID_idx, tID_idx, oID_idx, hID) for each player
catW_all <- mid %>%
  group_by(player_id) %>%
  filter(n() > ws) %>%
  group_modify(~{
    # For each player, create rolling windows of categorical indices
    W <- slide(
      .x %>% select(pID_idx, tID_idx, oID_idx, hID),
      .f = ~as.matrix(.x), .before = ws, .after = -1, .complete = TRUE
    )
    # Keep only valid windows (those with enough history)
    valid <- W[(ws + 1):length(W)]
    # Get corresponding row_ids for alignment
    rid <- .x$row_id[(ws + 1):nrow(.x)]
    tibble(window = valid, row_id = rid)
  }) %>% ungroup()

# Align categorical windows to numeric windows using row_id
catW <- numW %>%
  select(row_id) %>%
  left_join(catW_all, by = "row_id")

### MID 3.4.3: To Arrays
# Number of samples (number of rolling windows)
nSamp <- nrow(numW)

# Convert list of rolling numerical windows to a 3D array: [samples, window size, num features]
num_array <- array(unlist(numW$window), dim = c(nSamp, ws, length(numF)))

# Extract targets (scaled total_points) as a column matrix
targets <- matrix(numW$target, ncol = 1)

# Extract historical categorical indices for each window (for possible use in sequence models)
pID_hist_array <- t(sapply(catW$window, function(m) m[, "pID_idx"]))
tID_hist_array <- t(sapply(catW$window, function(m) m[, "tID_idx"]))
oID_hist_array <- t(sapply(catW$window, function(m) m[, "oID_idx"]))
hID_hist_array <- t(sapply(catW$window, function(m) m[, "hID"]))

# Extract current (last in window) categorical values for each sample
cat_cur_mat <- do.call(rbind, lapply(catW$window, function(m) m[nrow(m), ]))
pID_cur_array <- matrix(cat_cur_mat[, "pID_idx"], ncol = 1)
tID_cur_array <- matrix(cat_cur_mat[, "tID_idx"], ncol = 1)
oID_cur_array <- matrix(cat_cur_mat[, "oID_idx"], ncol = 1)
hID_cur_array <- matrix(cat_cur_mat[, "hID"], ncol = 1)


## MID 3.5: Split Data into Training and Validation Sets ----
idx <- which(numW$GW <= split_gw)
Xnum_train <- num_array[idx,,,drop=FALSE]
Xnum_val <- num_array[-idx,,,drop=FALSE]
y_train <- targets[idx,,drop=FALSE]
y_val <- targets[-idx,,drop=FALSE]

Xcat_hist_tr <- list(
  pID_hist = pID_hist_array[idx,,drop=FALSE],
  tID_hist = tID_hist_array[idx,,drop=FALSE],
  oID_hist = oID_hist_array[idx,,drop=FALSE],
  hID_hist = hID_hist_array[idx,,drop=FALSE]
)
Xcat_hist_va <- list(
  pID_hist = pID_hist_array[-idx,,drop=FALSE],
  tID_hist = tID_hist_array[-idx,,drop=FALSE],
  oID_hist = oID_hist_array[-idx,,drop=FALSE],
  hID_hist = hID_hist_array[-idx,,drop=FALSE]
)

Xcat_cur_tr <- list(
  pID_cur = pID_cur_array[idx,,drop=FALSE],
  tID_cur = tID_cur_array[idx,,drop=FALSE],
  oID_cur = oID_cur_array[idx,,drop=FALSE],
  hID_cur = hID_cur_array[idx,,drop=FALSE]
)
Xcat_cur_va <- list(
  pID_cur = pID_cur_array[-idx,,drop=FALSE],
  tID_cur = tID_cur_array[-idx,,drop=FALSE],
  oID_cur = oID_cur_array[-idx,,drop=FALSE],
  hID_cur = hID_cur_array[-idx,,drop=FALSE]
)

cat("Chunk Done\n")

## MID 3.6: Building the Keras Neural Network Model ----
### MID Dimensions of input layers ----
# nP == Number of players, nT == Number of teams, nO == Number of opponent teams
nP <- max(mid$pID_idx, na.rm=TRUE)
nT <- max(mid$tID_idx, na.rm=TRUE)
nO <- max(mid$oID_idx, na.rm=TRUE)

### MID Input Layers ----
inp_num <- layer_input(shape=c(ws,length(numF)), name="input_seq")
inp_ph  <- layer_input(shape=c(ws), dtype="int32", name="pID_hist")
inp_th  <- layer_input(shape=c(ws), dtype="int32", name="tID_hist")
inp_oh  <- layer_input(shape=c(ws), dtype="int32", name="oID_hist")
inp_hh  <- layer_input(shape=c(ws), dtype="int32", name="hID_hist")
inp_pc  <- layer_input(shape=c(1),   dtype="int32", name="pID_cur")
inp_tc  <- layer_input(shape=c(1),   dtype="int32", name="tID_cur")
inp_oc  <- layer_input(shape=c(1),   dtype="int32", name="oID_cur")
inp_hc  <- layer_input(shape=c(1),   dtype="int32", name="hID_cur")

### MID Embedding Layers ----
emb_ph <- inp_ph %>%
  layer_embedding(input_dim = nP + 1, output_dim = emb_dim, mask_zero = TRUE)
emb_th <- inp_th %>%
  layer_embedding(input_dim = nT + 1, output_dim = emb_dim, mask_zero = TRUE)
emb_oh <- inp_oh %>%
  layer_embedding(input_dim = nO + 1, output_dim = emb_dim, mask_zero = TRUE)
emb_hh <- inp_hh %>%
  layer_lambda(function(x){
    x_f <- tf$cast(x, tf$float32)
    tf$expand_dims(x_f, axis = as.integer(-1))
  })

### MID Concatenate Historical Embeddings for LSTM Sequence ----
cat_hist_seq <- layer_concatenate(list(emb_ph,emb_th,emb_oh,emb_hh), axis=-1)

### MID LSTM Layer for Sequential Modeling ----
lstm_in  <- layer_concatenate(list(inp_num, cat_hist_seq), axis=-1)
lstm_out <- lstm_in %>%
  layer_lstm(units=64, return_sequences=FALSE) %>%
  layer_dropout(rate=0.2)

### MID Embedding Layers for Current Categorical Inputs ----
emb_pc <- inp_pc %>%
  layer_embedding(input_dim = nP + 1, output_dim = emb_dim, mask_zero = TRUE) %>%
  layer_flatten()
emb_tc <- inp_tc %>%
  layer_embedding(input_dim = nT + 1, output_dim = emb_dim, mask_zero = TRUE) %>%
  layer_flatten()
emb_oc <- inp_oc %>%
  layer_embedding(input_dim = nO + 1, output_dim = emb_dim, mask_zero = TRUE) %>%
  layer_flatten()
emb_hc <- inp_hc %>%
  layer_lambda(function(x) tf$cast(x, tf$float32))

### MID Concatenate Current Categorical Embeddings ----
cat_cur_vec <- layer_concatenate(list(emb_pc,emb_tc,emb_oc,emb_hc), axis=-1)

### MID Merge LSTM Output and Current Categorical Features ----
merged <- layer_concatenate(list(lstm_out, cat_cur_vec), axis=-1)
head   <- merged %>% layer_dense(units=32, activation="relu")
out    <- head   %>% layer_dense(units=1,  activation="linear")

### MID Build and Compile Model ----
model <- keras_model(
  inputs  = list(inp_num, inp_ph, inp_th, inp_oh, inp_hh,
                 inp_pc, inp_tc, inp_oc, inp_hc),
  outputs = out,
  name = "mid_lstm_model"
)

model %>% compile(
  optimizer = optimizer_adam(),
  loss      = "mse",
  metrics   = metric_root_mean_squared_error()
)

### MID Model Summary and Visualization ----
summary(model)
plot(model)

### MID Save Model Plot to File ----
keras_python <- import("keras")
keras_python$utils$plot_model(
  model,
  to_file = "keras-model-mid.png",
  show_shapes = TRUE,
  show_dtype = FALSE,
  show_layer_names = TRUE,
  rankdir = "TB",
  expand_nested = FALSE,
  dpi = 200,
  show_layer_activations = FALSE
)

### MID Display Model Plot in R ----
library(png)
img <- readPNG("keras-model-mid.png")
grid::grid.raster(img)

## MID 3.7: Callback Functions and Scaling Factors ----

# Filepath for saving the BEST weights (unique for each position)
weights_filepath_mid <- "R Forecast/best_mid.hdf5"
cat("Will save best MID weights to:", weights_filepath_mid, "\n")

# 1. Tidlig stopp funksjon, laster inn siste beste loss value.
# patience: how many epochs to wait after last improvement before stopping
# restore_best_weights: load the weights from the epoch with the best val_loss
early_stopping <- callback_early_stopping(
  monitor = "val_loss",
  patience = num_patience, 
  restore_best_weights = TRUE
)

# 2. Model Checkpoint Callback (to save best weights)
model_checkpoint <- callback_model_checkpoint(
  filepath = weights_filepath_mid, # Path midined above
  monitor = "val_loss",           # Monitor the same metric as early stopping
  save_best_only = TRUE,          # IMPORTANT: Only save when val_loss improves
  save_weights_only = TRUE,       # IMPORTANT: Save ONLY the weights, not the whole model
  mode = "min",                   # We want to minimize loss
  verbose = 1                     # Optional: print message when weights are saved
)

# 3. Reduce Learning Rate on Plateau Callback
reduce_lr <- callback_reduce_lr_on_plateau(
  monitor = "val_loss",  # Monitor validation loss (same as early stopping)
  factor = 0.2,          # Reduce LR to 20% of its current value (lr * factor)
  patience = num_patience,           # Reduce LR if val_loss doesn't improve for 5 epochs
  min_lr = 1e-6,         # Don't reduce the LR below this value
  verbose = 1            # Print a message when LR is reduced
)

scaling_factors$mid <- list(mu = mu,
                            sigma = sigma,
                            numF = numF) # numF here is the one selected by Lasso for MID
cat("MID scaling factors stored.\n")

## MID 3.8: Train Midfielders LSTM Model ----

cat("Starting MID model training...\n")

history <- model %>% fit(
  x = c(
    list(input_seq = Xnum_train),
    Xcat_hist_tr,
    Xcat_cur_tr
  ),
  y              = y_train,
  epochs         = epoker,
  batch_size     = 16,
  validation_data= list(
    c(
      list(input_seq = Xnum_val),
      Xcat_hist_va,
      Xcat_cur_va
    ),
    y_val
  ),
  callbacks = list(early_stopping, model_checkpoint, reduce_lr)
)
cat("MID model training finished.\n")


## MID 3.9: Generate Predictions & Invert Scaling on Validation Set ----
# Use the validation set for predictions
val_row_ids <- numW$row_id[-idx]

pred_all <- model %>% predict(
  c(
    list(input_seq = Xnum_val),
    Xcat_hist_va,
    Xcat_cur_va
  )
)

preds_mid <- tibble(
  row_id                         = val_row_ids,
  predicted_total_points_scaled  = as.vector(pred_all),
  actual_total_points_scaled     = as.vector(y_val)
) %>%
  mutate(
    predicted_total_points = predicted_total_points_scaled * sigma + mu,
    actual_total_points    = actual_total_points_scaled    * sigma + mu
  ) %>%
  left_join(unscaled_mid, by = "row_id")

glimpse(preds_mid)

# Build a predictions table using the validation row IDs
preds_mid <- tibble(
  row_id = val_row_ids,  # row IDs corresponding to the validation set
  predicted_total_points_scaled = as.vector(pred_all),
  actual_total_points_scaled = as.vector(y_val)
) %>% 
  mutate(
    predicted_total_points = predicted_total_points_scaled * sigma + mu,
    actual_total_points = actual_total_points_scaled * sigma + mu
  )


preds_mid <- preds_mid %>%
  left_join(unscaled_mid, by = "row_id") # Use unscaled_mid

# View the predictions table
glimpse(preds_mid)

## MID 3.10 Save model and validation set predictions ----
# Store MID model and scaling factors 
model_list$mid <- model
cat("MID model stored in model_list$mid.\n")

# Clean up validation predictions (select essential columns)
preds_mid_clean <- preds_mid %>%
  select(row_id, GW, player_id, name, position, team,
         actual_total_points, predicted_total_points, value)
cat("Validation predictions stored in preds_mid_clean.\n")

## MID 3.11: Plots for Evaluation ----

### MID 3.11.1 Save training history ----
history_mid <- history # class() "keras_training_history"
history_df_mid <- data.frame(history_mid)

### MID 3.11.2 Training History Plot ----
plot_history_mid <- plot(history_mid)
plot_history_mid;str(history_df_mid)

### MID 3.11.3 Actual vs Predicted Points Plot ----
plot_actual_predicted_mid <- ggplot(preds_mid, aes(x = actual_total_points, y = predicted_total_points)) +
  geom_point(alpha = 0.5) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Actual vs. Predicted Total Points (Validation Set)",
       x = "Actual Total Points",
       y = "Predicted Total Points") +
  facet_wrap(~ position) +
  theme_grey() +
  coord_cartesian(xlim = range(preds_mid$actual_total_points, na.rm = TRUE),
                  ylim = range(preds_mid$predicted_total_points, na.rm = TRUE))
plot_actual_predicted_mid                  

### MID 3.11.4: Residuals vs Predicted ----
preds_mid_residuals <- preds_mid %>%
  mutate(residual = actual_total_points - predicted_total_points)

plot_residuals_predicted_mid <- ggplot(preds_mid_residuals, aes(x = predicted_total_points, y = residual)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs. Predicted Total Points (Validation Set)",
       x = "Predicted Total Points",
       y = "Residual (Actual - Predicted)") +
  facet_wrap(~ position) +
  theme_grey()  
plot_residuals_predicted_mid

### MID 3.11.5: Distribution of Residuals ----
plot_distribution_residuals_mid <- ggplot(preds_mid_residuals, aes(x = residual)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "blue", alpha = 0.7) +
  geom_density(color = "red") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Distribution of Residuals (Validation Set)",
       x = "Residual (Actual - Predicted)",
       y = "Density") +
  facet_wrap(~ position) +
  theme_grey()
plot_distribution_residuals_mid

### MID 3.11.6 Return list containing loss value and metrics ----
# Evaluate model on validation data using consistent variable names
model_evaluation <- model |> evaluate(
  c(
    list(input_seq = Xnum_val),
    Xcat_hist_va,
    Xcat_cur_va
  ),
  y_val,
  verbose = 1
)
# Print the evaluation results
print(model_evaluation)

# Store validation metrics
validation_metrics$mid <- model_evaluation
cat("Validation metrics stored in validation_metrics$mid.\n")

# 4: Forwards (FWD) ----

## FWD 4.1 Define Features, Target & Scale ----
#numF = numerical features, tar = target
numF <- c("goals_scored","assists", "creativity", "minutes", "goals_conceded",
          "bonus", "bps", "expected_assists", "expected_goal_involvements",
          "expected_goals", "ict_index", "own_goals", "red_cards",
          "threat", "transfers_in", "transfers_out", "yellow_cards",
          "expected_goals_conceded", "value",
          "selected", "transfers_balance", "starts", "influence",
          "clean_sheets"
)
tar <- "total_points"

initial_numF_fwd <- numF

cat("Numeriske features:", numF, "\n")
cat("Target variabel:", tar, "\n")
cat("Ferdig\n")

fwd <- fwd %>%
  arrange(pID_idx, GW) %>%
  group_by(pID_idx) %>%
  ungroup()
# Compute and store the mean and standard deviation for the target variable

mu <- mean(fwd[[tar]], na.rm = TRUE)
sigma <- sd(fwd[[tar]], na.rm = TRUE)

# Scale numerical features and target
fwd <- fwd %>%
  mutate(across(all_of(numF), 
                ~ (.x - mean(.x, na.rm = TRUE)) / sd(.x, na.rm = TRUE)),
         !!tar := (.data[[tar]] - mu) / sigma)
cat("Chunk done")

## FWD 4.2: Build rolling numerical windows ----

# Define window size (number of past gameweeks to use)
ws <- vindu

# Build numerical rolling windows (numW)
numW <- fwd %>%
  group_by(pID_idx) %>%
  filter(n() > ws) %>%
  group_modify(~{
    W   <- slide(.x[, numF], .f=~as.matrix(.x),
                 .before=ws, .after=-1, .complete=TRUE)
    valid <- W[(ws+1):length(W)]
    tgt   <- .x[[tar]][(ws+1):nrow(.x)]
    rid   <- .x$row_id[(ws+1):nrow(.x)]
    tibble(window=valid, target=tgt, row_id=rid)
  }) %>% ungroup() %>%
  left_join(select(fwd, row_id, GW), by="row_id")

cat("Chunk done\n")

## FWD 4.3: Lasso Regression ----

# 1. Aggregate each rolling window by column means (for each sample)
Xl <- do.call(rbind, lapply(numW$window, colMeans, na.rm = TRUE))

# 2. Extract the target variable for each window
yl <- numW$target

# 3. Perform cross-validated Lasso regression to select lambda
cvfit <- cv.glmnet(Xl, yl, alpha = 1)

# 4. Get the lambda value that minimizes cross-validated error
minlambda <- cvfit$lambda.min

# 5. Fit Lasso regression with the selected lambda
lassofit <- glmnet(Xl, yl, alpha = 1, lambda = minlambda)

# 6. Extract the names of features with non-zero coefficients (selected by Lasso)
keepF <- rownames(as.matrix(coef(lassofit)))[coef(lassofit)[, 1] != 0] %>%
  setdiff("(Intercept)")
numF_fwd <- keepF
# 7. Update numF to only include selected features
numF <- keepF

cat("Lasso selected features:", numF_fwd,"Initial number of features:", initial_numF_fwd, "\n",
    "Number of selected features:",length(numF_fwd), "\n")

## FWD 4.4.1: Rebuild Numeric Windows (numW) with Lasso selected variables ----
ws <- vindu
# Rebuild rolling numerical windows using only Lasso-selected features (numF)
# Why: After feature selection, 
# we want to ensure that only the most relevant variables are used for modeling, 
# which can improve model performance and reduce overfitting.
numW <- fwd %>%
  group_by(pID_idx) %>%
  filter(n() > ws) %>%
  group_modify(~{
    W     <- slide(.x[, numF], .f=~as.matrix(.x),
                   .before=ws, .after=-1, .complete=TRUE)
    valid <- W[(ws+1):length(W)]
    tgt   <- .x[[tar]][(ws+1):nrow(.x)]
    rid   <- .x$row_id[(ws+1):nrow(.x)]
    tibble(window=valid, target=tgt, row_id=rid)
  }) %>% ungroup() %>%
  left_join(select(fwd, row_id, GW), by="row_id")

cat("Chunk done\n")

### FWD 4.4.2: Build & align categorical windows (by player_id!) ----

# Build rolling windows for categorical variables (pID_idx, tID_idx, oID_idx, hID) for each player
catW_all <- fwd %>%
  group_by(player_id) %>%
  filter(n() > ws) %>%
  group_modify(~{
    # For each player, create rolling windows of categorical indices
    W <- slide(
      .x %>% select(pID_idx, tID_idx, oID_idx, hID),
      .f = ~as.matrix(.x), .before = ws, .after = -1, .complete = TRUE
    )
    # Keep only valid windows (those with enough history)
    valid <- W[(ws + 1):length(W)]
    # Get corresponding row_ids for alignment
    rid <- .x$row_id[(ws + 1):nrow(.x)]
    tibble(window = valid, row_id = rid)
  }) %>% ungroup()

# Align categorical windows to numeric windows using row_id
catW <- numW %>%
  select(row_id) %>%
  left_join(catW_all, by = "row_id")

### FWD 4.4.3: To Arrays
# Number of samples (number of rolling windows)
nSamp <- nrow(numW)

# Convert list of rolling numerical windows to a 3D array: [samples, window size, num features]
num_array <- array(unlist(numW$window), dim = c(nSamp, ws, length(numF)))

# Extract targets (scaled total_points) as a column matrix
targets <- matrix(numW$target, ncol = 1)

# Extract historical categorical indices for each window (for possible use in sequence models)
pID_hist_array <- t(sapply(catW$window, function(m) m[, "pID_idx"]))
tID_hist_array <- t(sapply(catW$window, function(m) m[, "tID_idx"]))
oID_hist_array <- t(sapply(catW$window, function(m) m[, "oID_idx"]))
hID_hist_array <- t(sapply(catW$window, function(m) m[, "hID"]))

# Extract current (last in window) categorical values for each sample
cat_cur_mat <- do.call(rbind, lapply(catW$window, function(m) m[nrow(m), ]))
pID_cur_array <- matrix(cat_cur_mat[, "pID_idx"], ncol = 1)
tID_cur_array <- matrix(cat_cur_mat[, "tID_idx"], ncol = 1)
oID_cur_array <- matrix(cat_cur_mat[, "oID_idx"], ncol = 1)
hID_cur_array <- matrix(cat_cur_mat[, "hID"], ncol = 1)


## FWD 4.5: Split Data into Training and Validation Sets ----
idx <- which(numW$GW <= split_gw)
Xnum_train <- num_array[idx,,,drop=FALSE]
Xnum_val <- num_array[-idx,,,drop=FALSE]
y_train <- targets[idx,,drop=FALSE]
y_val <- targets[-idx,,drop=FALSE]

Xcat_hist_tr <- list(
  pID_hist = pID_hist_array[idx,,drop=FALSE],
  tID_hist = tID_hist_array[idx,,drop=FALSE],
  oID_hist = oID_hist_array[idx,,drop=FALSE],
  hID_hist = hID_hist_array[idx,,drop=FALSE]
)
Xcat_hist_va <- list(
  pID_hist = pID_hist_array[-idx,,drop=FALSE],
  tID_hist = tID_hist_array[-idx,,drop=FALSE],
  oID_hist = oID_hist_array[-idx,,drop=FALSE],
  hID_hist = hID_hist_array[-idx,,drop=FALSE]
)

Xcat_cur_tr <- list(
  pID_cur = pID_cur_array[idx,,drop=FALSE],
  tID_cur = tID_cur_array[idx,,drop=FALSE],
  oID_cur = oID_cur_array[idx,,drop=FALSE],
  hID_cur = hID_cur_array[idx,,drop=FALSE]
)
Xcat_cur_va <- list(
  pID_cur = pID_cur_array[-idx,,drop=FALSE],
  tID_cur = tID_cur_array[-idx,,drop=FALSE],
  oID_cur = oID_cur_array[-idx,,drop=FALSE],
  hID_cur = hID_cur_array[-idx,,drop=FALSE]
)

cat("Chunk Done\n")

## FWD 4.6: Building the Keras Neural Network Model ----
### FWD Dimensions of input layers ----
# nP == Number of players, nT == Number of teams, nO == Number of opponent teams
nP <- max(fwd$pID_idx, na.rm=TRUE)
nT <- max(fwd$tID_idx, na.rm=TRUE)
nO <- max(fwd$oID_idx, na.rm=TRUE)

### FWD Input Layers ----
inp_num <- layer_input(shape=c(ws,length(numF)), name="input_seq")
inp_ph  <- layer_input(shape=c(ws), dtype="int32", name="pID_hist")
inp_th  <- layer_input(shape=c(ws), dtype="int32", name="tID_hist")
inp_oh  <- layer_input(shape=c(ws), dtype="int32", name="oID_hist")
inp_hh  <- layer_input(shape=c(ws), dtype="int32", name="hID_hist")
inp_pc  <- layer_input(shape=c(1),   dtype="int32", name="pID_cur")
inp_tc  <- layer_input(shape=c(1),   dtype="int32", name="tID_cur")
inp_oc  <- layer_input(shape=c(1),   dtype="int32", name="oID_cur")
inp_hc  <- layer_input(shape=c(1),   dtype="int32", name="hID_cur")

### FWD Embedding Layers ----
emb_ph <- inp_ph %>%
  layer_embedding(input_dim = nP + 1, output_dim = emb_dim, mask_zero = TRUE)
emb_th <- inp_th %>%
  layer_embedding(input_dim = nT + 1, output_dim = emb_dim, mask_zero = TRUE)
emb_oh <- inp_oh %>%
  layer_embedding(input_dim = nO + 1, output_dim = emb_dim, mask_zero = TRUE)
emb_hh <- inp_hh %>%
  layer_lambda(function(x){
    x_f <- tf$cast(x, tf$float32)
    tf$expand_dims(x_f, axis = as.integer(-1))
  })

### FWD Concatenate Historical Embeddings for LSTM Sequence ----
cat_hist_seq <- layer_concatenate(list(emb_ph,emb_th,emb_oh,emb_hh), axis=-1)

### FWD LSTM Layer for Sequential Modeling ----
lstm_in  <- layer_concatenate(list(inp_num, cat_hist_seq), axis=-1)
lstm_out <- lstm_in %>%
  layer_lstm(units=64, return_sequences=FALSE) %>%
  layer_dropout(rate=0.2)

### FWD Embedding Layers for Current Categorical Inputs ----
emb_pc <- inp_pc %>%
  layer_embedding(input_dim = nP + 1, output_dim = emb_dim, mask_zero = TRUE) %>%
  layer_flatten()
emb_tc <- inp_tc %>%
  layer_embedding(input_dim = nT + 1, output_dim = emb_dim, mask_zero = TRUE) %>%
  layer_flatten()
emb_oc <- inp_oc %>%
  layer_embedding(input_dim = nO + 1, output_dim = emb_dim, mask_zero = TRUE) %>%
  layer_flatten()
emb_hc <- inp_hc %>%
  layer_lambda(function(x) tf$cast(x, tf$float32))

### FWD Concatenate Current Categorical Embeddings ----
cat_cur_vec <- layer_concatenate(list(emb_pc,emb_tc,emb_oc,emb_hc), axis=-1)

### FWD Merge LSTM Output and Current Categorical Features ----
merged <- layer_concatenate(list(lstm_out, cat_cur_vec), axis=-1)
head   <- merged %>% layer_dense(units=32, activation="relu")
out    <- head   %>% layer_dense(units=1,  activation="linear")

### FWD Build and Compile Model ----
model <- keras_model(
  inputs  = list(inp_num, inp_ph, inp_th, inp_oh, inp_hh,
                 inp_pc, inp_tc, inp_oc, inp_hc),
  outputs = out,
  name = "fwd_lstm_model"
)

model %>% compile(
  optimizer = optimizer_adam(),
  loss      = "mse",
  metrics   = metric_root_mean_squared_error()
)

### FWD Model Summary and Visualization ----
summary(model)
plot(model)

### FWD Save Model Plot to File ----
keras_python <- import("keras")
keras_python$utils$plot_model(
  model,
  to_file = "keras-model-fwd.png",
  show_shapes = TRUE,
  show_dtype = FALSE,
  show_layer_names = TRUE,
  rankdir = "TB",
  expand_nested = FALSE,
  dpi = 200,
  show_layer_activations = FALSE
)

### FWD Display Model Plot in R ----
library(png)
img <- readPNG("keras-model-fwd.png")
grid::grid.raster(img)

## FWD 4.7: Callback Functions and Scaling Factors ----

# Filepath for saving the BEST weights (unique for each position)
weights_filepath_fwd <- "R Forecast/best_fwd.hdf5"
cat("Will save best FWD weights to:", weights_filepath_fwd, "\n")

# 1. Tidlig stopp funksjon, laster inn siste beste loss value.
# patience: how many epochs to wait after last improvement before stopping
# restore_best_weights: load the weights from the epoch with the best val_loss
early_stopping <- callback_early_stopping(
  monitor = "val_loss",
  patience = num_patience, 
  restore_best_weights = TRUE
)

# 2. Model Checkpoint Callback (to save best weights)
model_checkpoint <- callback_model_checkpoint(
  filepath = weights_filepath_fwd, # Path fwdined above
  monitor = "val_loss",           # Monitor the same metric as early stopping
  save_best_only = TRUE,          # IMPORTANT: Only save when val_loss improves
  save_weights_only = TRUE,       # IMPORTANT: Save ONLY the weights, not the whole model
  mode = "min",                   # We want to minimize loss
  verbose = 1                     # Optional: print message when weights are saved
)

# 3. Reduce Learning Rate on Plateau Callback
reduce_lr <- callback_reduce_lr_on_plateau(
  monitor = "val_loss",  # Monitor validation loss (same as early stopping)
  factor = 0.2,          # Reduce LR to 20% of its current value (lr * factor)
  patience = num_patience,           # Reduce LR if val_loss doesn't improve for 5 epochs
  min_lr = 1e-6,         # Don't reduce the LR below this value
  verbose = 1            # Print a message when LR is reduced
)

scaling_factors$fwd <- list(mu = mu,
                            sigma = sigma,
                            numF = numF) # numF here is the one selected by Lasso for FWD
cat("FWD scaling factors stored.\n")

## FWD 4.8: Train Forwards LSTM Model ----

cat("Starting FWD model training...\n")

history <- model %>% fit(
  x = c(
    list(input_seq = Xnum_train),
    Xcat_hist_tr,
    Xcat_cur_tr
  ),
  y              = y_train,
  epochs         = epoker,
  batch_size     = 16,
  validation_data= list(
    c(
      list(input_seq = Xnum_val),
      Xcat_hist_va,
      Xcat_cur_va
    ),
    y_val
  ),
  callbacks = list(early_stopping, model_checkpoint, reduce_lr)
)
cat("FWD model training finished.\n")


## FWD 4.9: Generate Predictions & Invert Scaling on Validation Set ----
# Use the validation set for predictions
val_row_ids <- numW$row_id[-idx]

pred_all <- model %>% predict(
  c(
    list(input_seq = Xnum_val),
    Xcat_hist_va,
    Xcat_cur_va
  )
)

preds_fwd <- tibble(
  row_id                         = val_row_ids,
  predicted_total_points_scaled  = as.vector(pred_all),
  actual_total_points_scaled     = as.vector(y_val)
) %>%
  mutate(
    predicted_total_points = predicted_total_points_scaled * sigma + mu,
    actual_total_points    = actual_total_points_scaled    * sigma + mu
  ) %>%
  left_join(unscaled_fwd, by = "row_id")

glimpse(preds_fwd)

# Build a predictions table using the validation row IDs
preds_fwd <- tibble(
  row_id = val_row_ids,  # row IDs corresponding to the validation set
  predicted_total_points_scaled = as.vector(pred_all),
  actual_total_points_scaled = as.vector(y_val)
) %>% 
  mutate(
    predicted_total_points = predicted_total_points_scaled * sigma + mu,
    actual_total_points = actual_total_points_scaled * sigma + mu
  )


preds_fwd <- preds_fwd %>%
  left_join(unscaled_fwd, by = "row_id") # Use unscaled_fwd

# View the predictions table
glimpse(preds_fwd)

## FWD 4.10 Save model and validation set predictions ----
# Store FWD model and scaling factors 
model_list$fwd <- model
cat("FWD model stored in model_list$fwd.\n")

# Clean up validation predictions (select essential columns)
preds_fwd_clean <- preds_fwd %>%
  select(row_id, GW, player_id, name, position, team,
         actual_total_points, predicted_total_points, value)
cat("Validation predictions stored in preds_fwd_clean.\n")

## FWD 4.11: Plots for Evaluation ----

### FWD 4.11.1 Save training history ----
history_fwd <- history # class() "keras_training_history"
history_df_fwd <- data.frame(history_fwd)

### FWD 4.11.2 Training History Plot ----
plot_history_fwd <- plot(history_fwd)
plot_history_fwd;str(history_df_fwd)

### FWD 4.11.3 Actual vs Predicted Points Plot ----
plot_actual_predicted_fwd <- ggplot(preds_fwd, aes(x = actual_total_points, y = predicted_total_points)) +
  geom_point(alpha = 0.5) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Actual vs. Predicted Total Points (Validation Set)",
       x = "Actual Total Points",
       y = "Predicted Total Points") +
  facet_wrap(~ position) +
  theme_grey() +
  coord_cartesian(xlim = range(preds_fwd$actual_total_points, na.rm = TRUE),
                  ylim = range(preds_fwd$predicted_total_points, na.rm = TRUE))
plot_actual_predicted_fwd                  

### FWD 4.11.4: Residuals vs Predicted ----
preds_fwd_residuals <- preds_fwd %>%
  mutate(residual = actual_total_points - predicted_total_points)

plot_residuals_predicted_fwd <- ggplot(preds_fwd_residuals, aes(x = predicted_total_points, y = residual)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs. Predicted Total Points (Validation Set)",
       x = "Predicted Total Points",
       y = "Residual (Actual - Predicted)") +
  facet_wrap(~ position) +
  theme_grey()  
plot_residuals_predicted_fwd

### FWD 4.11.5: Distribution of Residuals ----
plot_distribution_residuals_fwd <- ggplot(preds_fwd_residuals, aes(x = residual)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "blue", alpha = 0.7) +
  geom_density(color = "red") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Distribution of Residuals (Validation Set)",
       x = "Residual (Actual - Predicted)",
       y = "Density") +
  facet_wrap(~ position) +
  theme_grey()
plot_distribution_residuals_fwd

### FWD 4.11.6 Return list containing loss value and metrics ----
# Evaluate model on validation data using consistent variable names
model_evaluation <- model |> evaluate(
  c(
    list(input_seq = Xnum_val),
    Xcat_hist_va,
    Xcat_cur_va
  ),
  y_val,
  verbose = 1
)
# Print the evaluation results
print(model_evaluation)

# Store validation metrics
validation_metrics$fwd <- model_evaluation
cat("Validation metrics stored in validation_metrics$fwd.\n")

# Merging Datasets ----
glimpse(preds_gk)
glimpse(preds_def)
glimpse(preds_mid)
glimpse(preds_fwd)

# filepath: c:\Users\peram\Documents\test\R Forecast\prognose-modell.ipynb
# Combine the CLEANED validation predictions
validation_results_df <- bind_rows(
  preds_gk_clean,
  preds_def_clean,
  preds_mid_clean,
  preds_fwd_clean
) %>%
  arrange(player_id, GW)

glimpse(validation_results_df)

# Keep the original detailed forecast df as well (optional)
forecastdf_detailed <- bind_rows(preds_gk, preds_def, preds_mid, preds_fwd)
cat("Clean and detailed validation results combined.\n")

## Evaluation Metrics Dataframe ----
# Create metrics comparison dataframe
metrics_df <- data.frame(
  metric = names(validation_metrics$gk),
  GK = unlist(validation_metrics$gk),
  DEF = unlist(validation_metrics$def),
  MID = unlist(validation_metrics$mid),
  FWD = unlist(validation_metrics$fwd)
)

metrics_df
# Save plots from 1.11.n, 2.11.n, 3.11.n, 4.11.n to "C:/Users/peram/Documents/test/R Forecast/Plots from LSTM Forecast notebook"

# GK plots
ggsave("C:/Users/peram/Documents/test/R Forecast/Plots from LSTM Forecast notebook/plot_history_gk.png", plot = plot_history_gk, width = 7, height = 5)
ggsave("C:/Users/peram/Documents/test/R Forecast/Plots from LSTM Forecast notebook/plot_actual_predicted_gk.png", plot = plot_actual_predicted_gk, width = 7, height = 5)
ggsave("C:/Users/peram/Documents/test/R Forecast/Plots from LSTM Forecast notebook/plot_residuals_predicted_gk.png", plot = plot_residuals_predicted_gk, width = 7, height = 5)
ggsave("C:/Users/peram/Documents/test/R Forecast/Plots from LSTM Forecast notebook/plot_distribution_residuals_gk.png", plot = plot_distribution_residuals_gk, width = 7, height = 5)

# DEF plots
ggsave("C:/Users/peram/Documents/test/R Forecast/Plots from LSTM Forecast notebook/plot_history_def.png", plot = plot_history_def, width = 7, height = 5)
ggsave("C:/Users/peram/Documents/test/R Forecast/Plots from LSTM Forecast notebook/plot_actual_predicted_def.png", plot = plot_actual_predicted_def, width = 7, height = 5)
ggsave("C:/Users/peram/Documents/test/R Forecast/Plots from LSTM Forecast notebook/plot_residuals_predicted_def.png", plot = plot_residuals_predicted_def, width = 7, height = 5)
ggsave("C:/Users/peram/Documents/test/R Forecast/Plots from LSTM Forecast notebook/plot_distribution_residuals_def.png", plot = plot_distribution_residuals_def, width = 7, height = 5)

# MID plots
ggsave("C:/Users/peram/Documents/test/R Forecast/Plots from LSTM Forecast notebook/plot_history_mid.png", plot = plot_history_mid, width = 7, height = 5)
ggsave("C:/Users/peram/Documents/test/R Forecast/Plots from LSTM Forecast notebook/plot_actual_predicted_mid.png", plot = plot_actual_predicted_mid, width = 7, height = 5)
ggsave("C:/Users/peram/Documents/test/R Forecast/Plots from LSTM Forecast notebook/plot_residuals_predicted_mid.png", plot = plot_residuals_predicted_mid, width = 7, height = 5)
ggsave("C:/Users/peram/Documents/test/R Forecast/Plots from LSTM Forecast notebook/plot_distribution_residuals_mid.png", plot = plot_distribution_residuals_mid, width = 7, height = 5)

# FWD plots
ggsave("C:/Users/peram/Documents/test/R Forecast/Plots from LSTM Forecast notebook/plot_history_fwd.png", plot = plot_history_fwd, width = 7, height = 5)
ggsave("C:/Users/peram/Documents/test/R Forecast/Plots from LSTM Forecast notebook/plot_actual_predicted_fwd.png", plot = plot_actual_predicted_fwd, width = 7, height = 5)
ggsave("C:/Users/peram/Documents/test/R Forecast/Plots from LSTM Forecast notebook/plot_residuals_predicted_fwd.png", plot = plot_residuals_predicted_fwd, width = 7, height = 5)
ggsave("C:/Users/peram/Documents/test/R Forecast/Plots from LSTM Forecast notebook/plot_distribution_residuals_fwd.png", plot = plot_distribution_residuals_fwd, width = 7, height = 5)


## Export Predicted Validation Set as .CSV ----
# Export CLEAN validation results
write_csv(validation_results_df, "Validation_Predictions_Clean.csv")
cat("Clean validation predictions saved to Validation_Predictions_Clean.csv\n")

# Export DETAILED validation results (original behavior, renamed file)
write_csv(forecastdf_detailed, "Validation_Predictions_Detailed.csv")
cat("Detailed validation predictions saved to Validation_Predictions_Detailed.csv\n")

