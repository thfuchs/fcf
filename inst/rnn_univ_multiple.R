### Introduction ---------------------------------------------------------------

# Univariate Forecast of "Apple" FCF using LSTM.
# Instead of one-step-ahead forecasts, we use 4 timesteps to predict the next
# 4 data points simultaneously (and thus using 4 hidden states in the RNN).

# The data are pre-processed in the sense of normalization and transformation
# from data.table objects to 3D arrays in the format
# (sample, timesteps, features).

# Source: https://www.business-science.io/timeseries-analysis/2018/07/01/keras-lstm-sunspots-part2.html

library(data.table)
library(keras)
set.seed(123)

### Data Preparation -----------------------------------------------------------

apple <- fcf::dow30[
  ticker == "AAPL" & date > as.POSIXct("1990-01-01"),
  .SD, .SDcols = c("date", "ebit")
]
data.table::setnames(apple, c("date", "ebit"), c("index", "value"))

# Input variables
n_timesteps <- 4
n_predictions <- n_timesteps
length_val <- 16
length_test <- 8

# Normalizing the data
n <- nrow(apple) - length_val - length_test
train <- apple[1:n]

mean <- mean(train$value)
std <- sd(train$value)

data <- data.table::data.table(
  index = apple$index,
  value = scale(apple$value, center = mean, scale = std)[,1]
)

n <- nrow(data)
n_train <- 1:(n - (length_val + length_test))
n_val <- (n - (length_val + length_test)):(n - length_test)
n_val <- n_val[-1]
n_test <- (n - length_test):n
n_test <- n_test[-1]

### Reshaping the data

# functions used
build_matrix <- function(tseries, overall_timesteps) {
  t(sapply(1:(length(tseries) - overall_timesteps + 1), function(x)
    tseries[x:(x + overall_timesteps - 1)]))
}

reshape_X_3d <- function(X) {
  as.array(X)
  dim(X) <- c(dim(X)[1], dim(X)[2], 1)
  X
}

train_vals <- data[n_train, value]
val_vals <- data[n_val, value]
test_vals <- data[n_test, value]

# build the windowed matrices
train_matrix <- build_matrix(train_vals, n_timesteps + n_predictions)
valid_matrix <- build_matrix(val_vals, n_timesteps + n_predictions)
test_matrix <- build_matrix(test_vals, n_timesteps + n_predictions)

# separate matrices into training and testing parts
X_train <- train_matrix[, 1:n_timesteps, drop = FALSE]
y_train <-
  train_matrix[, (n_timesteps + 1):(n_timesteps + n_predictions), drop = FALSE]

X_valid <- valid_matrix[, 1:n_timesteps, drop = FALSE]
y_valid <-
  valid_matrix[, (n_timesteps + 1):(n_timesteps + n_predictions), drop = FALSE]

X_test <- test_matrix[, 1:n_timesteps, drop = FALSE]
y_test <-
  test_matrix[, (n_timesteps + 1):(n_timesteps + n_predictions), drop = FALSE]

# add on the required third axis
X_train <- reshape_X_3d(X_train)
X_valid <- reshape_X_3d(X_valid)
X_test <- reshape_X_3d(X_test)

y_train <- reshape_X_3d(y_train)
y_valid <- reshape_X_3d(y_valid)
y_test <- reshape_X_3d(y_test)


### LSTM -----------------------------------------------------------------------

# Hyperparameter Tuning with gridseacrch by tfruns
# Source: https://tensorflow.rstudio.com/tools/tfruns/overview/

FLAGS <- tfruns::flags(
  tfruns::flag_boolean("stateful", FALSE),
  tfruns::flag_boolean("stack_layers", FALSE),
  tfruns::flag_integer("n_timesteps", n_timesteps),
  tfruns::flag_integer("n_epochs", 200),
  tfruns::flag_numeric("dropout", 0.2),
  tfruns::flag_numeric("recurrent_dropout", 0.2),
  tfruns::flag_string("loss", "mae"),
  tfruns::flag_string("optimizer_type", "rmsprop"),
  tfruns::flag_integer("n_units", 32),
  tfruns::flag_numeric("lr", 0.003),
  # tfruns::flag_numeric("momentum", 0.9),
  tfruns::flag_integer("patience", 10)
)

# the number of predictions we'll make equals the length of the hidden state
n_predictions <- n_predictions
# how many features = predictors we have
n_features <- 1
# just in case we wanted to try different optimizers, we could add here
optimizer <- switch(
  FLAGS$optimizer_type,
  sgd = optimizer_sgd(lr = FLAGS$lr, momentum = FLAGS$momentum),
  rmsprop = optimizer_rmsprop()
)

# callbacks to be passed to the fit() function
callbacks <- list(
  callback_early_stopping(patience = FLAGS$patience)
)

# create the model
set.seed(120)

model <- keras_model_sequential() %>%
  layer_lstm(
    units = FLAGS$n_units,
    input_shape = c(FLAGS$n_timesteps, n_features),
    # batch_input_shape  = c(NULL, FLAGS$n_timesteps, n_features),
    dropout = FLAGS$dropout,
    recurrent_dropout = FLAGS$recurrent_dropout,
    # by default, an LSTM just returns the final state
    return_sequences = TRUE
  ) %>%
  time_distributed(layer_dense(units = 1))

model %>% compile(
  loss = FLAGS$loss,
  optimizer = optimizer,
  metrics = list("mse")
)

history <- model %>% fit(
  x               = X_train,
  y               = y_train,
  steps_per_epoch = 1,
  epochs          = FLAGS$n_epochs,
  batch_size      = NULL,
  verbose         = 1,
  shuffle         = FALSE,
  validation_data = list(X_valid, y_valid),
  callbacks       = callbacks
)

### Prediction and Evaluation --------------------------------------------------

# Evaluation
results$lstm$val_mae = history$metrics$val_loss * std

eval_lstm <- evaluate(model, X_test, y_test) * std
results$lstm$test_mae <- eval_lstm["loss"]

# Prediction and visualization
library(ggplot2)

pred_train <- model %>%
  predict(X_train) %>%
  .[, , 1]

# Retransform values to original scale
pred_train <- pred_train * std + mean
compare_train <- train

# build a dataframe that has both actual and predicted values
for (i in 1:nrow(pred_train)) {
  varname <- paste0("pred_train", i)
  compare_train <- dplyr::mutate(compare_train,!!varname := c(
    rep(NA, FLAGS$n_timesteps + i - 1),
    pred_train[i,],
    rep(NA, nrow(compare_train) - FLAGS$n_timesteps * 2 - i + 1)
  ))
}

pred_DT <- compare_train[, grep("^pred_train", names(compare_train)), with = FALSE]

timestep_seq <- 1:n_timesteps
names(timestep_seq) <- paste0("step", timestep_seq)
pred_stepwise_DT <- purrr::map_df(
  timestep_seq,
  function(x) {
    pred_seq <- seq(from = x, to = ncol(pred_DT), by = n_timesteps)
    data.table::fcoalesce(pred_DT[, pred_seq, with = FALSE])
  }
)

train_pred_DT <- cbind(
 compare_train[, c("index", "value")],
 train_pred = as.data.table(pred_stepwise_DT)[, MEAN := rowMeans(.SD, na.rm=T), .SDcols = paste0("step", timestep_seq)][, MEAN]
)

ggplot(train_pred_DT, aes(x = index, y = value)) +
  geom_line(color = "black") +
  geom_line(aes(y = train_pred), color = "blue")
