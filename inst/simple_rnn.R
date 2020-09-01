library(data.table)
library(keras)
set.seed(123)

### Data Preparation -----------------------------------------------------------

apple <- fcf::dow30[
  ticker == "AAPL" & date > as.POSIXct("1995-01-01"),
  .SD, .SDcols = c("date", "fcf")]
data.table::setnames(apple, c("date", "fcf"), c("index", "value"))

# Normalizing the data
n <- nrow(apple) - 24
train <- apple[1:n]

mean <- mean(train$value)
std <- sd(train$value)

apple_norm <- data.table::data.table(
  index = apple$index,
  value = scale(apple$value, center = mean, scale = std)[,1]
)


### Simple RNN -----------------------------------------------------------------

# Model inputs
lag_setting  <- 8
num_epochs <- 200
n_train <- 1:(nrow(apple_norm) - 24 - lag_setting)
n_val <- (last(n_train) + 1):(nrow(apple_norm) - 8 - lag_setting)
n_test <- (last(n_val) + 1):(nrow(apple_norm) - lag_setting)

results <- list()

# 2D And 3D Train/Test Arrays
apple_norm[, value_lag := shift(value, n = lag_setting, type = "lag")]
data <- apple_norm[!is.na(value_lag)]

train_DT <- data[n_train]
x_train_vec <- train_DT$value_lag
x_train_arr <- array(data = x_train_vec, dim = c(length(x_train_vec), 1, 1))

y_train_vec <- train_DT$value
y_train_arr <- array(data = y_train_vec, dim = c(length(y_train_vec), 1))

val_DT <- data[n_val]
x_val_vec <- val_DT$value_lag
x_val_arr <- array(data = x_val_vec, dim = c(length(x_val_vec), 1, 1))

y_val_vec <- val_DT$value
y_val_arr <- array(data = y_val_vec, dim = c(length(y_val_vec), 1))

test_DT <- data[n_test]
x_test_vec <- test_DT$value_lag
x_test_arr <- array(data = x_test_vec, dim = c(length(x_test_vec), 1, 1))

y_test_vec <- test_DT$value
y_test_arr <- array(data = y_test_vec, dim = c(length(y_test_vec), 1))

### A basic ML approach --------------------------------------------------------

model <- keras_model_sequential() %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 1)

model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mae",
  metrics = c("mse")
)

history <- model %>% keras::fit(
  x               = x_train_arr,
  y               = y_train_arr,
  steps_per_epoch = 1,
  epochs          = num_epochs,
  batch_size      = NULL,
  verbose         = 1,
  shuffle         = FALSE,
  validation_data = list(x_val_arr, y_val_arr)
)

results$basic$val_mae = history$metrics$val_loss
results$basic$test_mae <- mean(abs(predict(model, x_test_vec) - y_test_vec))

### A first recurrent baseline: GRU --------------------------------------------
keras::k_clear_session()

model <- keras_model_sequential() %>%
  layer_gru(units = 32, input_shape = c(1, 1)) %>%
  layer_dense(units = 1)

model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mae",
  metrics = c("mse")
)

history <- model %>% keras::fit(
  x               = x_train_arr,
  y               = y_train_arr,
  steps_per_epoch = 1,
  epochs          = num_epochs,
  batch_size      = NULL,
  verbose         = 1,
  shuffle         = FALSE,
  validation_data = list(x_val_arr, y_val_arr)
)

results$simple_gru$val_mae = history$metrics$val_loss
results$simple_gru$test_mae <- mean(abs(predict(model, x_test_arr) - y_test_vec))

### Simple LSTM ----------------------------------------------------------------
keras::k_clear_session()

model <- keras_model_sequential() %>%
  layer_lstm(units = 32, input_shape = c(1, 1)) %>%
  layer_dense(units = 1)

model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mae",
  metrics = c("mse")
)

history <- model %>% keras::fit(
  x               = x_train_arr,
  y               = y_train_arr,
  steps_per_epoch = 1,
  epochs          = num_epochs,
  batch_size      = NULL,
  verbose         = 1,
  shuffle         = FALSE,
  validation_data = list(x_val_arr, y_val_arr)
)

results$simple_lstm$val_mae = history$metrics$val_loss
results$simple_lstm$test_mae <- mean(abs(predict(model, x_test_arr) - y_test_vec))

### Using recurrent dropout with GRU -------------------------------------------
keras::k_clear_session()
set.seed(123)
model <- keras_model_sequential() %>%
  layer_gru(units = 32, input_shape = c(1, 1), dropout = 0.2, recurrent_dropout = 0.2) %>%
  layer_dense(units = 1)

model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mae",
  metrics = c("mse")
)

history <- model %>% keras::fit(
  x               = x_train_arr,
  y               = y_train_arr,
  steps_per_epoch = 1,
  epochs          = num_epochs,
  batch_size      = NULL,
  verbose         = 1,
  shuffle         = FALSE,
  validation_data = list(x_val_arr, y_val_arr)
)

results$gru_drop$val_mae <- history$metrics$val_loss
results$gru_drop$test_mae <- mean(abs(predict(model, x_test_arr) - y_test_vec))


### Save "results" -------------------------------------------------------------
# saveRDS(results, file = "inst/results/rnn_lag8.rds")

### Predicting Using The LSTM Model --------------------------------------------
# Make Predictions
pred_out <- model %>%
  predict(x_test_arr) %>%
  .[,1]

# Retransform values
pred_tbl <- data.table::data.table(
  index   = test_DT$index,
  value   = pred_out * std + mean
)

# Combine actual data with predictions
tbl_1 <- apple %>%
  tibble::add_column(key = "actual")

tbl_2 <- pred_tbl %>%
  tibble::add_column(key = "predict")

# Create time_bind_rows() to solve dplyr issue
ret <- rbind(tbl_1, tbl_2) %>%
  dplyr::arrange(key, index) %>%
  dplyr::mutate(key = forcats::as_factor(key))

# Assessing Performance Of The LSTM On A Single Split
calc_rmse(prediction_tbl = ret)

# Visualizing The Single Prediction
library(ggplot2)
plot_prediction <- function(data, alpha = 1, size = 2, base_size = 14) {

  g <- data %>%
    ggplot(aes(index, value, color = key)) +
    geom_point(alpha = alpha, size = size) +
    tidyquant::theme_tq(base_size = base_size) +
    tidyquant::scale_color_tq() +
    theme(legend.position = "none") +
    labs(
      x = "", y = ""
    )

  return(g)
}

ret %>%
  plot_prediction(alpha = 0.65) +
  theme(legend.position = "bottom")
