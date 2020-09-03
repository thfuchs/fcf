library(data.table)
library(keras)
set.seed(123)

### Data Preparation -----------------------------------------------------------

apple <- fcf::dow30[
  ticker == "AAPL" & date > as.POSIXct("1995-01-01"),
  .SD, .SDcols = c("date", "fcf")
]
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

c(X, Y) %<-% ts_nn_preparation(
  data = apple_norm,
  lag_setting = 8,
  length_val = 16,
  length_test = 8
)

# Model inputs
num_epochs <- 200
tsteps <- 1
results <- list()


### A basic ML approach --------------------------------------------------------
model_basic <- keras_model_sequential() %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 1)

model_basic %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mae",
  metrics = c("mse")
)

history_basic <- model_basic %>% keras::fit(
  x               = X$train,
  y               = Y$train,
  steps_per_epoch = 1,
  epochs          = num_epochs,
  batch_size      = NULL,
  verbose         = 1,
  shuffle         = FALSE,
  validation_data = list(X$val, Y$val)
)

save_model_hdf5(
  model_basic, filepath = "inst/models/basic_relu_32.hdf5", overwrite = TRUE)
results$basic$val_mae <- history_basic$metrics$val_loss * std

eval_basic <- evaluate(model_basic, X$test, Y$test) * std
results$basic$test_mae <- eval_basic["loss"]

### A first recurrent baseline: GRU --------------------------------------------
keras::k_clear_session()

set.seed(150)
model_gru <- keras_model_sequential() %>%
  layer_gru(units = 32, input_shape = c(tsteps, 1)) %>%
  layer_dense(units = 1)

model_gru %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mae",
  metrics = c("mse")
)

history_gru <- model_gru %>% keras::fit(
  x               = X$train,
  y               = Y$train,
  steps_per_epoch = 1,
  epochs          = num_epochs,
  batch_size      = NULL,
  verbose         = 1,
  shuffle         = FALSE,
  validation_data = list(X$val, Y$val)
)

save_model_hdf5(
  model_gru, filepath = "inst/models/gru_32.hdf5", overwrite = TRUE)
results$simple_gru$val_mae <- history_gru$metrics$val_loss * std

eval_gru <- evaluate(model_gru, X$test, Y$test) * std
results$simple_gru$test_mae <- eval_gru["loss"]

### Simple LSTM ----------------------------------------------------------------
keras::k_clear_session()

model_lstm <- keras_model_sequential() %>%
  layer_lstm(units = 32, input_shape = c(tsteps, 1)) %>%
  layer_dense(units = 1)

model_lstm %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mae",
  metrics = c("mse")
)

history_lstm <- model_lstm %>% keras::fit(
  x               = X$train,
  y               = Y$train,
  steps_per_epoch = 1,
  epochs          = num_epochs,
  batch_size      = NULL,
  verbose         = 1,
  shuffle         = FALSE,
  validation_data = list(X$val, Y$val)
)

save_model_hdf5(
  model_lstm, filepath = "inst/models/lstm_32.hdf5", overwrite = TRUE)
results$simple_lstm$val_mae = history_lstm$metrics$val_loss * std

eval_lstm <- evaluate(model_lstm, X$test, Y$test) * std
results$simple_lstm$test_mae <- eval_lstm["loss"]

### Using recurrent dropout with GRU -------------------------------------------
keras::k_clear_session()

model_gru_drop <- keras_model_sequential() %>%
  layer_gru(units = 32, input_shape = c(tsteps, 1),
            dropout = 0.2, recurrent_dropout = 0.2) %>%
  layer_dense(units = 1)

model_gru_drop %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mae",
  metrics = c("mse")
)

history_gru_drop <- model_gru_drop %>% keras::fit(
  x               = X$train,
  y               = Y$train,
  steps_per_epoch = 1,
  epochs          = num_epochs,
  batch_size      = NULL,
  verbose         = 1,
  shuffle         = FALSE,
  validation_data = list(X$val, Y$val)
)

save_model_hdf5(
  model_gru_drop, filepath = "inst/models/gru_drop_32.hdf5", overwrite = TRUE)
results$gru_drop$val_mae <- history_gru_drop$metrics$val_loss * std

eval_gru_drop <- evaluate(model_gru_drop, X$test, Y$test) * std
results$gru_drop$test_mae <- eval_gru_drop["loss"]

### Save "results" -------------------------------------------------------------
saveRDS(results, file = "inst/results/rnn.rds")

### Predicting Using The LSTM Model --------------------------------------------
# Make Predictions
# pred_out <- model %>%
#   predict(x_test_arr) %>%
#   .[,1]
#
# # Retransform values
# pred_tbl <- data.table::data.table(
#   index   = test_DT$index,
#   value   = pred_out * std + mean
# )
#
# # Combine actual data with predictions
# tbl_1 <- apple %>%
#   tibble::add_column(key = "actual")
#
# tbl_2 <- pred_tbl %>%
#   tibble::add_column(key = "predict")
#
# # Create time_bind_rows() to solve dplyr issue
# ret <- rbind(tbl_1, tbl_2) %>%
#   dplyr::arrange(key, index) %>%
#   dplyr::mutate(key = forcats::as_factor(key))
#
# # Assessing Performance Of The LSTM On A Single Split
# calc_rmse(prediction_tbl = ret)
#
# # Visualizing The Single Prediction
# library(ggplot2)
# plot_prediction <- function(data, alpha = 1, size = 2, base_size = 14) {
#
#   g <- data %>%
#     ggplot(aes(index, value, color = key)) +
#     geom_point(alpha = alpha, size = size) +
#     tidyquant::theme_tq(base_size = base_size) +
#     tidyquant::scale_color_tq() +
#     theme(legend.position = "none") +
#     labs(
#       x = "", y = ""
#     )
#
#   return(g)
# }
#
# ret %>%
#   plot_prediction(alpha = 0.65) +
#   theme(legend.position = "bottom")
