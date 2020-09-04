### Introduction ---------------------------------------------------------------

# Univariate Forecast of "Apple" FCF using NN (one basic) and RNN (GRU, LSTM).
# We do one-step-ahead forecasting by using 4 lags as input parameter to predict
# one label.
# The RNN works as follows: Take the 5th value (indexed by time) as label and
# predict this by its 4 lags. The output and next for lags are then used as
# input for the 6th value and on.

# The data are preprocessed in the sense of normalization and transformation
# from data.table objects to 3D arrays in the format
# (sample, timesteps, features).

# https://www.business-science.io/timeseries-analysis/2018/04/18/keras-lstm-sunspots-time-series-prediction.html

# library(data.table)
# library(keras)
# set.seed(123)

### Data Preparation -----------------------------------------------------------

# lag_setting <- 1:4
#
# # Normalizing the data
# n <- nrow(apple) - 24
# train <- apple[1:n]
#
# mean <- mean(train$value)
# std <- sd(train$value)
#
# apple_norm <- data.table::data.table(
#   index = apple$index,
#   value = scale(apple$value, center = mean, scale = std)[,1]
# )
#
# c(X, Y) %<-% ts_nn_preparation(
#   data = apple_norm,
#   lag_setting = lag_setting,
#   length_val = 16,
#   length_test = 8
# )
#
# # Model inputs
# num_epochs <- 200
# tsteps <- length(lag_setting)
# results <- list()

apple <- fcf::dow30[
  ticker == "AAPL" & date > as.POSIXct("1995-01-01"),
  .SD, .SDcols = c("date", "fcf")
]
data.table::setnames(apple, c("date", "fcf"), c("index", "value"))

# Cross Validation
periods_train <- 70
periods_test  <- 6
skip_span     <- 7

rolling_origin_resamples <- rsample::rolling_origin(
  apple,
  initial    = periods_train,
  assess     = periods_test,
  cumulative = FALSE,
  skip       = skip_span
)

rolling_origin_resamples


### A basic ML approach --------------------------------------------------------
# set.seed(150)
# basic <- purrr::map(
#   rolling_origin_resamples$splits,
#   function(split) {
#     DT_train <- rsample::analysis(split)[1:58]
#     DT_val <- rsample::analysis(split)[59:.N]
#     DT_test <- rsample::assessment(split)
#
#     DT <- rbind(DT_train, DT_val, DT_test)
#     predict_keras_sequential(
#       DT,
#       lag_setting = 1:4,
#       length_val = nrow(DT_val),
#       length_test = nrow(DT_test),
#       filepath = paste0(
#         system.file("models", package = "fcf"), "/basic_", split$id, ".hdf5")
#     )
#   }
# )
#
# saveRDS(basic, file = "inst/results/20200904_eval_pred_basicNN.rds")
basic <- readRDS("inst/results/20200904_eval_pred_basicNN.rds")

basic_mae <- purrr::map_df(
  basic,
  function(x) {
    df <- data.frame(
      train = x$metrics$train,
      val = x$metrics$val,
      test = x$metrics$test
    )
    df["loss",] * x$metrics$normalization$scale
  },
  .id = "id"
)
basic_pred <- purrr::map(basic, "predictions")

mean(basic_mae$test)
sd(basic_mae$test)

plot_prediction_samples(
  splits = basic_pred,
  title = "Basic Machine Learning Model",
  ncol = 2,
  scale = as.Date(c(min(apple$index), max(apple$index)))
)

###
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
  validation_data = list(X$val, Y$val),
  callbacks       = callback_early_stopping(patience = 10)
)

save_model_hdf5(
  model_basic, filepath = "inst/models/basic_relu_32.hdf5", overwrite = TRUE)
results$basic$val_mae <- history_basic$metrics$val_loss * std

eval_basic <- evaluate(model_basic, X$test, Y$test) * std
results$basic$test_mae <- eval_basic["loss"]

### A first recurrent baseline: GRU --------------------------------------------
keras::k_clear_session()

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
  validation_data = list(X$val, Y$val),
  callbacks       = callback_early_stopping(patience = 10)
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
  validation_data = list(X$val, Y$val),
  callbacks       = callback_early_stopping(patience = 10)
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
  validation_data = list(X$val, Y$val),
  callbacks       = callback_early_stopping(patience = 10)
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
