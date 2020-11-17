### Introduction ---------------------------------------------------------------

# Univariate Forecast of sampled (time series cross-validated) "Apple" FCF using
# NN (one basic) and RNN (GRU, LSTM).
# We do one-step-ahead forecasting by using 4 lags as input parameter to predict
# one label.
# The RNN works as follows: Take the 5th value (indexed by time) as label and
# predict with 4 lags. The output and next four lags are then used as
# input for the 6th value and on.

# The data are pre-processed in the sense of normalization and transformation
# from data.table objects to 3D arrays in the format
# (sample, timesteps, features).

# Sources: rnn_univ_lag.R and
# https://www.business-science.io/timeseries-analysis/2018/04/18/keras-lstm-sunspots-time-series-prediction.html

### Data Preparation -----------------------------------------------------------

apple <- fcf::DT_apple

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
  ~ purrr::map_df(.x$metrics, "loss") * .x$metrics$normalization$scale,
  .id = "split"
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

### RNN - GRU ------------------------------------------------------------------

### RNN - LSTM -----------------------------------------------------------------
