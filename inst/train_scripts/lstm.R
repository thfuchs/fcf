#' Tune a LSTM RNN on the Apple FCF time series and forecast the next year

# Initialization ---------------------------------------------------------------

apple <- fcf::DT_apple

tuning_grid <- list(
  lags = list(1, 1:2, 1:3, 1:4),
  optimizer = c("rmsprop", "adam"),
  n_epochs = 200,
  n_units = c(16, 32, 64),
  dropout = c(0.1, 0.2, 0.3)
)
cv_setting <- list(
  periods_train = 40,
  periods_val = 8,
  periods_test = 4,
  skip_span = 7
)

# Tuning -----------------------------------------------------------------------

# library(zeallot)
# c(results, min_params) %<-% tune_keras_sequential(
#   data = apple,
#   model_type = "lstm",
#   cv_setting = cv_setting,
#   tuning_grid = tuning_grid
# )
# save(results, min_params, file = "inst/results/20200911_tuning_lstm.rda")

load(file = "inst/results/20200909_tuning_basicNN.rda")

# Plot tuning results
library(ggplot2)
eval_DT <- results[[min_params$index]]$evaluation[type == "test",]

eval_DT %>%
  ggplot(aes(mse)) +
  geom_histogram(aes(y = ..density..), fill = "grey", bins = 12) +
  geom_density(alpha = 0.5, color = "darkgrey", fill = "grey") +
  theme_bw() +
  ggtitle("Histogram of Mean Squared Error (MSE)")

# Train cross validated data using best performing model -----------------------

# n_initial <- cv_setting$periods_train + cv_setting$periods_val
# rolling_origin_resamples <- rsample::rolling_origin(
#   apple,
#   initial = n_initial,
#   assess = cv_setting$periods_test,
#   cumulative = FALSE,
#   skip       = cv_setting$skip_span
# )
# basic <- purrr::map(
#   rolling_origin_resamples$splits,
#   function(split) {
#     DT_train <- rsample::analysis(split)[1:cv_setting$periods_train]
#     DT_val <- rsample::analysis(split)[(cv_setting$periods_train+1):.N]
#     DT_test <- rsample::assessment(split)
#
#     DT <- rbind(DT_train, DT_val, DT_test)
#     predict_keras_sequential(
#       DT,
#       lag_setting = min_params$lags,
#       length_val = cv_setting$periods_val,
#       length_test = cv_setting$periods_test,
#       model_type = "lstm",
#       n_units = min_params$n_units,
#       epochs = min_params$n_epochs,
#       optimizer_type = min_params$optimizer,
#       dropout = min_params$dropout,
#       recurrent_dropout = min_params$dropout,
#       patience = 20,
#       save_model = FALSE
#     )
#   }
# )
# saveRDS(basic, file = "inst/results/20200911_eval_pred_lstm.rds")

basic <- readRDS("inst/results/20200911_eval_pred_lstm.rds")

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
  title = "Long Short-Term Memory",
  ncol = 2,
  scale = as.Date(c(min(apple$index), max(apple$index)))
)

# Train data using best performing model ---------------------------------------

apple <- fcf::DT_apple

c(predictions, best_model_metrics) %<-% predict_keras_sequential(
  DT = apple,
  model_type = "lstm",
  n_units = 32,
  epochs = min_params$n_epochs,
  lag_setting = min_params$lags,
  length_val = 8,
  length_test = 0,
  optimizer_type = "rmsprop", #min_params$optimizer,
  dropout = 0.2,
  patience = 100,
  recurrent_dropout = 0.2,
  forecast_future = FALSE,
  save_model = FALSE #TRUE
  #filepath = "inst/models/best_gru.hdf5"
)
# save(predictions, best_model_metrics, file = "inst/results/20200909_best_basic.rda")

# load("inst/results/20200909_best_basic.rda")
# model <- keras::load_model_hdf5("inst/models/best_basic.hdf5")
# summary(model)

best_model_metrics

# Plot Prediction
plot_prediction(
  data = predictions[, index := as.Date(index)],
  title = "Basic Machine Learning Model Prediction"
)

# Train entire data for future forecast ----------------------------------------

# c(predictions_all, best_model_metrics_all) %<-% predict_keras_sequential(
#   DT = apple,
#   epochs = min_params$n_epochs,
#   lag_setting = min_params$lags,
#   length_val = 12,
#   length_test = 0,
#   optimizer_type = min_params$optimizer,
#   save_model = TRUE,
#   filepath = "inst/models/best_basic_all.hdf5",
#   forecast_future = TRUE,
#   forecast_length = 4
# )
# save(predictions_all, file = "inst/results/20200909_best_basic_all.rda")

load("inst/results/20200909_best_basic_all.rda")

plot_prediction(
  data = predictions_all[, index := as.Date(index)],
  title = "Basic Machine Learning Model 1 year Forecast"
)
