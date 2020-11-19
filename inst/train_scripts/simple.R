#' Tune a simple RNN on the UNH EBIT time series and forecast the next year

# Initialization ---------------------------------------------------------------

unh <- fcf::DT_unh

tuning_bounds <- list(
  lag_1 = c(1L, 4L),
  lag_2 = c(1L, 4L),
  n_units = c(8L, 32L),
  n_epochs = c(10L, 50L),
  optimizer_type = c(1L, 2L), # 1 = "rmsprop", 2 = "adam"
  dropout = c(0, 0.7),
  recurrent_dropout = c(0, 0.7),
  learning_rate = c(0.001, 0.01)
)

# tuning_grid <- list(
#   lags = list(1, 1:2, 1:3, 1:4),
#   optimizer = c("rmsprop", "adam"),
#   n_epochs = 200,
#   n_units = c(16, 32, 64),
#   dropout = c(0.1, 0.2, 0.3)
# )
cv_setting <- list(
  periods_train = 40,
  periods_val = 6,
  periods_test = 6,
  skip_span = 5
)
multiple_h <- list(short = 1, medium = 1:4, long = 5:6, total = 1:6)
frequency <- 4

# Tuning -----------------------------------------------------------------------

# library(zeallot)
# c(results, min_params) %<-% tune_keras_sequential(
#   data = unh,
#   model_type = "simple",
#   cv_setting = cv_setting,
#   tuning_bounds = tuning_bounds,
#   frequency = frequency,
#   multiple_h = multiple_h
# )
# save(results, min_params, file = "inst/results/20200915_tuning_simple.rda")

load(file = "inst/results/20201116_tuning_basicNN.rda")

# Plot tuning results
library(ggplot2)
eval_DT <- results[[min_params$index]][type == "test",]

ggplot(eval_DT, aes(mse)) +
  geom_histogram(aes(y = ..density..), fill = "grey", col = "white", bins = 12) +
  geom_density(alpha = 0.5, color = "darkgrey", fill = "grey") +
  theme_bw() +
  ggtitle("Histogram of Mean Squared Error (MSE)")

# Train cross validated data using best performing model -----------------------

n_initial <- cv_setting$periods_train + cv_setting$periods_val
rolling_origin_resamples <- rsample::rolling_origin(
  data,
  initial = n_initial,
  assess = cv_setting$periods_test,
  cumulative = FALSE,
  skip       = cv_setting$skip_span
)
basic <- purrr::map(
  rolling_origin_resamples$splits,
  function(split) {
    DT_train <- rsample::analysis(split)[1:cv_setting$periods_train]
    DT_val <- rsample::analysis(split)[(cv_setting$periods_train+1):.N]
    DT_test <- rsample::assessment(split)

    DT <- rbind(DT_train, DT_val, DT_test)

    # Forecast
    fc <- predict_keras_sequential(
      model_type = "simple",
      DT,
      lag_setting = min_params$lags,
      length_val = cv_setting$periods_val,
      length_test = cv_setting$periods_test,
      n_units = min_params$n_units,
      epochs = min_params$n_epochs,
      optimizer_type = min_params$optimizer,
      dropout = min_params$dropout,
      recurrent_dropout = min_params$dropout,
      save_model = FALSE
    )

    # Accuracy Measures
    acc <- evaluate_keras_sequential(
      DT,
      forecast = fc[key == "predict"],
      n_train = cv_setting$periods_train,
      n_val = cv_setting$periods_val,
      n_test = cv_setting$periods_test
    )

    # Output
    list(forecast = fc, accuracy = acc)
  }
)
# saveRDS(basic, file = "inst/results/20201117_eval_pred_simple.rds")

basic <- readRDS("inst/results/20201117_eval_pred_simple.rds")

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
  title = "simple Model",
  ncol = 2,
  scale = as.Date(c(min(data$index), max(data$index)))
)

# Train data using best performing model ---------------------------------------

# c(predictions, best_model_metrics) %<-% predict_keras_sequential(
#     DT = data,
#     model_type = "simple",
#     n_units = min_params$n_units,
#     epochs = min_params$n_epochs,
#     lag_setting = min_params$lags,
#     length_val = 16,
#     length_test = 8,
#     optimizer_type = min_params$optimizer,
#     dropout = min_params$dropout,
#     recurrent_dropout = min_params$dropout,
#     patience = 100,
#     forecast_future = FALSE,
#     save_model = TRUE,
#     filepath = "inst/models/best_simple.hdf5"
# )
# save(predictions, best_model_metrics, file = "inst/results/20200915_best_simple.rda")

load("inst/results/20200915_best_simple.rda")
model <- keras::load_model_hdf5("inst/models/best_simple.hdf5")
summary(model)

best_model_metrics

# Plot Prediction
plot_prediction(
  data = predictions[, index := as.Date(index)],
  title = "simple Prediction"
)

# Train entire data for future forecast ----------------------------------------

c(predictions_all, best_model_metrics_all) %<-% predict_keras_sequential(
  DT = data[index > "2010-01-01"],
  model_type = "simple",
  n_units = min_params$n_units,
  epochs = min_params$n_epochs,
  lag_setting = min_params$lags,
  length_val = 8,
  length_test = 0,
  optimizer_type = min_params$optimizer,
  dropout = min_params$dropout,
  recurrent_dropout = min_params$dropout,
  save_model = TRUE,
  filepath = "inst/models/best_simple_all.hdf5",
  patience = 100,
  forecast_future = TRUE,
  forecast_length = 4
)
# save(predictions_all, file = "inst/results/20200915_best_simple_all.rda")

load("inst/results/20200915_best_simple_all.rda")

plot_prediction(
  data = predictions_all[, index := as.Date(index)],
  title = "simple 1 year Forecast"
)
