#' Tune a simple deep NN on the Apple FCF time series and forecast the next year

# Initialization ---------------------------------------------------------------

apple <- fcf::DT_apple

tuning_grid <- list(
  lags = list(one = 1, two = 1:2, four = 1:4),
  optimizer = c("rmsprop", "adam"),
  n_epochs = 200,
  dropout = c(0.1)
)
cv_setting <- list(
  periods_train = 58,
  periods_val = 12,
  periods_test = 6,
  skip_span = 7
)

# Tuning -----------------------------------------------------------------------

# library(zeallot)
# c(results, min_params) %<-% tune_keras_sequential(apple, cv_setting, tuning_grid)
# save(results, min_params, file = "inst/results/20200909_tuning_basicNN.rda")

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
#       optimizer_type = min_params$optimizer,
#       save_model = FALSE
#     )
#   }
# )
# saveRDS(basic, file = "inst/results/20200909_eval_pred_basicNN.rds")

basic <- readRDS("inst/results/20200909_eval_pred_basicNN.rds")

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

# Train entire data set using best performing model ----------------------------

# c(predictions, best_model_metrics) %<-% predict_keras_sequential(
#   DT = apple,
#   epochs = min_params$n_epochs,
#   lag_setting = min_params$lags,
#   length_val = 16,
#   length_test = 8,
#   optimizer_type = min_params$optimizer,
#   save_model = TRUE,
#   filepath = "inst/models/best_basic.hdf5"
# )
# save(predictions, best_model_metrics, file = "inst/results/20200909_best_basic.rda")

load("inst/results/20200909_best_basic.rda")
model <- keras::load_model_hdf5("inst/models/best_basic.hdf5")
summary(model)

best_model_metrics

# Plot Prediction
plot_prediction(
  data = predictions[, index := as.Date(index)],
  title = "Basic Machine Learning Model Prediction"
)

# Forecast Future
# ---
