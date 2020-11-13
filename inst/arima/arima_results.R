### Settings -------------------------------------------------------------------

data <- fcf::dow30_clean

cv_setting <- list(
  periods_train = 40,
  periods_val = 6,
  periods_test = 6,
  skip_span = 5
)

companies <- unique(data$ticker)
multiple_h <- list(short = 1, medium = 1:4, long = 5:6, total = 1:6)

### EBIT -----------------------------------------------------------------------

# # seed for bootstrapped based PI
# set.seed(123)
#
# forecast <- purrr::map(
#   companies,
#   purrr::possibly(function(x) {
#     d <- data[ticker == x]
#     predict_arima(d, cv_setting, multiple_h = multiple_h)
#   }, otherwise = NULL)
# )
#
# fc_arima_ebit <- purrr::compact(forecast)
# str(fc_arima_ebit, max.level = 1)
#
# save(fc_arima_ebit, file = "inst/arima/fc_arima_ebit.rda")
load(file = "inst/arima/fc_arima_ebit.rda")

# Accuracy Measures
str_point_acc <- c("smape", "mase")
str_dist_acc <- c("smis", "acd")

samples <- purrr::map_df(
  fc_arima_ebit,
  ~ purrr::map_df(.x, "accuracy")[
    , lapply(.SD, mean), by = type, .SDcols = c(str_point_acc, str_dist_acc)],
  .id = "id"
)
point_acc <- samples[, lapply(.SD, mean), .SDcols = str_point_acc, by = "type"]; point_acc
dist_acc <- samples[, lapply(.SD, mean), .SDcols = str_dist_acc, by = "type"]; dist_acc

# Chart for single ticker (e.g. AAPL)
plot_prediction_samples(
  splits = purrr::map(fc_arima_ebit[[1]], "forecast"),
  title = "ARMIA Forecast including Prediction Interval for APPL",
  ncol = 3,
  scale = as.Date(c(min(data$index), max(data$index))),
  PI = TRUE
)

### EBITDA ---------------------------------------------------------------------


### Net Income -----------------------------------------------------------------

