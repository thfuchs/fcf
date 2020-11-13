# Forecasts and Evaluations for Baseline Models for dow30 data set

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
#     predict_baselines(d, cv_setting, multiple_h = multiple_h)
#   }, otherwise = NULL)
# )
#
# fc_baselines_ebit <- purrr::compact(forecast)
# str(fc_baselines_ebit, max.level = 1)
#
# save(fc_baselines_ebit, file = "inst/baseline/fc_baselines_ebit.rda")
load(file = "inst/baseline/fc_baselines_ebit.rda")

# Accuracy Measures
str_point_acc <- c("smape", "mase")
str_dist_acc <- c("smis", "acd")

samples <- purrr::map_df(
  fc_baselines_ebit,
  ~ purrr::map_df(.x, "accuracy")[
    , lapply(.SD, mean), by = type, .SDcols = c(str_point_acc, str_dist_acc)],
  .id = "id"
)
point_acc <- samples[, lapply(.SD, mean), .SDcols = str_point_acc, by = "type"]; point_acc
dist_acc <- samples[, lapply(.SD, mean), .SDcols = str_dist_acc, by = "type"]; dist_acc


### EBITDA ---------------------------------------------------------------------


### Net Income -----------------------------------------------------------------

