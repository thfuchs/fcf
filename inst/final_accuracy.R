### EBIT -----------------------------------------------------------------------

load(file = "inst/baseline/fc_baselines_ebit.rda")
load(file = "inst/arima/fc_arima_ebit.rda")

# Accuracy Results
acc_baselines <- purrr::map_df(
  fc_baselines_ebit,
  ~ purrr::map_df(.x, "accuracy", .id = "split"),
  .id = "company"
)
acc_arima <- purrr::map_df(
  fc_arima_ebit,
  ~ purrr::map_df(.x, "accuracy", .id = "split"),
  .id = "company"
)

acc <- rbind(acc_baselines, acc_arima)

# Calculate Ranks
acc_rank_str <- c("mape", "smape", "mase", "smis", "acd")
acc[
  , paste0("rank_", acc_rank_str) := lapply(.SD, frank, ties.method = "average"),
  by = c("company", "split", "h"),
  .SDcols = acc_rank_str
]

str_point_acc <- c("smape", "rank_smape", "mase", "rank_mase")
str_dist_acc <- c("smis", "rank_smis", "acd", "rank_acd")

point_acc <- acc[, lapply(.SD, mean), .SDcols = str_point_acc, by = c("type", "h")]; point_acc
dist_acc <- acc[, lapply(.SD, mean), .SDcols = str_dist_acc, by = c("type", "h")]; dist_acc

### EBITDA ---------------------------------------------------------------------


### Net Income -----------------------------------------------------------------

