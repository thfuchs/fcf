# Forecasts and Evaluations for Baseline Models for dow30 data set

### Settings -------------------------------------------------------------------

data <- fcf::dow30[, .(ticker, date, ebit)]
data.table::setnames(data, c("date", "ebit"), c("index", "value"))

cv_setting <- list(
  periods_train = 40,
  periods_val = 8,
  periods_test = 8,
  skip_span = 7
)

companies <- unique(data$ticker)

### w/o transformation ---------------------------------------------------------

forecast <- purrr::map(
  companies,
  purrr::possibly(function(x) {
    d <- data[ticker == x]
    predict_baselines(d, cv_setting)
  }, otherwise = NULL)
)

forecast_edit <- purrr::compact(forecast)
str(forecast_edit, max.level = 1)

# RMSE
forecast_edit[[1]]
mape_samples <- purrr::map_df(
  forecast_edit,
  ~ purrr::map_df(.x, "accuracy")[, .(mean = mean(MAPE)), by = type],
  .id = "id"
)

mape_samples[, .(
  Mean = mean(mean),
  `Std. Dev` = stats::sd(mean),
  Min = min(mean),
  Median = stats::median(mean),
  Max = max(mean)
), by = "type"]

