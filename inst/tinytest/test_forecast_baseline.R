### Intro ----------------------------------------------------------------------
apple <- fcf::ts_apple

### Check successful -----------------------------------------------------------

expect_equivalent(
  forecast_baseline(apple, test_size = NULL, acc_measure = "MAE"),
  list(
    fc_naive = c(1526.4020990991, 4100.125),
    fc_snaive = c(1030.41442592593, 2025.375),
    fc_mean = c(5436.21973469388, 12091.9524285714),
    fc_ses = c(1312.16683455669, 3761.18603739016),
    fc_holt = c(1376.71452624072, 3626.45927344563)
  )
)

output <-
  forecast_baseline(apple, test_size = NULL, acc_measure = c("MAE", "ACF1"))

expect_equivalent(
  output$fc_snaive,
  structure(list(
    MAE = c(1030.41442592593, 2025.375),
    ACF1 = c(0.750972255655298, -0.180061537209642)
  ), class = "data.frame", row.names = c("Training set", "Test set"))
)
expect_equivalent(
  output$fc_holt,
  structure(list(
    MAE = c(1376.71452624072, 3626.45927344563),
    ACF1 = c(0.0987116209006875, -0.182995418018833)
  ), class = "data.frame", row.names = c("Training set", "Test set"))
)

### Error ----------------------------------------------------------------------

# Wrong Classes
expect_error(
  forecast_baseline(TRUE),
  class = "forecast_baseline_data_error",
  pattern = "^`data` must be ts, not of class \"logical\"\\.$"
)
expect_error(
  forecast_baseline(apple, test_size = TRUE),
  class = "forecast_baseline_test_size_error",
  pattern = "^`test_size` must be numeric or integer, not of class \"logical\"\\.$"
)
expect_error(
  forecast_baseline(apple, acc_measure = TRUE),
  class = "forecast_baseline_acc_measure_error",
  pattern = "^`acc_measure` must be character, not of class \"logical\"\\.$"
)

# Wrong data types
expect_error(
  forecast_baseline(apple, test_size = c(1, 2), acc_measure = "MAE"),
  class = "forecast_baseline_test_size_error",
  pattern = "^`test_size` must be a vector of length 1\\.$"
)
expect_error(
  forecast_baseline(apple, test_size = -1),
  class = "forecast_baseline_test_size_error",
  pattern = "`test_size` must be NULL or a positive numeric, not \"-1\"\\.$"
)

expect_error(
  forecast_baseline(apple, acc_measure = ""),
  class = "forecast_baseline_acc_measure_error",
  pattern = "^`acc_measure` must be one of.*$"
)
