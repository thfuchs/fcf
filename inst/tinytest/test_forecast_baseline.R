### Intro ----------------------------------------------------------------------
apple <- fcf::apple_fcf

### Check successful -----------------------------------------------------------

expect_equivalent(
  forecast_baseline(apple, test_size = NULL, acc_measure = "MAE"),
  list(
    fc_naive = c(2818.67741935484, 6026.375),
    fc_snaive = c(1477.76666666667, 2832.125),
    fc_mean = c(5325.07990040742, 11568.9281914894),
    fc_ses = c(2093.51614469345, 5375.66012724925),
    fc_holt = c(2197.17212020866, 6520.34066856645)
  )
)

output <-
  forecast_baseline(apple, test_size = NULL, acc_measure = c("MAE", "ACF1"))

expect_equivalent(
  output$fc_snaive,
  structure(list(
    MAE = c(1477.76666666667, 2832.125),
    ACF1 = c(0.47423302949862, 0.0422430876126011)
  ), class = "data.frame", row.names = c("Training set", "Test set"))
)
expect_equivalent(
  output$fc_holt,
  structure(list(
    MAE = c(2197.17212020866, 6520.34066856645),
    ACF1 = c(-0.0103074264379004, -0.139309411878353)
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
