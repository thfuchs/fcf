### Intro ----------------------------------------------------------------------
suppressPackageStartupMessages(library(data.table))
apple <- tsRNN::DT_apple
apple_ticker <- data.table::copy(apple)[, ticker := "AAPL"]

cv_setting <- list(
  periods_train = 90,
  periods_val = 10,
  periods_test = 10,
  skip_span = 5
)

### Check successful -----------------------------------------------------------

output_01 <- cv_baselines(apple, cv_setting)
expect_identical(
  output_01[[2]]$accuracy$type,
  c("Naive", "Snaive", "Drift", "Holt")
)
expect_identical(
  round(output_01[[2]]$accuracy$mape, 2),
  c(62.54, 10.41, 70.58, 42.37)
)
expect_identical(
  round(output_01[[2]]$accuracy$smape, 2),
  c(44.74, 11.34, 48.92, 34.53)
)
expect_identical(
  round(output_01[[2]]$accuracy$mase, 2),
  c(7.71, 1.48, 8.74, 5.53)
)

# Check that not randomized results are identical if solely "periods_train" is specified
output_01b <- cv_baselines(
  apple,
  list(periods_train = 100, periods_val = 0, periods_test = 10, skip_span = 5)
)
expect_identical(
  purrr::map(output_01, ~.x[["forecast"]][, .SD, .SDcols = -c("lo95", "hi95")]),
  purrr::map(output_01b, ~.x[["forecast"]][, .SD, .SDcols = -c("lo95", "hi95")])
)
expect_identical(
  purrr::map(output_01, ~.x[["accuracy"]][, .SD, .SDcols = -c("smis", "acd")]),
  purrr::map(output_01b, ~.x[["accuracy"]][, .SD, .SDcols = -c("smis", "acd")])
)

# And similarly, same result with / without "col_id"
output_01c <- cv_baselines(
  copy(apple)[,ticker := "AAPL"],
  cv_setting,
  col_id = "ticker"
)
output_01d <- cv_baselines(apple, cv_setting, col_id = NULL)

expect_identical(unique(output_01c[[1]][["forecast"]][["ticker"]]), "AAPL")
expect_identical(
  purrr::map(output_01c, ~.x[["forecast"]][, .SD, .SDcols = -c("lo95", "hi95", "ticker")]),
  purrr::map(output_01d, ~.x[["forecast"]][, .SD, .SDcols = -c("lo95", "hi95")])
)
expect_identical(
  purrr::map(output_01c, ~.x[["accuracy"]][, .SD, .SDcols = -c("smis", "acd")]),
  purrr::map(output_01d, ~.x[["accuracy"]][, .SD, .SDcols = -c("smis", "acd")])
)

# Check with "h"
output_02 <- cv_baselines(apple, cv_setting, h = list(short = 1:2, long = 3:6))
expect_identical(names(output_02[[1]]), c("forecast", "accuracy"))
expect_equivalent(
  output_02[[2]]$accuracy[2:3, .SD, .SDcols = -c("smis", "acd")],
  structure(list(
    type = c("Naive", "Snaive"), h = c("long", "short"),
    mape = c(55.3289204777709, 3.46872005530352),
    smape = c(41.4179060896227, 3.56803432193035),
    mase = c(7.44680338706209, 0.36226871014052)),
    row.names = c(NA, -2L), class = c("data.table", "data.frame"))
)

selection <- c("index", "value", "key", "type")
expect_identical(
  cv_baselines(apple, cv_setting)[[1]]$forecast[, .SD, .SDcols = selection],
  cv_baselines(apple_ticker, cv_setting)[[1]]$forecast[, .SD, .SDcols = selection]
)

expect_identical(
  cv_baselines(apple, cv_setting, frequency = 3)[[1]]$forecast[, .SD, .SDcols = selection],
  cv_baselines(apple, cv_setting, frequency = 3L)[[1]]$forecast[, .SD, .SDcols = selection]
)

# Check "transform"
output_03 <- cv_baselines(apple, cv_setting, transform = "normalize")
expect_identical(names(output_03[[1]]), c("forecast", "accuracy"))
expect_equivalent(
  output_03[[2]]$forecast[c(1, 101, 130, 150), c("index", "value", "key", "type")],
  structure(list(index = structure(
    c(702000000, 1490918400, 1561680000,  1561680000),
    class = c("POSIXct", "POSIXt")),
    value = c(210.405, 14097, 10105, 21200.3781228207),
    key = c("actual", "actual", "predict", "predict"),
    type = c(NA, NA, "Snaive", "Holt")
  ), row.names = c(NA, -4L), class = c("data.table", "data.frame"))
)

### Error ----------------------------------------------------------------------

# Wrong Classes
expect_error(
  cv_baselines(TRUE),
  class = "cv_baselines_data_error",
  pattern = "^`data` must be data.frame, not of class \"logical\"\\.$"
)
expect_error(
  cv_baselines(apple, cv_setting = "cv"),
  class = "cv_baselines_cv_setting_error",
  pattern = "^`cv_setting` must be list, not of class \"character\"\\.$"
)
expect_error(
  cv_baselines(apple, cv_setting, col_id = 1),
  class = "cv_baselines_col_id_error",
  pattern = "^`col_id` must be character, not of class \"numeric\"\\.$"
)
expect_error(
  cv_baselines(apple, cv_setting, col_date = TRUE),
  class = "cv_baselines_col_date_error",
  pattern = "^`col_date` must be character, not of class \"logical\"\\.$"
)
expect_error(
  cv_baselines(apple, cv_setting, col_value = data),
  class = "cv_baselines_col_value_error",
  pattern = "^`col_value` must be character, not of class \"function\"\\.$"
)
expect_error(
  cv_baselines(apple, cv_setting, transform = TRUE),
  class = "cv_baselines_transform_error",
  pattern = "^`transform` must be character, not of class \"logical\"\\.$"
)
expect_error(
  cv_baselines(apple, cv_setting, frequency = TRUE),
  class = "cv_baselines_frequency_error",
  pattern = "^`frequency` must be numeric or integer, not of class \"logical\"\\.$"
)
expect_error(
  cv_baselines(apple, cv_setting, h = TRUE),
  class = "cv_baselines_h_error",
  pattern = "^`h` must be list, numeric or integer, not of class \"logical\"\\.$"
)

# Wrong data types
expect_error(
  cv_baselines(apple, cv_setting, col_id = "value"),
  class = "cv_baselines_col_id_error",
  pattern = "^Variable specified by `col_id` must be class \"character\"\\.$"
)
expect_error(
  cv_baselines(apple, cv_setting, col_id = "index"),
  class = "cv_baselines_col_id_error",
  pattern = "^Variable specified by `col_id` must be class \"character\"\\.$"
)
expect_error(
  cv_baselines(apple[,1], cv_setting, col_date = "index", col_value = "value"),
  class = "cv_baselines_col_value_error",
  pattern = "^Variable specified by `col_value` must be class \"numeric\"\\.$"
)
expect_error(
  cv_baselines(apple[,2], cv_setting, col_date = "index", col_value = "value"),
  class = "cv_baselines_col_date_error",
  pattern = "^Variable specified by `col_date` must be class \"Date\" or \"POSIXct\"\\.$"
)
expect_error(
  cv_baselines(apple[,1], cv_setting, col_date = "index", col_value = "value"),
  class = "cv_baselines_col_value_error",
  pattern = "^Variable specified by `col_value` must be class \"numeric\"\\.$"
)
expect_error(
  cv_baselines(apple_ticker, cv_setting, col_date = "index", col_value = "ticker"),
  class = "cv_baselines_col_value_error",
  pattern = "^Variable specified by `col_value` must be class \"numeric\"\\.$"
)

### `cv_setting` specific errors

# 1. `cv_setting`: Check for complete list with correct names
cv_setting_name_pattern <- paste(
  "^`cv_setting` must be a named list containing \"periods_test\",",
  "\"periods_train\", \"periods_val\", \"skip_span\"\\.$"
)

cv_setting_fail <- cv_setting
cv_setting_fail[["periods_train"]] <- NULL
expect_error(
  cv_baselines(apple, cv_setting_fail),
  class = "cv_baselines_cv_setting_error",
  pattern = cv_setting_name_pattern
)

cv_setting_fail <- cv_setting
names(cv_setting_fail)[1] <- "periods_trai"
expect_error(
  cv_baselines(apple, cv_setting_fail),
  class = "cv_baselines_cv_setting_error",
  pattern = cv_setting_name_pattern
)

cv_setting_fail <- cv_setting
names(cv_setting_fail)[2] <- "period_val"
expect_error(
  cv_baselines(apple, cv_setting_fail),
  class = "cv_baselines_cv_setting_error",
  pattern = cv_setting_name_pattern
)

cv_setting_fail <- cv_setting
names(cv_setting_fail)[3] <- "periods_tst"
expect_error(
  cv_baselines(apple, cv_setting_fail),
  class = "cv_baselines_cv_setting_error",
  pattern = cv_setting_name_pattern
)

cv_setting_fail <- cv_setting
names(cv_setting_fail)[4] <- "span_skip"
expect_error(
  cv_baselines(apple, cv_setting_fail),
  class = "cv_baselines_cv_setting_error",
  pattern = cv_setting_name_pattern
)

names(cv_setting_fail) <- NULL
expect_error(
  cv_baselines(apple, cv_setting_fail),
  class = "cv_baselines_cv_setting_error",
  pattern = cv_setting_name_pattern
)

# 2. `cv_setting`: Check types

cv_setting_fail <- cv_setting
cv_setting_fail[["periods_train"]] <- "90"
expect_error(
  cv_baselines(apple, cv_setting_fail),
  class = "cv_baselines_cv_setting_error",
  pattern = "^`periods_train` in `cv_setting` must be numeric\\(1\\), not character\\(1\\)\\.$"
)

cv_setting_fail <- cv_setting
cv_setting_fail[["periods_val"]] <- data
expect_error(
  cv_baselines(apple, cv_setting_fail),
  class = "cv_baselines_cv_setting_error",
  pattern = "^`periods_val` in `cv_setting` must be numeric\\(1\\), not function\\(1\\)\\.$"
)

cv_setting_fail <- cv_setting
cv_setting_fail[["periods_test"]] <- TRUE
expect_error(
  cv_baselines(apple, cv_setting_fail),
  class = "cv_baselines_cv_setting_error",
  pattern = "^`periods_test` in `cv_setting` must be numeric\\(1\\), not logical\\(1\\)\\.$"
)

cv_setting_fail <- cv_setting
cv_setting_fail[["skip_span"]] <- numeric()
expect_error(
  cv_baselines(apple, cv_setting_fail),
  class = "cv_baselines_cv_setting_error",
  pattern = "^`skip_span` in `cv_setting` must be numeric\\(1\\), not numeric\\(0\\)\\.$"
)

### "h"
expect_error(
  cv_baselines(apple, cv_setting, h = list(A = c(1, 2), B = c("1", "2"))),
  class = "cv_baselines_h_error",
  pattern = "^Elements of `h` must be numeric or integer, not of class \"character\"\\.$"
)
# `h > cv_setting$periods_test`: Warning
expect_warning(
  cv_baselines(apple, cv_setting, h = list(A = c(12:13))),
  class = "cv_baselines_h_warning"
)
