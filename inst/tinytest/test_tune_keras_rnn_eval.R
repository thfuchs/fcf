# Intro ------------------------------------------------------------------------
result_predict <- readRDS(system.file("tinytest_data/test_tune_keras_rnn_predict_simple.rds", package = "tsRNN"))
result_predict_ticker <- data.table::copy(result_predict)
for (slice in result_predict_ticker) slice[, ticker := "AAPL"]

cv_setting <- list(
  periods_train = 90,
  periods_val = 10,
  periods_test = 10,
  skip_span = 5
)
bayes_best_par <- purrr::map(
  readRDS(system.file("tinytest_data/apple_bayesoptim.rds", package = "tsRNN")),
  "Best_Par"
)

# Check successful -------------------------------------------------------------

output_01 <- tune_keras_rnn_eval(
  fc_sample = result_predict,
  cv_setting = cv_setting,
  bayes_best_par = bayes_best_par,
)

output_02 <- tune_keras_rnn_eval(
  fc_sample = result_predict,
  cv_setting = cv_setting,
  bayes_best_par = bayes_best_par,
  h = list(short = 1)
)
expect_equivalent(
  output_02,
  list(
    Slice1 = structure(list(
      type = "simple", h = "short",
      mape = 1.25390756764597, smape = 1.26181858702229,
      mase = 0.193946896881937, smis = 4.47187505990253, acd = 0.05
    ), row.names = c(NA, -1L), class = c("data.table", "data.frame")),
    Slice2 = structure(list(
      type = "simple", h = "short",
      mape = 6.97238320077379, smape = 6.73750100660553,
      mase = 0.887470721589168, smis = 5.69076463888269, acd = 0.05
    ), row.names = c(NA, -1L), class = c("data.table", "data.frame"))
  )
)

output_03 <- tune_keras_rnn_eval(
  fc_sample = result_predict,
  cv_setting = cv_setting,
  bayes_best_par = bayes_best_par,
  h = list(short = 1:2, long = 3:6)
)
expect_equivalent(
  output_03,
  list(
    Slice1 = structure(list(
      type = c("simple", "simple"), h = c("short", "long"),
      mape = c(13.982625707239, 32.7442254094141),
      smape = c(16.0452730723047, 29.385645866966),
      mase = c(3.51158411392178, 4.66875895310105),
      smis = c(4.1896282332441, 3.73391784998063),
      acd = c(0.45, 0.7)
    ), row.names = c(NA, -2L), class = c("data.table", "data.frame")),
    Slice2 = structure(list(
      type = c("simple", "simple"), h = c("short", "long"),
      mape = c(7.80512868465893, 9.81678401378092),
      smape = c(7.50887808223224, 11.1892813526112),
      mase = c(0.863647095138919, 2.11110227283782),
      smis = c(10.6116062875539, 6.82214930051058),
      acd = c(0.05, 0.05)
    ), row.names = c(NA, -2L), class = c("data.table", "data.frame"))
  )
)

### "fc_sample"
expect_identical(
  output_01,
  tune_keras_rnn_eval(
    fc_sample = result_predict_ticker,
    cv_setting = cv_setting,
    bayes_best_par = bayes_best_par
  )
)

### "cv_setting"
cv_setting_new <- cv_setting
cv_setting_new$periods_train <- 100
cv_setting_new$periods_val <- 0

expect_identical(
  output_01,
  tune_keras_rnn_eval(
    fc_sample = result_predict,
    cv_setting = cv_setting_new,
    bayes_best_par = bayes_best_par
  )
)
expect_identical(
  output_02,
  tune_keras_rnn_eval(
    fc_sample = result_predict,
    cv_setting = cv_setting_new,
    bayes_best_par = bayes_best_par,
    h = list(short = 1)
  )
)
expect_identical(
  output_03,
  tune_keras_rnn_eval(
    fc_sample = result_predict,
    cv_setting = cv_setting_new,
    bayes_best_par = bayes_best_par,
    h = list(short = 1:2, long = 3:6)
  )
)

### "bayes_best_par"

### "col_id"
expect_identical(
  output_01,
  tune_keras_rnn_eval(
    result_predict, cv_setting, bayes_best_par, col_id = NULL
  )
)

### "col_date"
expect_identical(
  output_01,
  tune_keras_rnn_eval(
    result_predict, cv_setting, bayes_best_par, col_date = "index"
  )
)

### "col_value"
expect_identical(
  output_01,
  tune_keras_rnn_eval(
    result_predict, cv_setting, bayes_best_par, col_value = "value"
  )
)

### "h"
expect_identical(
  output_01,
  tune_keras_rnn_eval(result_predict, cv_setting, bayes_best_par, h = 1:10)
)
expect_identical(
  output_02$Slice1[, .SD, .SDcols = -"h"],
  tune_keras_rnn_eval(result_predict, cv_setting, bayes_best_par, h = 1)[["Slice1"]]
)

### "frequency" = 4
expect_identical(
  output_01,
  tune_keras_rnn_eval(
    result_predict, cv_setting, bayes_best_par,
  )
)

### "level" = 95
expect_identical(
  output_01,
  tune_keras_rnn_eval(
    result_predict, cv_setting, bayes_best_par, col_value = "value"
  )
)

# Error ------------------------------------------------------------------------

### "data"
expect_error(
  tune_keras_rnn_eval(data, cv_setting, bayes_best_par),
  class = "tune_keras_rnn_eval_fc_sample_error",
  pattern = "^`fc_sample` must be list, not of class \"function\"\\.$"
)

result_predict_fail <- result_predict[[2]]
expect_error(
  tune_keras_rnn_eval(result_predict_fail, cv_setting, bayes_best_par),
  class = "tune_keras_rnn_eval_fc_sample_error",
  pattern = "^`fc_sample` must be list, not of class \"data.table / data.frame\"\\.$"
)

result_predict_fail <- result_predict
names(result_predict_fail) <- c("Sice1", "Sice2")
expect_error(
  tune_keras_rnn_eval(result_predict_fail, cv_setting, bayes_best_par),
  class = "tune_keras_rnn_eval_fc_sample_error",
  pattern = "^`fc_sample` must be a list named by each `rsample` split\\.$"
)

### "cv_setting"

# 1. wrong class
expect_error(
  tune_keras_rnn_eval(result_predict, cv_setting = as.data.frame(cv_setting)),
  class = "tune_keras_rnn_eval_cv_setting_error",
  pattern = "^`cv_setting` must be list, not of class \"data.frame\"\\.$"
)

# 2. `cv_setting`: Check for complete list with correct names
cv_setting_name_pattern <- paste(
  "^`cv_setting` must be a named list containing \"periods_test\",",
  "\"periods_train\", \"periods_val\", \"skip_span\"\\.$"
)

cv_setting_fail <- cv_setting
cv_setting_fail[["periods_train"]] <- NULL
expect_error(
  tune_keras_rnn_eval(result_predict, cv_setting_fail, bayes_best_par),
  class = "tune_keras_rnn_eval_cv_setting_error",
  pattern = cv_setting_name_pattern
)

cv_setting_fail <- cv_setting
names(cv_setting_fail)[1] <- "periods_trai"
expect_error(
  tune_keras_rnn_eval(result_predict, cv_setting_fail, bayes_best_par),
  class = "tune_keras_rnn_eval_cv_setting_error",
  pattern = cv_setting_name_pattern
)

cv_setting_fail <- cv_setting
names(cv_setting_fail)[2] <- "period_val"
expect_error(
  tune_keras_rnn_eval(result_predict, cv_setting_fail, bayes_best_par),
  class = "tune_keras_rnn_eval_cv_setting_error",
  pattern = cv_setting_name_pattern
)

cv_setting_fail <- cv_setting
names(cv_setting_fail)[3] <- "periods_tst"
expect_error(
  tune_keras_rnn_eval(result_predict, cv_setting_fail, bayes_best_par),
  class = "tune_keras_rnn_eval_cv_setting_error",
  pattern = cv_setting_name_pattern
)

cv_setting_fail <- cv_setting
names(cv_setting_fail)[4] <- "span_skip"
expect_error(
  tune_keras_rnn_eval(result_predict, cv_setting_fail, bayes_best_par),
  class = "tune_keras_rnn_eval_cv_setting_error",
  pattern = cv_setting_name_pattern
)

names(cv_setting_fail) <- NULL
expect_error(
  tune_keras_rnn_eval(result_predict, cv_setting_fail, bayes_best_par),
  class = "tune_keras_rnn_eval_cv_setting_error",
  pattern = cv_setting_name_pattern
)

# 3. `cv_setting`: Check types
cv_setting_fail <- cv_setting
cv_setting_fail[["periods_train"]] <- "90"
expect_error(
  tune_keras_rnn_eval(result_predict, cv_setting_fail, bayes_best_par),
  class = "tune_keras_rnn_eval_cv_setting_error",
  pattern = "^`periods_train` in `cv_setting` must be numeric\\(1\\), not character\\(1\\)\\.$"
)

cv_setting_fail <- cv_setting
cv_setting_fail[["periods_val"]] <- data
expect_error(
  tune_keras_rnn_eval(result_predict, cv_setting_fail, bayes_best_par),
  class = "tune_keras_rnn_eval_cv_setting_error",
  pattern = "^`periods_val` in `cv_setting` must be numeric\\(1\\), not function\\(1\\)\\.$"
)

cv_setting_fail <- cv_setting
cv_setting_fail[["periods_test"]] <- TRUE
expect_error(
  tune_keras_rnn_eval(result_predict, cv_setting_fail, bayes_best_par),
  class = "tune_keras_rnn_eval_cv_setting_error",
  pattern = "^`periods_test` in `cv_setting` must be numeric\\(1\\), not logical\\(1\\)\\.$"
)

cv_setting_fail <- cv_setting
cv_setting_fail[["skip_span"]] <- numeric()
expect_error(
  tune_keras_rnn_eval(result_predict, cv_setting_fail, bayes_best_par),
  class = "tune_keras_rnn_eval_cv_setting_error",
  pattern = "^`skip_span` in `cv_setting` must be numeric\\(1\\), not numeric\\(0\\)\\.$"
)

### "bayes_best_par"
expect_error(
  tune_keras_rnn_eval(result_predict, cv_setting, bayes_best_par = as.data.frame(bayes_best_par)),
  class = "tune_keras_rnn_eval_bayes_best_par_error",
  pattern = "^`bayes_best_par` must be list, not of class \"data.frame\"\\.$"
)

bayes_best_par_fail <- bayes_best_par
names(bayes_best_par_fail) <- c("Slice", "Slice")
expect_error(
  tune_keras_rnn_eval(result_predict, cv_setting, bayes_best_par = bayes_best_par_fail),
  class = "tune_keras_rnn_eval_bayes_best_par_error",
  pattern = "^`bayes_best_par` must be a list named by each `rsample` split\\.$"
)

# missing
bayes_best_par_fail <- bayes_best_par
bayes_best_par_fail[["Slice2"]] <- bayes_best_par_fail[["Slice2"]][-2]
expect_error(
  tune_keras_rnn_eval(result_predict, cv_setting, bayes_best_par_fail),
  class = "tune_keras_rnn_eval_bayes_best_par_error",
  pattern = "^`bayes_best_par\\[\\[\"Slice2\"\\]\\] must contain all required parameters\\.\nMisses \"lag_2\"\\.$"
)

# wrong class (character)
bayes_best_par_fail <- bayes_best_par
bayes_best_par_fail[["Slice2"]][["n_units"]] <- "2"
test_01 <- expect_error(
  tune_keras_rnn_eval(result_predict, cv_setting, bayes_best_par_fail),
  class = "tune_keras_rnn_eval_bayes_best_par_error",
  pattern = "^`bayes_best_par\\[\\[\"Slice2\"\\]\\]` must be numeric, not of class \"character\"\\.$"
)

### "col_id"
expect_error(
  tune_keras_rnn_eval(result_predict, cv_setting, bayes_best_par, col_id = 12),
  class = "tune_keras_rnn_eval_col_id_error",
  pattern = "^`col_id` must be character\\(1\\), not of class \"numeric\\(1\\)\"\\.$"
)
expect_error(
  tune_keras_rnn_eval(result_predict, cv_setting, bayes_best_par, col_id = c("ticker", "id")),
  class = "tune_keras_rnn_eval_col_id_error",
  pattern = "^`col_id` must be character\\(1\\), not of class \"character\\(2\\)\"\\.$"
)
expect_error(
  tune_keras_rnn_eval(result_predict_ticker, cv_setting, bayes_best_par, col_id = "value"),
  class = "tune_keras_rnn_eval_col_id_error",
  pattern = "^Variable specified by `col_id` must be class \"character\"\\.$"
)
expect_error(
  tune_keras_rnn_eval(result_predict_ticker, cv_setting, bayes_best_par, col_id = "index"),
  class = "tune_keras_rnn_eval_col_id_error",
  pattern = "^Variable specified by `col_id` must be class \"character\"\\.$"
)
result_predict_ticker_fail <- data.table::copy(result_predict_ticker)
result_predict_ticker_fail[[1]][, ticker := runif(nrow(result_predict_ticker[[1]]), 0, 1)]
expect_error(
  tune_keras_rnn_eval(result_predict_ticker_fail, cv_setting, bayes_best_par, col_id = "ticker"),
  class = "tune_keras_rnn_eval_col_id_error",
  pattern = "^Variable specified by `col_id` must be class \"character\"\\.$"
)

### "col_date"
expect_error(
  tune_keras_rnn_eval(result_predict, cv_setting, bayes_best_par, col_date = 12),
  class = "tune_keras_rnn_eval_col_date_error",
  pattern = "^`col_date` must be character\\(1\\), not of class \"numeric\\(1\\)\"\\.$"
)
expect_error(
  tune_keras_rnn_eval(result_predict, cv_setting, bayes_best_par, col_date = c("index", "id")),
  class = "tune_keras_rnn_eval_col_date_error",
  pattern = "^`col_date` must be character\\(1\\), not of class \"character\\(2\\)\"\\.$"
)
result_predict_fail <- data.table::copy(result_predict)
result_predict_fail[[2]][, index := value]
expect_error(
  tune_keras_rnn_eval(result_predict_fail, cv_setting, bayes_best_par, col_date = "index", col_value = "value"),
  class = "tune_keras_rnn_eval_col_date_error",
  pattern = "^Variable specified by `col_date` must be class \"Date\" or \"POSIXct\"\\.$"
)

### "col_value"
expect_error(
  tune_keras_rnn_eval(result_predict, cv_setting, bayes_best_par, col_value = 12),
  class = "tune_keras_rnn_eval_col_value_error",
  pattern = "^`col_value` must be character\\(1\\), not of class \"numeric\\(1\\)\"\\.$"
)
expect_error(
  tune_keras_rnn_eval(result_predict, cv_setting, bayes_best_par, col_value = c("value", "value2")),
  class = "tune_keras_rnn_eval_col_value_error",
  pattern = "^`col_value` must be character\\(1\\), not of class \"character\\(2\\)\"\\.$"
)
result_predict_fail <- data.table::copy(result_predict)
result_predict_fail[[2]][, value := as.character(value)]
expect_error(
  tune_keras_rnn_eval(result_predict_fail, cv_setting, bayes_best_par, col_date = "index", col_value = "value"),
  class = "tune_keras_rnn_eval_col_value_error",
  pattern = "^Variable specified by `col_value` must be class \"numeric\"\\.$"
)

### "h"
expect_error(
  tune_keras_rnn_eval(result_predict, cv_setting, bayes_best_par, h = "test"),
  class = "tune_keras_rnn_eval_h_error",
  pattern = "^`h` must be list, numeric or integer, not of class \"character\"\\.$"
)
expect_error(
  tune_keras_rnn_eval(result_predict, cv_setting, bayes_best_par, h = list(A = c(1, 2), B = c("1", "2"))),
  class = "tune_keras_rnn_eval_h_error",
  pattern = "^Elements of `h` must be numeric or integer, not of class \"character\"\\.$"
)
# `h > cv_setting$periods_test`: Warning
expect_warning(
  tune_keras_rnn_eval(result_predict, cv_setting, bayes_best_par, h = c(9:11)),
  class = "tune_keras_rnn_eval_h_warning"
)
expect_warning(
  tune_keras_rnn_eval(result_predict, cv_setting, bayes_best_par, h = list(A = c(12:13))),
  class = "tune_keras_rnn_eval_h_warning"
)

### "frequency"
expect_error(
  tune_keras_rnn_eval(result_predict, cv_setting, bayes_best_par, frequency = NULL),
  class = "tune_keras_rnn_eval_frequency_error",
  pattern = "^`frequency` must be numeric\\(1\\) or integer\\(1\\), not of class \"NULL\\(0\\)\"\\.$"
)
expect_error(
  tune_keras_rnn_eval(result_predict, cv_setting, bayes_best_par, frequency = c(80, 90)),
  class = "tune_keras_rnn_eval_frequency_error",
  pattern = "^`frequency` must be numeric\\(1\\) or integer\\(1\\), not of class \"numeric\\(2\\)\"\\.$"
)
expect_error(
  tune_keras_rnn_eval(result_predict, cv_setting, bayes_best_par, frequency = -5),
  class = "tune_keras_rnn_eval_frequency_error",
  pattern = "^`frequency` must be positive\\.$"
)

### "level"
expect_error(
  tune_keras_rnn_eval(result_predict, cv_setting, bayes_best_par, level = NULL),
  class = "tune_keras_rnn_eval_level_error",
  pattern = "^`level` must be numeric\\(1\\) or integer\\(1\\), not of class \"NULL\\(0\\)\"\\.$"
)
expect_error(
  tune_keras_rnn_eval(result_predict, cv_setting, bayes_best_par, level = c(80, 90)),
  class = "tune_keras_rnn_eval_level_error",
  pattern = "^`level` must be numeric\\(1\\) or integer\\(1\\), not of class \"numeric\\(2\\)\"\\.$"
)
expect_error(
  tune_keras_rnn_eval(result_predict, cv_setting, bayes_best_par, level = -5),
  class = "tune_keras_rnn_eval_level_error",
  pattern = "^`level` must be within interval \\(0, 100\\)\\.$"
)
expect_error(
  tune_keras_rnn_eval(result_predict, cv_setting, bayes_best_par, level = 110),
  class = "tune_keras_rnn_eval_level_error",
  pattern = "^`level` must be within interval \\(0, 100\\)\\.$"
)
