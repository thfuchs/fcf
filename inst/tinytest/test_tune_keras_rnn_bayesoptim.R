# Intro ------------------------------------------------------------------------
suppressWarnings(suppressPackageStartupMessages(library(mockery)))

apple <- tsRNN::DT_apple
apple_ticker <- data.table::copy(apple)[, ticker := "AAPL"]

result <- readRDS(system.file("tinytest_data/apple_bayesoptim.rds", package = "tsRNN"))
result_ticker <- readRDS(system.file("tinytest_data/apple_ticker_bayesoptim.rds", package = "tsRNN"))

cv_setting <- list(
  periods_train = 90,
  periods_val = 10,
  periods_test = 10,
  skip_span = 5
)

tuning_bounds <- list(
  lag_1 = c(1L, 4L),
  lag_2 = c(1L, 4L),
  n_units = c(8L, 32L),
  n_epochs = c(20L, 50L),
  optimizer_type = c(1L, 3L),
  dropout = c(0, 0.5),
  recurrent_dropout = c(0, 0.5),
  learning_rate = c(0.001, 0.01)
)

# Check successful -------------------------------------------------------------

# Mock BayesianOptimization with pre-calculated result from "apple" ([[1]] only)
stub(
  where = tune_keras_rnn_bayesoptim,
  what = "rBayesianOptimization::BayesianOptimization",
  how = function(...) readRDS(
    system.file("tinytest_data/apple_bayes_simple.rds", package = "tsRNN")
  )
)
expect_identical(
  tune_keras_rnn_bayesoptim(apple, "simple", cv_setting, tuning_bounds)[[1]],
  result[[1]]
)
expect_identical(
  tune_keras_rnn_bayesoptim(apple, "simple", cv_setting, tuning_bounds = list())[[1]],
  result[[1]]
)

expect_identical(
  tune_keras_rnn_bayesoptim(
    data = apple,
    model_type = "simple",
    col_id = NULL,
    col_date = "index",
    col_value = "value",
    cv_setting = cv_setting,
    tuning_bounds = tuning_bounds,
    save = NULL
  )[[1]],
  result[[1]]
)
# "save" and "save_id"
tmp <- tempdir()
expect_identical(
  tune_keras_rnn_bayesoptim(
    apple, "simple", cv_setting, tuning_bounds = list(), save = tmp, save_id = "test-save-01"
  )[[1]],
  result[[1]]
)
tmp_files <- list.files(tmp, pattern = "\\w+_test-save-01_\\w+\\.rds$", full.names = TRUE)
expect_identical(length(tmp_files), 2L)
ignore(expect_true)(all(file.remove(tmp_files)))

# Mock BayesianOptimization with pre-calculated result from "apple_ticker" ([[2]] only)
stub(
  where = tune_keras_rnn_bayesoptim,
  what = "rBayesianOptimization::BayesianOptimization",
  how = function(...) readRDS(
    system.file("tinytest_data/apple_ticker_bayes_simple.rds", package = "tsRNN")
  )
)

expect_identical(
  tune_keras_rnn_bayesoptim(apple_ticker, "simple", cv_setting, tuning_bounds, col_id = "ticker")[[2]],
  result_ticker[[2]]
)
expect_identical(
  tune_keras_rnn_bayesoptim(apple_ticker, "simple", cv_setting, tuning_bounds = list(), col_id = "ticker")[[2]],
  result_ticker[[2]]
)
expect_identical(
  tune_keras_rnn_bayesoptim(
    data = apple_ticker,
    model_type = "simple",
    col_id = "ticker",
    col_date = "index",
    col_value = "value",
    cv_setting = cv_setting,
    tuning_bounds = tuning_bounds,
    save = NULL
  )[[2]],
  result_ticker[[2]]
)
# "save" and "save_id"
tmp <- tempdir()
expect_identical(
  tune_keras_rnn_bayesoptim(
    apple_ticker, "simple", cv_setting, tuning_bounds = list(), col_id = "ticker",
    save = tmp, save_id = "test-save-ticker")[[2]],
  result_ticker[[2]]
)
tmp_files <- list.files(tmp, pattern = "\\w+_test-save-ticker_\\w+\\.rds$", full.names = TRUE)
expect_identical(length(tmp_files), 2L)
ignore(expect_true)(all(file.remove(tmp_files)))

# Error ------------------------------------------------------------------------
rm(tune_keras_rnn_bayesoptim)

### "data"
expect_error(
  tune_keras_rnn_bayesoptim(data),
  class = "tune_keras_rnn_bayesoptim_data_error",
  pattern = "^`data` must be data.frame, not of class \"function\"\\.$"
)

### "model_type"
expect_error(
  tune_keras_rnn_bayesoptim(apple, model_type = TRUE),
  class = "tune_keras_rnn_bayesoptim_model_type_error",
  pattern = "^`model_type` must be character\\(1\\), not of class \"logical\\(1\\)\"\\.$"
)
expect_error(
  tune_keras_rnn_bayesoptim(apple, model_type = c("ltsm")),
  class = "rlang_error",
  pattern = "^`model_type` must be one of \"simple\", \"gru\", or \"lstm\"\\."
)
expect_error(
  tune_keras_rnn_bayesoptim(apple, model_type = c("simple", "lstm")),
  class = "tune_keras_rnn_bayesoptim_model_type_error",
  pattern = "^`model_type` must be character\\(1\\), not of class \"character\\(2\\)\"\\.$"
)

### "cv_setting"

# 1. wrong class
expect_error(
  tune_keras_rnn_bayesoptim(apple, "simple", cv_setting = as.data.frame(cv_setting)),
  class = "tune_keras_rnn_bayesoptim_cv_setting_error",
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
  tune_keras_rnn_bayesoptim(apple, "simple", cv_setting_fail, tuning_bounds),
  class = "tune_keras_rnn_bayesoptim_cv_setting_error",
  pattern = cv_setting_name_pattern
)

cv_setting_fail <- cv_setting
names(cv_setting_fail)[1] <- "periods_trai"
expect_error(
  tune_keras_rnn_bayesoptim(apple, "simple", cv_setting_fail, tuning_bounds),
  class = "tune_keras_rnn_bayesoptim_cv_setting_error",
  pattern = cv_setting_name_pattern
)

cv_setting_fail <- cv_setting
names(cv_setting_fail)[2] <- "period_val"
expect_error(
  tune_keras_rnn_bayesoptim(apple, "simple", cv_setting_fail, tuning_bounds),
  class = "tune_keras_rnn_bayesoptim_cv_setting_error",
  pattern = cv_setting_name_pattern
)

cv_setting_fail <- cv_setting
names(cv_setting_fail)[3] <- "periods_tst"
expect_error(
  tune_keras_rnn_bayesoptim(apple, "simple", cv_setting_fail, tuning_bounds),
  class = "tune_keras_rnn_bayesoptim_cv_setting_error",
  pattern = cv_setting_name_pattern
)

cv_setting_fail <- cv_setting
names(cv_setting_fail)[4] <- "span_skip"
expect_error(
  tune_keras_rnn_bayesoptim(apple, "simple", cv_setting_fail, tuning_bounds),
  class = "tune_keras_rnn_bayesoptim_cv_setting_error",
  pattern = cv_setting_name_pattern
)

names(cv_setting_fail) <- NULL
expect_error(
  tune_keras_rnn_bayesoptim(apple, "simple", cv_setting_fail, tuning_bounds),
  class = "tune_keras_rnn_bayesoptim_cv_setting_error",
  pattern = cv_setting_name_pattern
)

# 3. `cv_setting`: Check types
cv_setting_fail <- cv_setting
cv_setting_fail[["periods_train"]] <- "90"
expect_error(
  tune_keras_rnn_bayesoptim(apple, "simple", cv_setting_fail, tuning_bounds),
  class = "tune_keras_rnn_bayesoptim_cv_setting_error",
  pattern = "^`periods_train` in `cv_setting` must be numeric\\(1\\), not character\\(1\\)\\.$"
)

cv_setting_fail <- cv_setting
cv_setting_fail[["periods_val"]] <- data
expect_error(
  tune_keras_rnn_bayesoptim(apple, "simple", cv_setting_fail, tuning_bounds),
  class = "tune_keras_rnn_bayesoptim_cv_setting_error",
  pattern = "^`periods_val` in `cv_setting` must be numeric\\(1\\), not function\\(1\\)\\.$"
)

cv_setting_fail <- cv_setting
cv_setting_fail[["periods_test"]] <- TRUE
expect_error(
  tune_keras_rnn_bayesoptim(apple, "simple", cv_setting_fail, tuning_bounds),
  class = "tune_keras_rnn_bayesoptim_cv_setting_error",
  pattern = "^`periods_test` in `cv_setting` must be numeric\\(1\\), not logical\\(1\\)\\.$"
)

cv_setting_fail <- cv_setting
cv_setting_fail[["skip_span"]] <- numeric()
expect_error(
  tune_keras_rnn_bayesoptim(apple, "simple", cv_setting_fail, tuning_bounds),
  class = "tune_keras_rnn_bayesoptim_cv_setting_error",
  pattern = "^`skip_span` in `cv_setting` must be numeric\\(1\\), not numeric\\(0\\)\\.$"
)

### "tuning_bounds"
expect_error(
  tune_keras_rnn_bayesoptim(apple, "simple", cv_setting, tuning_bounds = as.data.frame(tuning_bounds)),
  class = "tune_keras_rnn_bayesoptim_tuning_bounds_error",
  pattern = "^`tuning_bounds` must be list, not of class \"data.frame\"\\.$"
)

# loop over "tuning_bounds"
tests_tuning_bounds <- sapply(names(tuning_bounds), function(bound) {
  tuning_bounds_fail <- tuning_bounds
  bound_class <- sprintf(
    "tune_keras_rnn_bayesoptim_tuning_bounds[[\"%s\"]]_error", bound)

  # single length
  tuning_bounds_fail[[bound]] <- 1L
  test_01 <- expect_error(
    tune_keras_rnn_bayesoptim(apple, "simple", cv_setting, tuning_bounds_fail),
    class = bound_class
  )

  # wrong class (character)
  tuning_bounds_fail[[bound]] <- c("0", "4")
  test_02 <- expect_error(
    tune_keras_rnn_bayesoptim(apple, "simple", cv_setting, tuning_bounds_fail),
    class = bound_class
  )

  if (is.integer(tuning_bounds[[bound]])) {
    # negative
    tuning_bounds_fail[[bound]] <- c(-2L, 3L)
    test_03 <- expect_error(
      tune_keras_rnn_bayesoptim(apple, "simple", cv_setting, tuning_bounds_fail),
      class = bound_class
    )

    # false integer / numeric
    tuning_bounds_fail[[bound]] <- c(1, 10)
    test_04 <- expect_error(
      tune_keras_rnn_bayesoptim(apple, "simple", cv_setting, tuning_bounds_fail),
      class = bound_class
    )
  } else {
    # negative
    tuning_bounds_fail[[bound]] <- c(-2, 3)
    test_03 <- expect_error(
      tune_keras_rnn_bayesoptim(apple, "simple", cv_setting, tuning_bounds_fail),
      class = bound_class
    )

    # false integer / numeric
    tuning_bounds_fail[[bound]] <- c(1L, 10L)
    test_04 <- expect_error(
      tune_keras_rnn_bayesoptim(apple, "simple", cv_setting, tuning_bounds_fail),
      class = bound_class
    )
  }

  return(c(test_01, test_02, test_03, test_04))
})
expect_true(all(tests_tuning_bounds))

# c. new variable
tuning_bounds_fail <- tuning_bounds
tuning_bounds_fail[["X"]] <- c(1, 2)
expect_identical(
  suppressMessages(tune_keras_rnn_bayesoptim(apple, "simple", cv_setting, tuning_bounds_fail)),
  list(Slice1 = NULL, Slice2 = NULL)
)
expect_message(
  tune_keras_rnn_bayesoptim(apple, "simple", cv_setting, tuning_bounds_fail),
  "^Error"
)

### "col_id"
expect_error(
  tune_keras_rnn_bayesoptim(apple, "simple", cv_setting, tuning_bounds, col_id = 12),
  class = "tune_keras_rnn_bayesoptim_col_id_error",
  pattern = "^`col_id` must be character\\(1\\), not of class \"numeric\\(1\\)\"\\.$"
)
expect_error(
  tune_keras_rnn_bayesoptim(apple, "simple", cv_setting, tuning_bounds, col_id = c("ticker", "id")),
  class = "tune_keras_rnn_bayesoptim_col_id_error",
  pattern = "^`col_id` must be character\\(1\\), not of class \"character\\(2\\)\"\\.$"
)
expect_error(
  tune_keras_rnn_bayesoptim(apple, "simple", cv_setting, tuning_bounds, col_id = "value"),
  class = "tune_keras_rnn_bayesoptim_col_id_error",
  pattern = "^Variable specified by `col_id` must be class \"character\"\\.$"
)
expect_error(
  tune_keras_rnn_bayesoptim(apple, "simple", cv_setting, tuning_bounds, col_id = "index"),
  class = "tune_keras_rnn_bayesoptim_col_id_error",
  pattern = "^Variable specified by `col_id` must be class \"character\"\\.$"
)

### "col_date"
expect_error(
  tune_keras_rnn_bayesoptim(apple, "simple", cv_setting, tuning_bounds, col_date = 12),
  class = "tune_keras_rnn_bayesoptim_col_date_error",
  pattern = "^`col_date` must be character\\(1\\), not of class \"numeric\\(1\\)\"\\.$"
)
expect_error(
  tune_keras_rnn_bayesoptim(apple, "simple", cv_setting, tuning_bounds, col_date = c("index", "id")),
  class = "tune_keras_rnn_bayesoptim_col_date_error",
  pattern = "^`col_date` must be character\\(1\\), not of class \"character\\(2\\)\"\\.$"
)
expect_error(
  tune_keras_rnn_bayesoptim(apple[,2], "simple", cv_setting, tuning_bounds, col_date = "index", col_value = "value"),
  class = "tune_keras_rnn_bayesoptim_col_date_error",
  pattern = "^Variable specified by `col_date` must be class \"Date\" or \"POSIXct\"\\.$"
)

### "col_value"
expect_error(
  tune_keras_rnn_bayesoptim(apple, "simple", cv_setting, tuning_bounds, col_value = 12),
  class = "tune_keras_rnn_bayesoptim_col_value_error",
  pattern = "^`col_value` must be character\\(1\\), not of class \"numeric\\(1\\)\"\\.$"
)
expect_error(
  tune_keras_rnn_bayesoptim(apple, "simple", cv_setting, tuning_bounds, col_value = c("value", "value2")),
  class = "tune_keras_rnn_bayesoptim_col_value_error",
  pattern = "^`col_value` must be character\\(1\\), not of class \"character\\(2\\)\"\\.$"
)
expect_error(
  tune_keras_rnn_bayesoptim(apple[,1], "simple", cv_setting, tuning_bounds, col_date = "index", col_value = "value"),
  class = "tune_keras_rnn_bayesoptim_col_value_error",
  pattern = "^Variable specified by `col_value` must be class \"numeric\"\\.$"
)
expect_error(
  tune_keras_rnn_bayesoptim(apple[,1], "simple", cv_setting, tuning_bounds, col_date = "index", col_value = "value"),
  class = "tune_keras_rnn_bayesoptim_col_value_error",
  pattern = "^Variable specified by `col_value` must be class \"numeric\"\\.$"
)
expect_error(
  tune_keras_rnn_bayesoptim(apple_ticker, "simple", cv_setting, tuning_bounds, col_date = "index", col_value = "ticker"),
  class = "tune_keras_rnn_bayesoptim_col_value_error",
  pattern = "^Variable specified by `col_value` must be class \"numeric\"\\.$"
)

### "save"
expect_error(
  tune_keras_rnn_bayesoptim(apple, "simple", cv_setting, tuning_bounds, save = TRUE),
  class = "tune_keras_rnn_bayesoptim_save_error",
  pattern = "^`save` must be character\\(1\\), not of class \"logical\\(1\\)\"\\.$"
)
expect_error(
  tune_keras_rnn_bayesoptim(apple, "simple", cv_setting, tuning_bounds, save = file.path(tempdir(), "XXX")),
  class = "tune_keras_rnn_bayesoptim_save_error",
  pattern = "^Directory specified in `save` does not exist\\.$"
)
expect_error(
  tune_keras_rnn_bayesoptim(apple, "simple", cv_setting, tuning_bounds, save = file.path(tempdir(), "file.rda")),
  class = "tune_keras_rnn_bayesoptim_save_error",
  pattern = "^Directory specified in `save` does not exist\\.$"
)

### "save_id"
expect_error(
  tune_keras_rnn_bayesoptim(apple, "simple", cv_setting, tuning_bounds, save_id = 12),
  class = "tune_keras_rnn_bayesoptim_save_id_error",
  pattern = "^`save_id` must be character\\(1\\), not of class \"numeric\\(1\\)\"\\.$"
)
