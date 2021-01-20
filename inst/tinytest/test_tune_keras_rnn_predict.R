# Intro ------------------------------------------------------------------------
apple <- tsRNN::DT_apple
apple_ticker <- data.table::copy(apple)[, ticker := "AAPL"]

result_simple <- readRDS(system.file(
  "tinytest_data/test_tune_keras_rnn_predict_simple.rds", package = "tsRNN"))
result_gru <- readRDS(system.file(
  "tinytest_data/test_tune_keras_rnn_predict_gru.rds", package = "tsRNN"))

model_simple <- keras::load_model_hdf5(system.file(
  "tinytest_data/test_simple_Slice1.hdf5", package = "tsRNN"))
model_gru <- keras::load_model_hdf5(system.file(
  "tinytest_data/test_simple_Slice1.hdf5", package = "tsRNN"))

bayes_best_par <- purrr::map(
  readRDS(system.file("tinytest_data/apple_bayesoptim.rds", package = "tsRNN")),
  "Best_Par"
)
cv_setting <- list(
  periods_train = 90,
  periods_val = 10,
  periods_test = 10,
  skip_span = 5
)
tmp <- tempdir()

# Check successful -------------------------------------------------------------

### Simple
result_check_simple <- tune_keras_rnn_predict(
  data = apple,
  model_type = "simple",
  cv_setting = cv_setting,
  bayes_best_par = bayes_best_par,
  col_id = NULL,
  col_date = "index",
  col_value = "value",
  level = 95,
  iter = 1,
  iter_dropout = 5,
  save_model = tmp,
  save_model_id = "test-save-simple"
)

expect_identical(names(result_check_simple), names(result_simple))
expect_identical(names(result_check_simple[[1]]), names(result_simple[[2]]))
expect_identical(dim(result_check_simple[[1]]), dim(result_simple[[2]]))

expect_identical(
  result_check_simple[[1]][, c("index", "key", "type")],
  result_simple[[1]][, c("index", "key", "type")],
)
expect_identical(
  result_check_simple[[2]][, c("index", "key", "type")],
  result_simple[[2]][, c("index", "key", "type")],
)

tmp_files <- list.files(tmp, pattern = "\\w+_test-save-simple_\\w+\\.hdf5$", full.names = TRUE)
expect_identical(length(tmp_files), 2L)

test_results_simple <- sapply(tmp_files, function(file) {
  model <- keras::load_model_hdf5(file)
  test01 <- expect_equivalent(model, model_simple)
  test02 <- expect_equivalent(keras::get_config(model), keras::get_config(model_simple))
  return(c(test01, test02))
}, simplify = TRUE, USE.NAMES = FALSE)
expect_true(all(test_results_simple))
ignore(expect_true)(all(file.remove(tmp_files)))

### GRU
result_check_gru <- tune_keras_rnn_predict(
  data = apple_ticker,
  model_type = "gru",
  cv_setting = cv_setting,
  bayes_best_par = bayes_best_par,
  col_id = "ticker",
  col_date = "index",
  col_value = "value",
  level = 95,
  iter = 1,
  iter_dropout = 5,
  save_model = tmp,
  save_model_id = "test-save-gru"
)

expect_identical(names(result_check_gru), names(result_gru))
expect_identical(names(result_check_gru[[1]]), names(result_gru[[2]]))
expect_identical(dim(result_check_gru[[1]]), dim(result_gru[[2]]))

expect_identical(
  result_check_gru[[1]][, c("index", "ticker", "key", "type")],
  result_gru[[1]][, c("index", "ticker", "key", "type")],
)
expect_identical(
  result_check_gru[[2]][, c("index", "ticker", "key", "type")],
  result_gru[[2]][, c("index", "ticker", "key", "type")],
)

tmp_files <- list.files(tmp, pattern = "\\w+_test-save-gru_\\w+\\.hdf5$", full.names = TRUE)
expect_identical(length(tmp_files), 2L)

test_results_gru <- sapply(tmp_files, function(file) {
  model <- keras::load_model_hdf5(file)
  test01 <- expect_equivalent(model, model_gru)
  test02 <- expect_equivalent(keras::get_config(model), keras::get_config(model_gru))
  return(c(test01, test02))
}, simplify = TRUE, USE.NAMES = FALSE)
expect_true(all(test_results_gru))
ignore(expect_true)(all(file.remove(tmp_files)))

# Error ------------------------------------------------------------------------

### "data"
expect_error(
  tune_keras_rnn_predict(data),
  class = "tune_keras_rnn_predict_data_error",
  pattern = "^`data` must be data.frame, not of class \"function\"\\.$"
)

### "model_type"
expect_error(
  tune_keras_rnn_predict(apple, model_type = TRUE),
  class = "tune_keras_rnn_predict_model_type_error",
  pattern = "^`model_type` must be character\\(1\\), not of class \"logical\\(1\\)\"\\.$"
)
expect_error(
  tune_keras_rnn_predict(apple, model_type = c("ltsm")),
  class = "rlang_error",
  pattern = "^`model_type` must be one of \"simple\", \"gru\", or \"lstm\"\\."
)
expect_error(
  tune_keras_rnn_predict(apple, model_type = c("simple", "lstm")),
  class = "tune_keras_rnn_predict_model_type_error",
  pattern = "^`model_type` must be character\\(1\\), not of class \"character\\(2\\)\"\\.$"
)

### "cv_setting"

# 1. wrong class
expect_error(
  tune_keras_rnn_predict(apple, "simple", cv_setting = as.data.frame(cv_setting)),
  class = "tune_keras_rnn_predict_cv_setting_error",
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
  tune_keras_rnn_predict(apple, "simple", cv_setting_fail, bayes_best_par),
  class = "tune_keras_rnn_predict_cv_setting_error",
  pattern = cv_setting_name_pattern
)

cv_setting_fail <- cv_setting
names(cv_setting_fail)[1] <- "periods_trai"
expect_error(
  tune_keras_rnn_predict(apple, "simple", cv_setting_fail, bayes_best_par),
  class = "tune_keras_rnn_predict_cv_setting_error",
  pattern = cv_setting_name_pattern
)

cv_setting_fail <- cv_setting
names(cv_setting_fail)[2] <- "period_val"
expect_error(
  tune_keras_rnn_predict(apple, "simple", cv_setting_fail, bayes_best_par),
  class = "tune_keras_rnn_predict_cv_setting_error",
  pattern = cv_setting_name_pattern
)

cv_setting_fail <- cv_setting
names(cv_setting_fail)[3] <- "periods_tst"
expect_error(
  tune_keras_rnn_predict(apple, "simple", cv_setting_fail, bayes_best_par),
  class = "tune_keras_rnn_predict_cv_setting_error",
  pattern = cv_setting_name_pattern
)

cv_setting_fail <- cv_setting
names(cv_setting_fail)[4] <- "span_skip"
expect_error(
  tune_keras_rnn_predict(apple, "simple", cv_setting_fail, bayes_best_par),
  class = "tune_keras_rnn_predict_cv_setting_error",
  pattern = cv_setting_name_pattern
)

names(cv_setting_fail) <- NULL
expect_error(
  tune_keras_rnn_predict(apple, "simple", cv_setting_fail, bayes_best_par),
  class = "tune_keras_rnn_predict_cv_setting_error",
  pattern = cv_setting_name_pattern
)

# 3. `cv_setting`: Check types
cv_setting_fail <- cv_setting
cv_setting_fail[["periods_train"]] <- "90"
expect_error(
  tune_keras_rnn_predict(apple, "simple", cv_setting_fail, bayes_best_par),
  class = "tune_keras_rnn_predict_cv_setting_error",
  pattern = "^`periods_train` in `cv_setting` must be numeric\\(1\\), not character\\(1\\)\\.$"
)

cv_setting_fail <- cv_setting
cv_setting_fail[["periods_val"]] <- data
expect_error(
  tune_keras_rnn_predict(apple, "simple", cv_setting_fail, bayes_best_par),
  class = "tune_keras_rnn_predict_cv_setting_error",
  pattern = "^`periods_val` in `cv_setting` must be numeric\\(1\\), not function\\(1\\)\\.$"
)

cv_setting_fail <- cv_setting
cv_setting_fail[["periods_test"]] <- TRUE
expect_error(
  tune_keras_rnn_predict(apple, "simple", cv_setting_fail, bayes_best_par),
  class = "tune_keras_rnn_predict_cv_setting_error",
  pattern = "^`periods_test` in `cv_setting` must be numeric\\(1\\), not logical\\(1\\)\\.$"
)

cv_setting_fail <- cv_setting
cv_setting_fail[["skip_span"]] <- numeric()
expect_error(
  tune_keras_rnn_predict(apple, "simple", cv_setting_fail, bayes_best_par),
  class = "tune_keras_rnn_predict_cv_setting_error",
  pattern = "^`skip_span` in `cv_setting` must be numeric\\(1\\), not numeric\\(0\\)\\.$"
)

### "bayes_best_par"
expect_error(
  tune_keras_rnn_predict(apple, "simple", cv_setting, bayes_best_par = as.data.frame(bayes_best_par)),
  class = "tune_keras_rnn_predict_bayes_best_par_error",
  pattern = "^`bayes_best_par` must be list, not of class \"data.frame\"\\.$"
)

bayes_best_par_fail <- bayes_best_par
names(bayes_best_par_fail) <- c("Slice", "Slice")
expect_error(
  tune_keras_rnn_predict(apple, "simple", cv_setting, bayes_best_par = bayes_best_par_fail),
  class = "tune_keras_rnn_predict_bayes_best_par_error",
  pattern = "^`bayes_best_par` must be a list named by each `rsample` split\\.$"
)

# missing
bayes_best_par_fail <- bayes_best_par
bayes_best_par_fail[["Slice2"]] <- bayes_best_par_fail[["Slice2"]][-2]
expect_error(
  tune_keras_rnn_predict(apple, "simple", cv_setting, bayes_best_par_fail),
  class = "tune_keras_rnn_predict_bayes_best_par_error",
  pattern = "^`bayes_best_par\\[\\[\"Slice2\"\\]\\] must contain all required parameters\\.\nMisses \"lag_2\"\\.$"
)

# wrong class (character)
bayes_best_par_fail <- bayes_best_par
bayes_best_par_fail[["Slice2"]][["n_units"]] <- "2"
test_01 <- expect_error(
  tune_keras_rnn_predict(apple, "simple", cv_setting, bayes_best_par_fail),
  class = "tune_keras_rnn_predict_bayes_best_par_error",
  pattern = "^`bayes_best_par\\[\\[\"Slice2\"\\]\\]` must be numeric, not of class \"character\"\\.$"
)

### "col_id"
expect_error(
  tune_keras_rnn_predict(apple, "simple", cv_setting, bayes_best_par, col_id = 12),
  class = "tune_keras_rnn_predict_col_id_error",
  pattern = "^`col_id` must be character\\(1\\), not of class \"numeric\\(1\\)\"\\.$"
)
expect_error(
  tune_keras_rnn_predict(apple, "simple", cv_setting, bayes_best_par, col_id = c("ticker", "id")),
  class = "tune_keras_rnn_predict_col_id_error",
  pattern = "^`col_id` must be character\\(1\\), not of class \"character\\(2\\)\"\\.$"
)
expect_error(
  tune_keras_rnn_predict(apple, "simple", cv_setting, bayes_best_par, col_id = "value"),
  class = "tune_keras_rnn_predict_col_id_error",
  pattern = "^Variable specified by `col_id` must be class \"character\"\\.$"
)
expect_error(
  tune_keras_rnn_predict(apple, "simple", cv_setting, bayes_best_par, col_id = "index"),
  class = "tune_keras_rnn_predict_col_id_error",
  pattern = "^Variable specified by `col_id` must be class \"character\"\\.$"
)
data_fail <- data.table::copy(apple)[, ticker := 12]
expect_error(
  tune_keras_rnn_predict(data_fail, "simple", cv_setting, bayes_best_par, col_id = "index"),
  class = "tune_keras_rnn_predict_col_id_error",
  pattern = "^Variable specified by `col_id` must be class \"character\"\\.$"
)

### "col_date"
expect_error(
  tune_keras_rnn_predict(apple, "simple", cv_setting, bayes_best_par, col_date = 12),
  class = "tune_keras_rnn_predict_col_date_error",
  pattern = "^`col_date` must be character\\(1\\), not of class \"numeric\\(1\\)\"\\.$"
)
expect_error(
  tune_keras_rnn_predict(apple, "simple", cv_setting, bayes_best_par, col_date = c("index", "id")),
  class = "tune_keras_rnn_predict_col_date_error",
  pattern = "^`col_date` must be character\\(1\\), not of class \"character\\(2\\)\"\\.$"
)
expect_error(
  tune_keras_rnn_predict(apple[,2], "simple", cv_setting, bayes_best_par, col_date = "index", col_value = "value"),
  class = "tune_keras_rnn_predict_col_date_error",
  pattern = "^Variable specified by `col_date` must be class \"Date\" or \"POSIXct\"\\.$"
)

### "col_value"
expect_error(
  tune_keras_rnn_predict(apple, "simple", cv_setting, bayes_best_par, col_value = 12),
  class = "tune_keras_rnn_predict_col_value_error",
  pattern = "^`col_value` must be character\\(1\\), not of class \"numeric\\(1\\)\"\\.$"
)
expect_error(
  tune_keras_rnn_predict(apple, "simple", cv_setting, bayes_best_par, col_value = c("value", "value2")),
  class = "tune_keras_rnn_predict_col_value_error",
  pattern = "^`col_value` must be character\\(1\\), not of class \"character\\(2\\)\"\\.$"
)
expect_error(
  tune_keras_rnn_predict(apple[,1], "simple", cv_setting, bayes_best_par, col_date = "index", col_value = "value"),
  class = "tune_keras_rnn_predict_col_value_error",
  pattern = "^Variable specified by `col_value` must be class \"numeric\"\\.$"
)
expect_error(
  tune_keras_rnn_predict(apple[,1], "simple", cv_setting, bayes_best_par, col_date = "index", col_value = "value"),
  class = "tune_keras_rnn_predict_col_value_error",
  pattern = "^Variable specified by `col_value` must be class \"numeric\"\\.$"
)
expect_error(
  tune_keras_rnn_predict(apple_ticker, "simple", cv_setting, bayes_best_par, col_date = "index", col_value = "ticker"),
  class = "tune_keras_rnn_predict_col_value_error",
  pattern = "^Variable specified by `col_value` must be class \"numeric\"\\.$"
)

### "level"
expect_error(
  tune_keras_rnn_predict(apple, "simple", cv_setting, bayes_best_par, level = NULL),
  class = "tune_keras_rnn_predict_level_error",
  pattern = "^`level` must be numeric\\(1\\) or integer\\(1\\), not of class \"NULL\\(0\\)\"\\.$"
)
expect_error(
  tune_keras_rnn_predict(apple, "simple", cv_setting, bayes_best_par, level = c(80, 90)),
  class = "tune_keras_rnn_predict_level_error",
  pattern = "^`level` must be numeric\\(1\\) or integer\\(1\\), not of class \"numeric\\(2\\)\"\\.$"
)
expect_error(
  tune_keras_rnn_predict(apple, "simple", cv_setting, bayes_best_par, level = -5),
  class = "tune_keras_rnn_predict_level_error",
  pattern = "^`level` must be within interval \\(0, 100\\)\\.$"
)
expect_error(
  tune_keras_rnn_predict(apple, "simple", cv_setting, bayes_best_par, level = 110),
  class = "tune_keras_rnn_predict_level_error",
  pattern = "^`level` must be within interval \\(0, 100\\)\\.$"
)

### "iter"
expect_error(
  tune_keras_rnn_predict(apple, "simple", cv_setting, bayes_best_par, iter = NULL),
  class = "tune_keras_rnn_predict_iter_error",
  pattern = "^`iter` must be numeric\\(1\\) or integer\\(1\\), not of class \"NULL\\(0\\)\"\\.$"
)
expect_error(
  tune_keras_rnn_predict(apple, "simple", cv_setting, bayes_best_par, iter = c(100, 200)),
  class = "tune_keras_rnn_predict_iter_error",
  pattern = "^`iter` must be numeric\\(1\\) or integer\\(1\\), not of class \"numeric\\(2\\)\"\\.$"
)
expect_error(
  tune_keras_rnn_predict(apple, "simple", cv_setting, bayes_best_par, iter = -5),
  class = "tune_keras_rnn_predict_iter_error",
  pattern = "^`iter` must be a positive integer\\.$"
)

### "iter_dropout"
expect_error(
  tune_keras_rnn_predict(apple, "simple", cv_setting, bayes_best_par, iter_dropout = NULL),
  class = "tune_keras_rnn_predict_iter_dropout_error",
  pattern = "^`iter_dropout` must be numeric\\(1\\) or integer\\(1\\), not of class \"NULL\\(0\\)\"\\.$"
)
expect_error(
  tune_keras_rnn_predict(apple, "simple", cv_setting, bayes_best_par, iter_dropout = c(100, 200)),
  class = "tune_keras_rnn_predict_iter_dropout_error",
  pattern = "^`iter_dropout` must be numeric\\(1\\) or integer\\(1\\), not of class \"numeric\\(2\\)\"\\.$"
)
expect_error(
  tune_keras_rnn_predict(apple, "simple", cv_setting, bayes_best_par, iter_dropout = -5),
  class = "tune_keras_rnn_predict_iter_dropout_error",
  pattern = "^`iter_dropout` must be a positive integer\\.$"
)

### "save_model"
expect_error(
  tune_keras_rnn_predict(apple, "simple", cv_setting, bayes_best_par, save_model = TRUE),
  class = "tune_keras_rnn_predict_save_model_error",
  pattern = "^`save_model` must be character\\(1\\), not of class \"logical\\(1\\)\"\\.$"
)
expect_error(
  tune_keras_rnn_predict(apple, "simple", cv_setting, bayes_best_par, save_model = file.path(tempdir(), "XXX")),
  class = "tune_keras_rnn_predict_save_model_error",
  pattern = "^Directory specified in `save_model` does not exist\\.$"
)
expect_error(
  tune_keras_rnn_predict(apple, "simple", cv_setting, bayes_best_par, save_model = file.path(tempdir(), "file.rda")),
  class = "tune_keras_rnn_predict_save_model_error",
  pattern = "^Directory specified in `save_model` does not exist\\.$"
)

### "save_model_id"
expect_error(
  tune_keras_rnn_predict(apple, "simple", cv_setting, bayes_best_par, save_model_id = 12),
  class = "tune_keras_rnn_predict_save_model_id_error",
  pattern = "^`save_model_id` must be character\\(1\\), not of class \"numeric\\(1\\)\"\\.$"
)
