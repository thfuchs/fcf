#' Timeseries data preparation for neural network Keras models
#'
#' The following steps are proceeded:
#'   - Split in train, validation and test split
#'   - Transformation to 3D arrays for Keras
#'
#' @param data balanced univariate time series as data.table object. Lagged
#'   columns in the form `value_lagX` required, where X represents number of lag
#' @param tsteps number of lagged time steps
#' @param length_val length of validation set
#' @param length_test length of test set
#' @param value_col name of column with value to prepare
#' @param value_lag_col name of lagged value column(s) to prepare, searched by
#'   starting pattern. E.g. `value_lag_col = "value_lag"` will catch column
#'   "value_lag" and "value_lag1" but not "2_value_lag"
#'
#' @return list of "X" and "Y" each containing "train", "val" and "test" arrays
#' @export
#'
#' @examples
#' data <- tsRNN::DT_apple
#'
#' data[, value_lag1 := data.table::shift(value, type = "lag", n = 1)]
#' data[, value_lag2 := data.table::shift(value, type = "lag", n = 2)]
#' data <- data[!is.na(get(paste0("value_lag2")))]
#'
#' ts_nn_preparation(data, tsteps = 2L, length_val = 6L, length_test = 6L)
ts_nn_preparation <- function(data,
                              tsteps,
                              length_val = 16L,
                              length_test = 8L,
                              value_col = "value",
                              value_lag_col = "value_lag") {

  ### Checks -------------------------------------------------------------------
  testr::check_class(data, "data.frame")
  testr::check_num_int(tsteps, n = 1)
  testr::check_num_int(length_val, n = 1)
  testr::check_num_int(length_test, n = 1)
  testr::check_class(value_col, "character", n = 1)
  testr::check_class(value_lag_col, "character", n = 1)

  DT <- data.table::copy(data)
  data.table::setDT(DT)
  n <- nrow(DT)

  if (n - length_val - length_test <= 0) rlang::abort(
    message = paste(
      "Number of rows of \"data\" must be higher then sum of \"length_val\"",
      "and \"length_test\"."),
    class = "ts_nn_preparation_data_error"
  )

  if (tsteps <= 0) rlang::abort(
    message = "`tsteps` must be positive.",
    class = "ts_nn_preparation_tsteps_error"
  )
  if (length_val < 0) rlang::abort(
    message = "`length_val` must be non-negative.",
    class = "ts_nn_preparation_length_val_error"
  )
  if (length_test < 0) rlang::abort(
    message = "`length_test` must be non-negative.",
    class = "ts_nn_preparation_length_test_error"
  )
  if (sum(names(DT) %like% "value_lag[0-9]") < tsteps) rlang::abort(
    message = "`data` must be a data.frame with at least `tsteps` lagged columns.",
    class = "ts_nn_preparation_data_error"
  )

  ### Function -----------------------------------------------------------------
  n_train <- 1:(n - length_val - length_test)
  n_val <- if (length_val > 0) (n - length_val - length_test + 1):(n - length_test) else 0
  n_test <- if (length_test > 0) (n - length_test + 1):n else 0

  # 2D And 3D Train/Test Arrays
  patterns <- function(...) NULL # to address data.table R CMD check Note
  lag_pattern <- paste0("^", value_lag_col)

  train_DT <- DT[n_train]
  x_train_mat <- as.matrix(train_DT[, .SD, .SDcols = patterns(lag_pattern)])
  x_train_dim <- c(nrow(x_train_mat), tsteps, 1)
  x_train_arr <- array(data = x_train_mat, dim = x_train_dim)

  y_train_vec <- train_DT[[value_col]]
  y_train_arr <- array(data = y_train_vec, dim = c(length(y_train_vec), 1))

  val_DT <- DT[n_val]
  x_val_mat <- as.matrix(val_DT[, .SD, .SDcols = patterns(lag_pattern)])
  x_val_dim <- c(nrow(x_val_mat), tsteps, 1)
  x_val_arr <- array(data = x_val_mat, dim = x_val_dim)

  y_val_vec <- val_DT[[value_col]]
  y_val_arr <- array(data = y_val_vec, dim = c(length(y_val_vec), 1))

  test_DT <- DT[n_test]
  x_test_mat <- as.matrix(test_DT[, .SD, .SDcols = patterns(lag_pattern)])
  x_test_dim <- c(nrow(x_test_mat), tsteps, 1)
  x_test_arr <- array(data = x_test_mat, dim = x_test_dim)

  y_test_vec <- test_DT[[value_col]]
  y_test_arr <- array(data = y_test_vec, dim = c(length(y_test_vec), 1))

  return(list(
    x = list(train = x_train_arr, val = x_val_arr, test = x_test_arr),
    y = list(train = y_train_arr, val = y_val_arr, test = y_test_arr)
  ))
}
