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
#'
#' @return list of "X" and "Y" each containing "train", "val" and "test" arrays
#' @export
#'
#' @examples
#' data <- tsRNN::DT_unh
#'
#' data[, value_lag1 := data.table::shift(value, type = "lag", n = 1)]
#' data[, value_lag2 := data.table::shift(value, type = "lag", n = 2)]
#' data <- data[!is.na(get(paste0("value_lag2")))]
#'
#' ts_nn_preparation(data, tsteps = 2L, length_val = 6L, length_test = 6L)
ts_nn_preparation <- function(data, tsteps, length_val = 16L, length_test = 8L) {

  ### Checks -------------------------------------------------------------------
  testr::check_class(data, "data.table", "ts_nn_preparation")
  testr::check_class(tsteps, "integer", "ts_nn_preparation")
  testr::check_class(length_val, "integer", "ts_nn_preparation")
  testr::check_class(length_test, "integer", "ts_nn_preparation")

  if (sum(names(data) %like% "value_lag[0-9]") < tsteps) {
    rlang::abort(
      message = "`data` must be a data.table with at least `tsteps` lagged columns",
      class = "ts_nn_preparation_data_error"
    )
  }
  if (length(tsteps) != 1) {
    rlang::abort(
      message = "`tsteps` must be an integer of length 1.",
      class = "ts_nn_preparation_tsteps_error"
    )
  }
  if (length(length_val) != 1) {
    rlang::abort(
      message = "`length_val` must be an integer of length 1.",
      class = "ts_nn_preparation_length_val_error"
    )
  }
  if (length(length_test) != 1) {
    rlang::abort(
      message = "`length_test` must be an integer of length 1.",
      class = "ts_nn_preparation_length_test_error"
    )
  }

  ### Function -----------------------------------------------------------------
  n <- nrow(data)
  n_train <- 1:(n - length_val - length_test)
  n_val <- if (length_val > 0) (n - length_val - length_test + 1):(n - length_test) else 0
  n_test <- if (length_test > 0) (n - length_test + 1):n else 0

  # 2D And 3D Train/Test Arrays
  patterns <- function(...) NULL # to address data.table R CMD check Note

  train_DT <- data[n_train]
  x_train_mat <- as.matrix(train_DT[, .SD, .SDcols = patterns("^value_lag")])
  x_train_dim <- c(nrow(x_train_mat), tsteps, 1)
  x_train_arr <- array(data = x_train_mat, dim = x_train_dim)

  y_train_vec <- train_DT$value
  y_train_arr <- array(data = y_train_vec, dim = c(length(y_train_vec), 1))

  val_DT <- data[n_val]
  x_val_mat <- as.matrix(val_DT[, .SD, .SDcols = patterns("^value_lag")])
  x_val_dim <- c(nrow(x_val_mat), tsteps, 1)
  x_val_arr <- array(data = x_val_mat, dim = x_val_dim)

  y_val_vec <- val_DT$value
  y_val_arr <- array(data = y_val_vec, dim = c(length(y_val_vec), 1))

  test_DT <- data[n_test]
  x_test_mat <- as.matrix(test_DT[, .SD, .SDcols = patterns("^value_lag")])
  x_test_dim <- c(nrow(x_test_mat), tsteps, 1)
  x_test_arr <- array(data = x_test_mat, dim = x_test_dim)

  y_test_vec <- test_DT$value
  y_test_arr <- array(data = y_test_vec, dim = c(length(y_test_vec), 1))

  return(list(
    x = list(train = x_train_arr, val = x_val_arr, test = x_test_arr),
    y = list(train = y_train_arr, val = y_val_arr, test = y_test_arr)
  ))
}
