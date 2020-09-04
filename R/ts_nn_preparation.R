#' Timeseries data preparation for neural network Keras models
#'
#' The following steps are proceeded:
#' - New columns "value_lagX", with X for #lags, specified by `lag_setting`
#' - Split in train, validation and test split
#' - Transformation to 3D arrays for Keras
#'
#' @param data balanced timeseries data
#' @param lag_setting by how much is y lagged
#' @param length_val length of validation set
#' @param length_test length of test set
#'
#' @return list of "X" and "Y" each containing "train", "val" and "test" arrays
#' @export
#'
#' @examples
#'
ts_nn_preparation <- function(
  data,
  lag_setting = 1:4,
  length_val = 16,
  length_test = 8
) {

  n <- nrow(data)
  max_lag <- max(lag_setting)
  n_train <- 1:(n - (length_val + length_test) - max_lag)
  n_val <- (n - (length_val + length_test) - max_lag):(n - length_test - max_lag)
  n_val <- n_val[-1]
  n_test <- (n - length_test - max_lag):(n - max_lag)
  n_test <- n_test[-1]

  # 2D And 3D Train/Test Arrays
  add_shift(data, cols = "value", nlags = lag_setting, type = "lag")
  message(sprintf(
    "%s lag(s) created. Now %s rows missing - removing.",
    max_lag,
    data[is.na(get(paste0("value_lag", max_lag))), .N]
  ))
  data <- data[!is.na(get(paste0("value_lag", max_lag)))]

  train_DT <- data[n_train]
  x_train_mat <- as.matrix(train_DT[, .SD, .SDcols = patterns("^value_lag")])
  x_train_dim <- c(nrow(x_train_mat), length(lag_setting), 1)
  x_train_arr <- array(data = x_train_mat, dim = x_train_dim)

  y_train_vec <- train_DT$value
  y_train_arr <- array(data = y_train_vec, dim = c(length(y_train_vec), 1))

  val_DT <- data[n_val]
  x_val_mat <- as.matrix(val_DT[, .SD, .SDcols = patterns("^value_lag")])
  x_val_dim <- c(nrow(x_val_mat), length(lag_setting), 1)
  x_val_arr <- array(data = x_val_mat, dim = x_val_dim)

  y_val_vec <- val_DT$value
  y_val_arr <- array(data = y_val_vec, dim = c(length(y_val_vec), 1))

  test_DT <- data[n_test]
  x_test_mat <- as.matrix(test_DT[, .SD, .SDcols = patterns("^value_lag")])
  x_test_dim <- c(nrow(x_test_mat), length(lag_setting), 1)
  x_test_arr <- array(data = x_test_mat, dim = x_test_dim)

  y_test_vec <- test_DT$value
  y_test_arr <- array(data = y_test_vec, dim = c(length(y_test_vec), 1))

  return(list(
    x = list(train = x_train_arr, val = x_val_arr, test = x_test_arr),
    y = list(train = y_train_arr, val = y_val_arr, test = y_test_arr)
  ))
}
