#' Timeseries data preparation for neural network Keras models
#'
#' The following steps are proceeded:
#' - New column "lag_value", lagged by `lag_setting`
#' - Split in train, validation and test split
#' - Transformation in arrays for Keras
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
  lag_setting = 8,
  length_val = 16,
  length_test = 8
) {

  n <- nrow(data)
  n_train <- 1:(n - (length_val + length_test) - lag_setting)
  n_val <- (n - (length_val + length_test) - lag_setting):(n - length_test - lag_setting)
  n_val <- n_val[-1]
  n_test <- (n - length_test - lag_setting):(n - lag_setting)
  n_test <- n_test[-1]

  # 2D And 3D Train/Test Arrays
  data[, value_lag := shift(value, n = lag_setting, type = "lag")]
  data <- data[!is.na(value_lag)]

  train_DT <- data[n_train]
  x_train_vec <- train_DT$value_lag
  x_train_arr <- array(data = x_train_vec, dim = c(length(x_train_vec), 1, 1))

  y_train_vec <- train_DT$value
  y_train_arr <- array(data = y_train_vec, dim = c(length(y_train_vec), 1))

  val_DT <- data[n_val]
  x_val_vec <- val_DT$value_lag
  x_val_arr <- array(data = x_val_vec, dim = c(length(x_val_vec), 1, 1))

  y_val_vec <- val_DT$value
  y_val_arr <- array(data = y_val_vec, dim = c(length(y_val_vec), 1))

  test_DT <- data[n_test]
  x_test_vec <- test_DT$value_lag
  x_test_arr <- array(data = x_test_vec, dim = c(length(x_test_vec), 1, 1))

  y_test_vec <- test_DT$value
  y_test_arr <- array(data = y_test_vec, dim = c(length(y_test_vec), 1))

  return(list(
    x = list(train = x_train_arr, val = x_val_arr, test = x_test_arr),
    y = list(train = y_train_arr, val = y_val_arr, test = y_test_arr)
  ))
}
