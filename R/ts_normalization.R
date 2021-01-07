#' Normalize univariate timeseries
#'
#' @param DT data.table object with columns "index" and "value"
#' @param length_val length for validation set
#' @param length_test length for test set
#' @param metrics return data only or list of data and metrics?
#'
#' @import data.table
#'
#' @return Depending on `metrics`, processed DT object or list of "data" and
#'   "metrics" (center and scale)
#' @export
ts_normalization <- function(DT, length_val, length_test, metrics = FALSE) {
  n <- nrow(DT) - length_val - length_test
  train <- DT[1:n, ]

  mean <- mean(train$value)
  std <- stats::sd(train$value)

  data <- data.table::copy(DT)

  cols <- names(data)[grepl("^value", names(data))]
  data[, (cols) := lapply(.SD, function(x) scale(x, center = mean, scale = std)[, 1]), .SDcols = cols]

  if (!metrics) {
    return(data)
  }

  return(list(data = data, metrics = list(center = mean, scale = std)))
}
