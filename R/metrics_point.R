# Point Forecast Accuracy Measures

#' Mean Absolute Scaled Error (MASE), scaled according to M4 Forecasting
#' competition
#'
#' @param data actual values (containing both train and evaluation values)
#' @param forecast forecasted values
#' @param m frequency, e.g. 12 for monthly and 4 for quarterly series
#'
#' @return vector of length 1
#' @export
#' @rdname metrics_point
#'
#' @references \itemize{
#'   \item \link[greybox]{sMIS}
#'   \item Makridakis, S., Spiliotis, E., & Assimakopoulos, V. (2020). The M4
#'   Competition: 100,000 time series and 61 forecasting methods. International
#'   Journal of Forecasting, 36(1), 54–74. \url{https://doi.org/10.1016/j.ijforecast.2019.04.014}
#' }
#'
mase <- function(data, forecast, m) {

  n <- length(data)
  h <- length(forecast)
  n_train <- n-h
  train <- data[1:n_train]
  test <- data[(n_train+1):n]

  scale <- 1/(n_train-m) * sum(abs(train[(m+1):n_train] - train[1:(n_train-m)]))

  MASE <- mean(abs(as.vector(test)-as.vector(forecast)),na.rm=TRUE)/scale
  return(MASE)
}

#' Mean Absolute Percentage Error (MAPE)
#'
#' @param actual actual values (only test set)
#' @param forecast forecasted values
#'
#' @return
#' @export
#' @rdname metrics_point
mape <- function(actual, forecast) {
  stopifnot(identical(length(actual), length(forecast)))

  MAPE <- mean(abs((as.vector(actual)-as.vector(forecast))/as.vector(actual)),na.rm=TRUE)
  return(MAPE * 100)
}

#' symmetric Mean Absolute Percentage Error (sMAPE)
#'
#' @param actual actual values (only test set)
#' @param forecast forecasted values
#'
#' @return
#' @export
#' @rdname metrics_point
smape <- function(actual, forecast) {
  stopifnot(identical(length(actual), length(forecast)))

  h <- length(forecast)

  sMAPE <- 2/h * sum(abs(actual - forecast) / (abs(actual) + abs(forecast)))
  return(sMAPE * 100)
}
