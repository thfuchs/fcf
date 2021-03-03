#' Absolute coverage difference (ACD)
#'
#' @param actual actual values (Equivalent of upper/lower)
#' @param lower lower bound of prediction interval
#' @param upper upper bound of prediction interval
#' @param level level used for prediction interval construction
#'
#' @family PI accuracy measures
#' @return numeric vector of length 1
#' @export
#'
#' @references \itemize{
#'   \item Makridakis, S., Spiliotis, E., & Assimakopoulos, V. (2020). The M4
#'   Competition: 100,000 time series and 61 forecasting methods. International
#'   Journal of Forecasting, 36(1), 54–74. \url{https://doi.org/10.1016/j.ijforecast.2019.04.014}
#' }
#'
acd <- function(actual, lower, upper, level) {
  stopifnot(identical(length(actual), length(lower), length(upper)))

  coverage <- sum(actual >= lower & actual <= upper) / length(actual)
  ACD <- abs(coverage - level)
  return(ACD)
}

#' scaled Mean Interval Score (sMIS)
#'
#' sMIS scaled according to M4 Forecasting competition (see references)
#'
#' @param data time series (only train set)
#' @param actual actual values (Equivalent of upper/lower)
#' @param lower lower bound of prediction interval
#' @param upper upper bound of prediction interval
#' @param m frequency, e.g. 12 for monthly and 4 for quarterly series
#' @param level level used for prediction interval construction
#'
#' @family PI accuracy measures
#' @return numeric vector of length 1
#'
#' @references \itemize{
#'   \item 	Svetunkov, I., Sagaert, Y. R. (2020). greybox: Toolbox for Model
#'   Building and Forecasting. \url{https://cran.r-project.org/package=greybox}
#'   \item Gneiting, T., & Raftery, A. E. (2007). Strictly proper scoring rules,
#'    prediction, and estimation. Journal of the American Statistical Association,
#'    102(477), 359–378. \url{https://doi.org/10.1198/016214506000001437}
#'   \item Makridakis, S., Spiliotis, E., & Assimakopoulos, V. (2020). The M4
#'   Competition: 100,000 time series and 61 forecasting methods. International
#'   Journal of Forecasting, 36(1), 54–74. \url{https://doi.org/10.1016/j.ijforecast.2019.04.014}
#' }
#' @export
#'
#' @examples
#' data <- tsRNN::fc_arima[key == "actual", value]
#' forecast <- tsRNN::fc_arima[key == "predict", value]
#' data_train <- data[1:(length(data) - length(forecast))]
#' data_test <- data[(length(data) - length(forecast) + 1):length(data)]
#' lower <- tsRNN::fc_arima[key == "predict", lo95]
#' upper <- tsRNN::fc_arima[key == "predict", hi95]
#'
#' tsRNN::smis(
#'   data = data_train,
#'   actual = data_test,
#'   lower = lower,
#'   upper = upper,
#'   m = 4,
#'   level = 0.95
#' )
#'
smis <- function(data, actual, lower, upper, m, level) {

  stopifnot(identical(length(actual), length(lower), length(upper)))

  n_train <- length(data)
  alpha <- 1 - level
  scale <- 1 / (n_train - m) * sum(abs(data[(m + 1):n_train] - data[1:(n_train - m)]))

  MIS <- mean(
    upper - lower +
      2 / alpha * (lower - actual) * (actual < lower) +
      2 / alpha * (actual - upper) * (actual > upper))
  SMIS <- MIS / scale

  return(SMIS)
}
