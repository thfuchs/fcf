#' Mean Absolute Scaled Error (MASE)
#'
#' Mean Absolute Scaled Error (MASE), scaled according to M4 Forecasting
#' competition
#'
#' @param data time series (only train set)
#' @param actual actual values (Equivalent of forecast)
#' @param forecast predicted values
#' @param m frequency, e.g. 12 for monthly and 4 for quarterly series
#'
#' @family PF accuracy measures
#' @return numeric vector of length 1
#'
#' @references \itemize{
#'   \item 	Svetunkov, I., Sagaert, Y. R. (2020). greybox: Toolbox for Model
#'   Building and Forecasting. \url{https://cran.r-project.org/package=greybox}
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
#'
#' mase(data = data_train, actual = data_test, forecast = forecast, m = 4)
#'
mase <- function(data, actual, forecast, m) {

  n_train <- length(data)
  scale <- 1 / (n_train - m) * sum(abs(data[(m + 1):n_train] - data[1:(n_train - m)]))

  MASE <- mean(abs(as.vector(actual) - as.vector(forecast))) / scale
  return(MASE)
}
#' Mean Absolute Percentage Error (MAPE)
#'
#' @param actual actual values (Equivalent of forecast)
#' @param forecast predicted values
#'
#' @family PF accuracy measures
#' @return numeric vector of length 1
#' @export
#'
#' @references \itemize{
#'   \item 	Svetunkov, I., Sagaert, Y. R. (2020). greybox: Toolbox for Model
#'   Building and Forecasting. \url{https://cran.r-project.org/package=greybox}
#'   \item Makridakis, S., Spiliotis, E., & Assimakopoulos, V. (2020). The M4
#'   Competition: 100,000 time series and 61 forecasting methods. International
#'   Journal of Forecasting, 36(1), 54–74. \url{https://doi.org/10.1016/j.ijforecast.2019.04.014}
#' }
#'
mape <- function(actual, forecast) {
  stopifnot(identical(length(actual), length(forecast)))

  MAPE <- mean(abs((as.vector(actual) - as.vector(forecast)) / as.vector(actual)))
  return(MAPE * 100)
}
#' symmetric Mean Absolute Percentage Error (sMAPE)
#'
#' @param actual actual values (Equivalent of forecast)
#' @param forecast predicted values
#'
#' @family PF accuracy measures
#' @return numeric vector of length 1
#' @export
#'
#' @references \itemize{
#'   \item 	Svetunkov, I., Sagaert, Y. R. (2020). greybox: Toolbox for Model
#'   Building and Forecasting. \url{https://cran.r-project.org/package=greybox}
#'   \item Makridakis, S., Spiliotis, E., & Assimakopoulos, V. (2020). The M4
#'   Competition: 100,000 time series and 61 forecasting methods. International
#'   Journal of Forecasting, 36(1), 54–74. \url{https://doi.org/10.1016/j.ijforecast.2019.04.014}
#' }
#'
smape <- function(actual, forecast) {
  stopifnot(identical(length(actual), length(forecast)))

  h <- length(forecast)

  sMAPE <- 2 / h * sum(abs(actual - forecast) / (abs(actual) + abs(forecast)))
  return(sMAPE * 100)
}
