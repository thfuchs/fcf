#' Absolute coverage difference (ACD)
#'
#' @param actual actual values (only test set)
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
#' @param data actual values (containing both train and evaluation values)
#' @param lower lower bound of prediction interval
#' @param upper upper bound of prediction interval
#' @param h forecast horizon
#' @param m frequency, e.g. 12 for monthly and 4 for quarterly series
#' @param level level used for prediction interval construction
#'
#' @family PI accuracy measures
#' @return numeric vector of length 1
#' @export
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
#'
smis <- function(data, lower, upper, h, m, level) {
  n <- length(data)
  n_train <- n - h
  train <- data[1:n_train]
  test <- data[(n_train + 1):n]

  alpha <- 1 - level
  scale <- 1 / (n_train - m) * sum(abs(train[(m + 1):n_train] - train[1:(n_train - m)]))

  MIS <-
    sum(as.vector(upper) - as.vector(lower)) + 2 / alpha * (
      sum((as.vector(lower) - as.vector(test)) * (as.vector(test) < as.vector(lower))) +
        sum((as.vector(test) - as.vector(upper)) * (as.vector(test) > as.vector(upper)))
    )
  MIS <- MIS / h
  SMIS <- MIS / scale

  return(SMIS)
}
