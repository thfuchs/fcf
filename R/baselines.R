#' Forecast with baseline models
#'
#' Get forecast results for the following simple forecasting methods:
#'   - Naive forecast (\link[forecast]{naive})
#'   - Seasonal naive forecast
#'   - Mean forecast (\link[forecast]{meanf})
#'   - Simple exponential smoothing forecasts (\link[forecast]{ses})
#'   - Exponential smoothing with Holt's trend
#'
#' @param data Univariate time series (ts object)
#' @param test_size Numeric vector of length one specifying length of test set
#'   (along with forecasting horizon). `NULL` by default which yield 2 times the
#'    frequency of the time series
#' @param acc_measure Accuracy indicator. Valid options are "ME", "RMSE", "MAE",
#'   "MPE", "MAPE", "MASE" and "ACF1"
#'
#' @return list of forecasting methods with data.frame for accuracy indicators
#' @export
#'
#' @examples
#' # Calculate MAE and RMSE of simple forecasting models for Apple FCF
#' apple <- tsRNN::ts_apple
#' baselines(apple, acc_measure = c("MAE", "RMSE"))
#'
#' # Change forecast horizon to 4 years
#' baselines(apple, test_size = 16, acc_measure = c("MAE", "RMSE"))
baselines <- function(data, test_size = NULL, acc_measure) {

  ### Checks -------------------------------------------------------------------

  # Variable `data`
  testr::check_class(data, "ts")

  # Variable `test_size`
  if (is.null(test_size)) test_size <- 2 * stats::frequency(data)
  if (!rlang::inherits_any(test_size, c("numeric", "integer"))) {
    rlang::abort(
      message = sprintf(
        "`test_size` must be numeric or integer, not of class \"%s\".",
        paste(class(test_size), collapse = " / ")
      ),
      class = "baselines_test_size_error"
    )
  }
  if (length(test_size) != 1) {
    rlang::abort(
      message = "`test_size` must be a vector of length 1.",
      class = "baselines_test_size_error"
    )
  }
  if (test_size < 0) {
    rlang::abort(
      message = sprintf(
        "`test_size` must be NULL or a positive numeric, not \"%s\".", test_size
      ),
      class = "baselines_test_size_error"
    )
  }

  # Variable `acc_measure`
  testr::check_class(acc_measure, "character")
  if (
    !all(acc_measure %in% c("ME", "RMSE", "MAE", "MPE", "MAPE", "MASE", "ACF1"))
  ) {
    rlang::abort(
      message = "`acc_measure` must be one of \"ME\", \"RMSE\", \"MAE\",
        \"MPE\", \"MAPE\", \"MASE\", \"ACF1\"",
      value = acc_measure,
      class = "baselines_acc_measure_error"
    )
  }

  ### Function -----------------------------------------------------------------

  train_end <- stats::time(data)[length(data) - test_size]
  train <- stats::window(data, end = train_end)

  fc_list <- list(
    fc_naive = forecast::naive(train, h = test_size),
    fc_snaive = forecast::snaive(train, h = test_size),
    fc_mean = forecast::meanf(train, h = test_size),
    fc_ses = forecast::ses(train, h = test_size),
    fc_holt = forecast::holt(train, h = test_size)
  )

  acc_list <- lapply(fc_list, function(result) {
    acc <- as.data.frame(forecast::accuracy(result, data))
    acc[, acc_measure]
  })

  return(acc_list)
}
