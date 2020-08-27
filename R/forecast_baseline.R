#' baseline models to get benchmark results for advanced models
#'
#' Get results for the following simple forecasting methods:
#' - Naive forecast (\link[forecast]{naive})
#' - Seasonal naive forecast (\link[forecast]{snaive})
#' - Mean forecast (\link[forecast]{meanf})
#' - Simple exponential smoothing forecasts (\link[forecast]{ses})
#' - Exponential smoothing with Holt's trend (\link[forecast]{holt})
#'
#' @param data Univariate time series object
#' @param h atomic vector specifying forecasting horizon
#' @param acc_measure Accuracy indicator. Valid options are "ME", "RMSE", "MAE",
#'   "MPE", "MAPE", "MASE" and "ACF1"
#'
#' @return
#' @export
#'
#' @examples
forecast_baseline <- function(data, h = NULL, acc_measure) {

  ### Checks -------------------------------------------------------------------

  # Variable data
  testr::check_class(data, "ts", forecast_baseline)
  if (rlang::inherits_any(h, c("numeric", "integer"))) {rlang::abort(
      message = sprintf(
        "`h` must be numeric or integer, not of class \"%s\".",
        paste(class(data), collapse = " / ")
      ),
      class = "forecast_baseline_data_error"
  )}

  # Variable h
  if (is.null(h)) h <- 2 * stats::frequency(data)
  if (!is.atomic(h)) {rlang::abort(
    message = "`h` must be an atomic vector.",
    class = "forecast_baseline_h_error"
  )}

  # Variable acc_measure
  testr::check_class(acc_measure, "character", forecast_baseline)
  rlang::arg_match(c("ME", "RMSE", "MAE", "MPE", "MAPE", "MASE", "ACF1"))

  ### Function -----------------------------------------------------------------

  fc_list <- list(
    fc_naive = forecast::naive(data, h = h),
    fc_snaive = forecast::snaive(data, h = h),
    fc_mean = forecast::meanf(data, h = h),
    fc_ses = forecast::ses(data, h = h),
    fc_holt = forecast::holt(data, h = h)
  )

  acc_list <- lapply(fc_list, function(result) {
    acc <- as.data.frame(forecast::accuracy(result))
    acc[, acc_measure]
  })
}
