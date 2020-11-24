#' Check for autocorrelation
#'
#' Ljung-Box tests for 8, 12 and 16 lags
#'
#' @param data a numeric vector or univariate time series object
#' @param level significance level
#' @param lag_grid vector of lags to test on
#'
#' @return list with data.frames of p-values for both `Ljung-Box` and
#'   `Box-Pierce` tests results and logical vector `pass` which is TRUE if all
#'   tests passes and FALSE otherwise
#' @export
#'
#' @examples
#' apple <- tsRNN::ts_apple
#'
#' # With all default
#' check_acf(apple)
#'
#' # Significance level to 0.01 and 1 to 5 lags
#' check_acf(apple, level = 0.01, lag_grid = 1:5)
#'
check_acf <- function(data, level = 0.05, lag_grid = c(8, 12, 16)) {

  ### Checks -------------------------------------------------------------------

  # data
  if (!rlang::inherits_any(data, c("numeric", "ts"))) {
    rlang::abort(message = sprintf(
      "`data` must be a numeric vector or ts object, not of class \"%s\".",
      paste(class(data), collapse = " / ")
    ), class = "check_acf_data_error")
  }

  # level
  testr::check_class(level, "numeric", fun_name = "check_acf")
  if (level < 0 || level > 1) {
    rlang::abort(
      message = sprintf("`level` must be between 0 and 1, not \"%s\".", level),
      class = "check_acf_level_error"
    )
  }

  # lag_grid
  if (!rlang::inherits_any(lag_grid, c("numeric", "integer"))) {
    rlang::abort(message = sprintf(
      "`lag_grid` must be numeric or integer, not of class \"%s\".",
      paste(class(lag_grid), collapse = " / ")
    ), class = "check_acf_lag_grid_error")
  }
  if (any(lag_grid <= 0)) {
    rlang::abort(
      message = sprintf("`lag_grid` must be positive, not \"%s\".", lag_grid),
      class = "check_acf_lag_grid_error"
    )
  }

  ### Function -----------------------------------------------------------------

  ljung <- sapply(lag_grid, function(lag) {
    test <- stats::Box.test(data, lag = lag, type = "Ljung-Box")
    test$p.value
  })
  box <- sapply(lag_grid, function(lag) {
    test <- stats::Box.test(data, lag = lag, type = "Box-Pierce")
    test$p.value
  })

  list(
    p.values = data.frame(
      `Ljung-Box` = ljung,
      `Box-Pierce` = box,
      row.names = paste("lag", lag_grid, sep = "_")
    ),
    pass = all(c(ljung, box) < level)
  )

}
