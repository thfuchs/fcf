#' Cross validated prediction and evaluation with ARIMA forecasts
#'
#' Time-series cross-validation wrapper for \link[forecast]{auto.arima}
#'
#' @param data data.table object
#' @param cv_setting cross validation settings. Named list requiring `periods_train`,
#'   `periods_val` `periods_test` and `skip_span`. See section "Cross validation
#'   settings" for details.
#' @param col_id Optional ID column in `data`, default to "ticker"
#' @param col_date Date column in `data`, default to "index"
#' @param col_value Value column in `data`, default to "value"
#' @param transform Transform data before estimation? One of NULL (default)
#'   and "normalize"
#' @param frequency time series frequency, e.g. 4 for quarters and 12 for months
#' @param h NULL if forecast horizon equals cv_setting$n_test, else named list
#'   of forecast horizons for accuracy measures
#'
#' @import data.table
#' @importFrom zeallot %<-%
#'
#' @section Cross validation settings:
#' Using \link[rsample]{rolling_origin} to split the time series. Requiring:
#' \itemize{
#'   \item `periods_train`: Length of training set per split
#'   \item `periods_val`: Length of validation set per split
#'   \item `periods_test`: Length of test/hold-out set per split
#'   \item `skip_span`: Gaps between overlapping splits to reduce computational
#'    intensity and recundancy between data splits.\cr \cr
#' }
#' Note: `periods_val` only relevant for deep learning models.
#' \link{cv_baselines} and \link{cv_arima} use sum of `periods_train` and
#' `periods_val` for training and only `periods_test` as hold-out test set (no
#' learning and feedback through validation by traditional statistical models)
#'
#' @return list of `type` (model), `h` (forecast horizon, if specified),
#'   \link{mape}, \link{smape}, \link{mase}, \link{smis} and \link{acd}
#' @export
#'
#' @examples
#' \dontrun{
#' cv_setting <- list(
#'   periods_train = 90,
#'   periods_val = 10,
#'   periods_test = 10,
#'   skip_span = 5
#' )
#'
#' fc_01 <- cv_arima(
#'   data = tsRNN::DT_apple,
#'   cv_setting = cv_setting
#' )
#' fc_01
#'
#' # Multiple forecast horizons
#' fc_02 <- cv_arima(
#'   data = tsRNN::DT_apple,
#'   cv_setting = cv_setting,
#'   h = list(short = 1:2, long = 3:6)
#' )
#' fc_02
#' }
cv_arima <- function(
                          data,
                          cv_setting,
                          col_id = NULL,
                          col_date = "index",
                          col_value = "value",
                          transform = NULL,
                          frequency = 4,
                          h = NULL) {

  ### Checks -------------------------------------------------------------------
  testr::check_class(data, "data.frame")
  testr::check_class(cv_setting, "list")
  testr::check_class(col_id, "character", allowNULL = TRUE)
  testr::check_class(col_date, "character")
  testr::check_class(col_value, "character")
  testr::check_class(transform, "character", allowNULL = TRUE)
  testr::check_num_int(frequency)

  data.table::setDT(data)

  check_data_structure(data, col_id, col_date, col_value)
  check_cv_setting(cv_setting)

  # `h`: If NULL: periods_test, else numeric or list of numeric vectors
  if (is.null(h)) h <- list(1:cv_setting$periods_test)
  if (!rlang::inherits_any(h, c("list", "numeric", "integer"))) rlang::abort(
    message = sprintf(
      "`h` must be list, numeric or integer, not of class \"%s\".",
      paste(class(h), collapse = " / ")
    ),
    class = "cv_arima_h_error"
  )
  if (is.numeric(h)) h <- list(h)
  if (inherits(h, "list")) for (single_h in h) {
    if (!rlang::inherits_any(single_h, c("numeric", "integer"))) rlang::abort(
      message = sprintf(
        "Elements of `h` must be numeric or integer, not of class \"%s\".",
        paste(class(single_h), collapse = " / ")
      ),
      class = "cv_arima_h_error"
    )
    # warning if `h > cv_setting$periods_test`
    if (any(single_h > cv_setting[["periods_test"]])) rlang::warn(
      message = "At least one element of `h` is larger than cv_setting[[\"periods_test\"]].",
      class = "cv_arima_h_warning"
    )
  }

  ### Settings -----------------------------------------------------------------
  n_initial <- cv_setting$periods_train + cv_setting$periods_val
  n_test <- cv_setting$periods_test

  rolling_origin_resamples <- rsample::rolling_origin(
    data,
    initial    = n_initial,
    assess     = n_test,
    cumulative = FALSE,
    skip       = cv_setting$skip_span
  )

  ### Function -----------------------------------------------------------------
  resample <- purrr::map(
    rolling_origin_resamples$splits,
    function(split) {
      # Train-Test Split
      DT_train <- rsample::analysis(split)
      DT_test <- rsample::assessment(split)

      DT <- rbind(DT_train, DT_test)

      # Normalization
      metrics_norm <- NULL
      if (!is.null(transform) && transform == "normalize") {
        c(data, metrics_norm) %<-% ts_normalization(DT, 0, n_test, metrics = TRUE)
      } else {
        data <- DT
      }
      data[, key := "actual"]

      # Reshaping to ts object
      min_date <- data[, c(year(min(get(col_date))), quarter(min(get(col_date))))]
      train_date <- data[n_initial, c(year(get(col_date)), quarter(get(col_date)))]
      max_date <- data[, c(year(max(get(col_date))), quarter(max(get(col_date))))]

      ts_data <- stats::ts(data[[col_value]], frequency = 4, start = min_date, end = max_date)
      ts_train <- stats::window(ts_data, end = train_date)

      old_names_acc <- c("Training set", "Test set")
      new_names_acc <- c("train", "test")

      ### ARIMA Model
      arima <- forecast::auto.arima(
        ts_train,
        max.p = 5, max.q = 5, max.P = 2, max.Q = 2,
        max.order = 5, max.d = 2, max.D = 1,
        start.p = 2, start.q = 2, start.P = 1, start.Q = 1,
        stationary = FALSE,
        seasonal = TRUE,
        ic = "aicc",
        stepwise = FALSE,
        approximation = FALSE,
        method = "ML",
        test = "kpss", test.args = list(alpha = 0.05, type = "level"),
        seasonal.test = "ch", seasonal.test.args = list(alpha = 0.05),
        allowdrift = TRUE,
        allowmean = TRUE,
        parallel = TRUE, num.cores = NULL
      )

      ### Forecast
      fc_arima <- forecast::forecast(
        arima,
        h = n_test,
        bootstrap = TRUE,
        npaths = 2000,
        level = 95
      )

      fc <- data.table::data.table(
        index = data[(.N - n_test + 1):.N, get(col_date)],
        value = as.numeric(fc_arima$mean),
        lo95 = as.numeric(fc_arima$lower),
        hi95 = as.numeric(fc_arima$upper)
      )
      fc[, `:=`(key = "predict", type = "ARIMA")]
      fc <- rbind(data, fc, fill = TRUE)
      if (!is.null(col_id)) fc[, paste(col_id) := unique(DT[[col_id]])]

      if (!is.null(transform) && transform == "normalize") {
        norm_cols <- c("value", "lo95", "hi95")
        fc[, (norm_cols) := lapply(
          .SD, function(x) x * metrics_norm$scale + metrics_norm$center
        ), .SDcols = norm_cols]
      }

      ### Accuracy Measures
      fc_values <- fc[key == "predict"]
      index <- value <- lo95 <- hi95 <- NULL

      # Point Forecast Measures
      acc_MAPE <- sapply(h, function(y)
        mape(actual = DT_test[y, get(col_value)], forecast = fc_values[y, value])
      )
      acc_sMAPE <- sapply(h, function(y)
        smape(actual = DT_test[y, get(col_value)], forecast = fc_values[y, value])
      )
      acc_MASE <- sapply(h, function(y)
        mase(
          data = DT[1:(n_initial + max(y)), get(col_value)],
          forecast = fc_values[y, value], m = frequency
        )
      )

      # Prediction Interval Measures
      acc_SMIS <- sapply(h, function(y)
        smis(
          data = DT[1:n_initial, get(col_value)],
          forecast = fc_values[y, value],
          lower = fc_values[y, lo95],
          upper = fc_values[y, hi95],
          h = max(y), m = frequency, level = 0.95
        )
      )
      acc_ACD <- sapply(h, function(y)
        acd(
          actual = DT_test[y, get(col_value)],
          lower = fc_values[y, lo95],
          upper = fc_values[y, hi95],
          level = 0.95
        )
      )

      # Accuracy result
      acc <- data.table::data.table(
        type = "ARIMA", h = names(h),
        mape = acc_MAPE, smape = acc_sMAPE, mase = acc_MASE,
        smis = acc_SMIS, acd = acc_ACD
      )

      ### Output
      list(forecast = fc, accuracy = acc)
    }
  )

  return(resample)
}
