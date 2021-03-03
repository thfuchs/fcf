#' Cross validated prediction and evaluation with baseline forecasts
#'
#' Get results for the following simple forecasting methods:
#'   - Naive forecast (\link[forecast]{naive})
#'   - Seasonal naive forecast
#'   - Mean forecast (\link[forecast]{meanf})
#'   - Simple exponential smoothing forecasts (\link[forecast]{ses})
#'   - Exponential smoothing with Holt's trend
#'
#' @param data Univariate time series (data.frame)
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
#' cv_setting <- list(
#'   periods_train = 90,
#'   periods_val = 10,
#'   periods_test = 10,
#'   skip_span = 5
#' )
#'
#' fc_01 <- cv_baselines(
#'   data = tsRNN::DT_apple,
#'   cv_setting = cv_setting
#' )
#' fc_01
#'
#' # Multiple forecast horizons
#' \dontrun{
#' fc_02 <- cv_baselines(
#'   data = tsRNN::DT_apple,
#'   cv_setting = cv_setting,
#'   h = list(short = 1:2, long = 3:6)
#' )
#' fc_02
#' }
cv_baselines <- function(
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

  # Check column's fit in "data"
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
    class = "cv_baselines_h_error"
  )
  if (is.numeric(h)) h <- list(h)
  if (inherits(h, "list")) for (single_h in h) {
    if (!rlang::inherits_any(single_h, c("numeric", "integer"))) rlang::abort(
      message = sprintf(
        "Elements of `h` must be numeric or integer, not of class \"%s\".",
        paste(class(single_h), collapse = " / ")
      ),
      class = "cv_baselines_h_error"
    )
    # warning if `h > cv_setting$periods_test`
    if (any(single_h > cv_setting[["periods_test"]])) rlang::warn(
      message = "At least one element of `h` is larger than cv_setting[[\"periods_test\"]].",
      class = "cv_baselines_h_warning"
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

      ts_data <- stats::ts(data[[col_value]], frequency = frequency, start = min_date, end = max_date)
      ts_train <- stats::window(ts_data, end = train_date)

      old_names_acc <- c("Training set", "Test set")
      new_names_acc <- c("train", "test")

      # Naive forecast
      fc_naive <- forecast::naive(
        ts_train,
        h = n_test,
        level = 95,
        bootstrap = TRUE,
        npaths = 2000,
        lambda = if (!is.null(transform) && transform == "box") "auto"
      )

      fc_naive <- data.table::data.table(
        index = data[(.N - n_test + 1):.N, get(col_date)],
        value = as.numeric(fc_naive$mean),
        lo95 = as.numeric(fc_naive$lower),
        hi95 = as.numeric(fc_naive$upper)
      )
      fc_naive[, `:=`(key = "predict", type = "Naive")]

      # Seasonal naive forecast
      fc_snaive <- forecast::snaive(
        ts_train,
        h = n_test,
        level = 95,
        bootstrap = TRUE,
        npaths = 2000,
        lambda = if (!is.null(transform) && transform == "box") "auto"
      )

      fc_snaive <- data.table::data.table(
        index = data[(.N - n_test + 1):.N, get(col_date)],
        value = as.numeric(fc_snaive$mean),
        lo95 = as.numeric(fc_snaive$lower),
        hi95 = as.numeric(fc_snaive$upper)
      )
      fc_snaive[, `:=`(key = "predict", type = "Snaive")]

      # Random walk with drift
      fc_drift <- forecast::rwf(
        ts_train,
        h = n_test,
        drift = TRUE,
        bootstrap = TRUE,
        npaths = 2000,
        level = 95,
        lambda = if (!is.null(transform) && transform == "box") "auto"
      )

      fc_drift <- data.table::data.table(
        index = data[(.N - n_test + 1):.N, get(col_date)],
        value = as.numeric(fc_drift$mean),
        lo95 = as.numeric(fc_drift$lower),
        hi95 = as.numeric(fc_drift$upper)
      )
      fc_drift[, `:=`(key = "predict", type = "Drift")]

      # Exponential smoothing with trend: Holt's trend
      fc_holt <- forecast::holt(
        ts_train,
        h = n_test,
        level = 95,
        bootstrap = TRUE,
        npaths = 2000,
        lambda = if (!is.null(transform) && transform == "box") "auto"
      )

      fc_holt <- data.table::data.table(
        index = data[(.N - n_test + 1):.N, get(col_date)],
        value = as.numeric(fc_holt$mean),
        lo95 = as.numeric(fc_holt$lower),
        hi95 = as.numeric(fc_holt$upper)
      )
      fc_holt[, `:=`(key = "predict", type = "Holt")]

      fc <- rbind(data, fc_naive, fc_snaive, fc_drift, fc_holt, fill = TRUE)
      if (!is.null(col_id)) fc[, paste(col_id) := unique(DT[[col_id]])]

      if (!is.null(transform) && transform == "normalize") {
        norm_cols <- c("value", "lo95", "hi95")
        fc[, (norm_cols) := lapply(
          .SD, function(x) x * metrics_norm$scale + metrics_norm$center
        ), .SDcols = norm_cols]
      }

      ### Accuracy Measures
      index <- value <- lo95 <- hi95 <- NULL

      acc <- data.table::setDT(purrr::map_df(
        list(fc_naive, fc_snaive, fc_drift, fc_holt),
        function(x) {

          # Point Forecast Measures
          acc_MAPE <- sapply(h, function(y)
            mape(actual = DT_test[y, get(col_value)], forecast = x[y, get(col_value)])
          )
          acc_sMAPE <- sapply(h, function(y)
            smape(actual = DT_test[y, get(col_value)], forecast = x[y, get(col_value)])
          )
          acc_MASE <- sapply(h, function(y) {
              mase(
                data = DT[1:n_initial, get(col_value)],
                actual = DT_test[y, get(col_value)],
                forecast = x[y, get(col_value)],
                m = frequency
              )
            }
          )

          # Prediction Interval Measures
          acc_SMIS <- sapply(h, function(y) {
              smis(
                data = DT[1:n_initial, get(col_value)],
                actual = DT_test[y, get(col_value)],
                forecast = x[y, value],
                lower = x[y, lo95],
                upper = x[y, hi95],
                m = frequency,
                level = 0.95
              )
            }
          )

          acc_ACD <- sapply(h, function(y)
            acd(
              actual = DT_test[y, get(col_value)], lower = x[y, lo95], upper = x[y, hi95],
              level = 0.95
            )
          )

          # Accuracy result
          data.table::data.table(
            type = unique(x$type), h = names(h),
            mape = acc_MAPE, smape = acc_sMAPE, mase = acc_MASE,
            smis = acc_SMIS, acd = acc_ACD
          )
        }
      ))

      ### Output
      list(forecast = fc, accuracy = acc)
    }
  )

  return(resample)
}
