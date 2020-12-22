#' Forecast the next n steps by ARIMA for each split
#'
#' @param data data.table object
#' @param cv_setting cross valudation settings. List Requiring \itemize{
#'   \item periods_train
#'   \item periods_val
#'   \item periods_test
#'   \item skip_span
#' }
#' @param col_id Optional ID column in `data`, default to "ticker"
#' @param col_date Date column in `data`, default to "index"
#' @param col_value Value column in `data`, default to "value"
#' @param transform One of NULL (default) and "normalize"
#' @param frequency time series frequency, e.g. 4 for quarters and 12 for months
#' @param multiple_h NULL if forecast horizon equals cv_setting$n_test, else
#'   named list of forecast horizons for accuracy measures
#'
#' @import data.table
#'
#' @return list with accuracy and forecasts (point forecast and PI)
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
#' fc_01 <- predict_arima(
#'   data = tsRNN::DT_unh,
#'   cv_setting = cv_setting
#' )
#' fc_01
#'
#' # Multiple forecast horizons
#' fc_02 <- predict_arima(
#'   data = tsRNN::DT_unh,
#'   cv_setting = cv_setting,
#'   multiple_h = list(short = 1:2, long = 3:4)
#' )
#' fc_02
#' }
predict_arima <- function(
  data,
  cv_setting,
  col_id = NULL,
  col_date = "index",
  col_value = "value",
  transform = NULL,
  frequency = 4,
  multiple_h = NULL
) {

  ### Checks -------------------------------------------------------------------
  testr::check_class(data, "data.frame", "predict_arima")
  testr::check_class(cv_setting, "list", "predict_arima")
  testr::check_class(col_id, "character", "predict_arima", allowNULL = TRUE)
  testr::check_class(col_date, "character", "predict_arima")
  testr::check_class(col_value, "character", "predict_arima")
  testr::check_class(transform, "character", "predict_arima", allowNULL = TRUE)
  testr::check_class(frequency, "numeric", "predict_arima")
  testr::check_class(multiple_h, "list", "predict_arima", allowNULL = TRUE)

  # Check column's fit in "data"
  data.table::setDT(data)
  if (
    !is.null(col_id) && is.null(data[[col_id]]) ||
    !is.null(col_id) && !inherits(data[[col_id]], "numeric")
  ) rlang::abort(
    message = "Variable specified by `col_id` must be class \"character\".",
    class = "predict_arima_col_id_error"
  )
  if (
    is.null(data[[col_date]]) ||
    !rlang::inherits_any(data[[col_date]], c("Date", "POSIXct"))
  ) rlang::abort(
    message = "Variable specified by `col_date` must be class \"Date\" or \"POSIXct\".",
    class = "predict_arima_col_date_error"
  )
  if (is.null(data[[col_value]]) || !inherits(data[[col_value]], "numeric"))
    rlang::abort(
      message = "Variable specified by `col_value` must be class \"numeric\".",
      class = "predict_arima_col_value_error"
    )

  ### Settings -----------------------------------------------------------------
  n_train <- cv_setting$periods_train
  n_val <- cv_setting$periods_val
  n_initial <- n_train + n_val
  n_test <- cv_setting$periods_test
  if (is.null(multiple_h)) multiple_h <- list(1:n_test)

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
      data <- if (!is.null(transform) && transform == "normalize")
        ts_normalization(DT, n_val, n_test, metrics = FALSE) else DT
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
        parallel = TRUE,  num.cores = NULL
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
        index = data[(.N-n_test+1):.N, get(col_date)],
        value = as.numeric(fc_arima$mean),
        lo95 = as.numeric(fc_arima$lower),
        hi95 = as.numeric(fc_arima$upper)
      )
      fc[, `:=` (key = "predict", type = "ARIMA")]
      fc <- rbind(data, fc, fill = TRUE)
      if (!is.null(col_id)) fc[, paste(col_id) := unique(DT[[col_id]])]

      ### Accuracy Measures
      fc_values <- fc[key == "predict"]
      index <- value <- lo95 <- hi95 <- NULL

      # Point Forecast Measures
      acc_MAPE <- sapply(
        multiple_h,
        function(h) mape(
          actual = DT_test[h, get(col_value)], forecast = fc_values[h,value]))
      acc_sMAPE <- sapply(
        multiple_h,
        function(h) smape(
          actual = DT_test[h, get(col_value)], forecast = fc_values[h,value]))
      acc_MASE <- sapply(
        multiple_h, function(h) mase(
          data = DT[1:(n_initial+max(h)), get(col_value)],
          forecast = fc_values[h,value], m = frequency)
      )

      # Prediction Interval Measures
      acc_SMIS <- sapply(
        multiple_h, function(h) smis(
          data = DT[1:(n_initial+max(h)), get(col_value)],
          lower = fc_values[h,lo95],
          upper = fc_values[h,hi95],
          h = max(h), m = frequency, level = 0.95)
      )
      acc_ACD <- sapply(multiple_h, function(h) acd(
        actual = DT_test[h, get(col_value)],
        lower = fc_values[h,lo95],
        upper = fc_values[h,hi95],
        level = 0.95)
      )

      # Accuracy result
      acc <- data.table::data.table(
        type = "ARIMA", h = names(multiple_h),
        mape = acc_MAPE, smape = acc_sMAPE, mase = acc_MASE,
        smis = acc_SMIS, acd = acc_ACD
      )

      ### Output
      list(forecast = fc, accuracy = acc)
    }
  )

  return(resample)
}
