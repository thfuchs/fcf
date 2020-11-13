#' Forecast the next n steps by ARIMA for each split
#'
#' @param data data.table object
#' @param cv_setting cross valudation settings. List Requiring \itemize{
#'   \item periods_train
#'   \item periods_val
#'   \item periods_test
#'   \item skip_span
#' }
#' @param transform One of NULL (default) and "normalize"
#' @param frequency time series frequency, e.g. 4 for quarters and 12 for months
#' @param multiple_h NULL if forecast horizon equals cv_setting$n_test, else
#'   named list of forecast horizons for accuracy measures
#'
#' @return list with accuracy and forecasts (point forecast and PI)
#' @export
predict_arima <- function(
  data, cv_setting, transform = NULL, frequency = 4, multiple_h = NULL
) {

  ### Checks -------------------------------------------------------------------
  testr::check_class(data, "data.frame", "predict_baselines")
  data.table::setDT(data)
  if (any(!c("index", "value") %in% names(data))) rlang::abort(
    message = "`data` requires variables `index` and `value`",
    class = "predict_baselines_data_error"
  )
  testr::check_class(cv_setting, "list", "predict_baselines")
  testr::check_class(transform, "character", "predict_baselines", allowNULL = TRUE)
  testr::check_class(frequency, "numeric", "predict_baselines")
  testr::check_class(multiple_h, "list", "predict_baselines", allowNULL = TRUE)

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
      min_date <- data[, c(year(min(index)), quarter(min(index)))]
      max_date <- data[, c(year(max(index)), quarter(max(index)))]

      ts_data <- stats::ts(data$value, frequency = 4, start = min_date, end = max_date)
      ts_train <- subset(ts_data, end = n_initial)

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
        index = data[(.N-n_test+1):.N, index],
        value = as.numeric(fc_arima$mean),
        lo95 = as.numeric(fc_arima$lower),
        hi95 = as.numeric(fc_arima$upper)
      )
      fc[, `:=` (key = "predict", type = "ARIMA")]
      fc <- rbind(data, fc, fill = TRUE)
      if (!is.null(fc[["ticker"]])) fc[, ticker := unique(DT$ticker)]

      ### Accuracy Measures
      fc_values <- fc[key == "predict"]

      # Point Forecast Measures
      acc_MAPE <- sapply(
        multiple_h,
        function(h) mape(actual = DT_test[h,value], forecast = fc_values[h,value]))
      acc_sMAPE <- sapply(
        multiple_h,
        function(h) smape(actual = DT_test[h,value], forecast = fc_values[h,value]))
      acc_MASE <- sapply(
        multiple_h, function(h) mase(
          data = DT[1:(n_initial+max(h)),value],
          forecast = fc_values[h,value], m = frequency)
      )

      # Prediction Interval Measures
      acc_SMIS <- sapply(
        multiple_h, function(h) smis(
          data = DT[1:(n_initial+max(h)),value],
          lower = fc_values[h,lo95],
          upper = fc_values[h,hi95],
          h = max(h), m = frequency, level = 0.95)
      )
      acc_ACD <- sapply(multiple_h, function(h) acd(
        actual = DT_test[h,value],
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
