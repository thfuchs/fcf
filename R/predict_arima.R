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
#'
#' @return list with accuracy and forecasts (point forecast and PI)
#' @export
predict_arima <- function(data, cv_setting, transform = NULL) {

  n_train <- cv_setting$periods_train
  n_val <- cv_setting$periods_val
  n_initial <- n_train + n_val
  n_test <- cv_setting$periods_test

  rolling_origin_resamples <- rsample::rolling_origin(
    data,
    initial    = n_initial,
    assess     = n_test,
    cumulative = FALSE,
    skip       = cv_setting$skip_span
  )

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
      acc_MAPE <- mape(actual = DT_test$value, forecast = fc_values$value)
      acc_sMAPE <- smape(actual = DT_test$value, forecast = fc_values$value)
      acc_MASE <- mase(data = DT$value, forecast = fc_values$value, m = 4)

      # Prediction Interval Measures
      acc_SMIS <- smis(
        data = DT$value, lower = fc_values$lo95, upper = fc_values$hi95,
        h = 8, m = 4, level = 0.95)
      acc_ACD <- acd(
        actual = DT_test$value, lower = fc_values$lo95, upper = fc_values$hi95,
        level = 0.95)

      # Result
      acc <- data.table(
        type = "ARIMA",
        MAPE = acc_MAPE, sMAPE = acc_sMAPE, MASE = acc_MASE,
        SMIS = acc_SMIS, ACD = acc_ACD
      )

      ### Output
      list(forecast = fc, accuracy = acc)
    }
  )

  return(resample)
}
