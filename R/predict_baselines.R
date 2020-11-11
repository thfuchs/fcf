#' Prediction and evaluation for baseline models including cross validation
#'
#' @param data data.table object
#' @param cv_setting cross valudation settings. List Requiring \itemize{
#'   \item periods_train
#'   \item periods_val
#'   \item periods_test
#'   \item skip_span
#' }
#' @param transform One of NULL, "normalize" and "box" for Box-Cox transformation
#'
#' @import data.table
#' @import forecast
#'
#' @return list with accuracy and forecasts (point forecast and PI)
#' @export
predict_baselines <- function(data, cv_setting, transform = NULL) {

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
        index = data[(.N-n_test+1):.N, index],
        value = as.numeric(fc_naive$mean),
        lo95 = as.numeric(fc_naive$lower),
        hi95 = as.numeric(fc_naive$upper)
      )
      fc_naive[, `:=` (key = "predict", type = "Naive")]

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
        index = data[(.N-n_test+1):.N, index],
        value = as.numeric(fc_snaive$mean),
        lo95 = as.numeric(fc_snaive$lower),
        hi95 = as.numeric(fc_snaive$upper)
      )
      fc_snaive[, `:=` (key = "predict", type = "Snaive")]

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
        index = data[(.N-n_test+1):.N, index],
        value = as.numeric(fc_drift$mean),
        lo95 = as.numeric(fc_drift$lower),
        hi95 = as.numeric(fc_drift$upper)
      )
      fc_drift[, `:=` (key = "predict", type = "Drift")]

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
        index = data[(.N-n_test+1):.N, index],
        value = as.numeric(fc_holt$mean),
        lo95 = as.numeric(fc_holt$lower),
        hi95 = as.numeric(fc_holt$upper)
      )
      fc_holt[, `:=` (key = "predict", type = "Holt")]

      ### Output
      fc <- rbind(data, fc_naive, fc_snaive, fc_drift, fc_holt, fill = TRUE)
      if (!is.null(fc[["ticker"]])) fc[, ticker := unique(DT$ticker)]

      acc <- purrr::map_df(
        list(fc_naive, fc_snaive, fc_drift, fc_holt),
        function(x) {
          # Point Forecast Measures
          acc_MAPE <- mape(actual = DT_test$value, forecast = x$value)
          acc_sMAPE <- smape(actual = DT_test$value, forecast = x$value)
          acc_MASE <- mase(data = DT$value, forecast = x$value, m = 4)

          # Prediction Interval Measures
          acc_SMIS <- smis(
            data = DT$value, lower = x$lo95, upper = x$hi95,
            h = 8, m = 4, level = 0.95)
          acc_ACD <- acd(
            actual = DT_test$value, lower = x$lo95, upper = x$hi95,
            level = 0.95)

          data.table(
            type = unique(x$type),
            MAPE = acc_MAPE, sMAPE = acc_sMAPE, MASE = acc_MASE,
            SMIS = acc_SMIS, ACD = acc_ACD
          )
        }
      )

      list(forecast = fc, accuracy = acc)
    }
  )

  return(resample)
}
