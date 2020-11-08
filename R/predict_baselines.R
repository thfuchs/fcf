#' Prediction and evaluation for baseline models including cross validation
#'
#' @param data data.table object
#' @param cv_setting cross valudation settings. List Requiring \itemize{
#'   \item periods_train
#'   \item periods_val
#'   \item periods_test
#'   \item skip_span
#' }
#' @param normalize normalize data?
#'
#' @import data.table
#'
#' @return list with accuracy and forecasts (point forecast and PI)
#' @export
predict_baselines <- function(data, cv_setting, normalize) {

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
      data <- if (normalize)
        ts_normalization(DT, n_val, n_test, metrics = FALSE) else DT
      data[, key := "actual"]

      # Reshaping to ts object
      min_date <- data[, c(year(min(index)), quarter(min(index)))]
      max_date <- data[, c(year(max(index)), quarter(max(index)))]

      ts_data <- stats::ts(data$value, frequency = 4, start = min_date, end = max_date)
      ts_train <- subset(ts_data, end = n_train+n_val)

      old_names_acc <- c("Training set", "Test set")
      new_names_acc <- c("train", "test")

      # Naive forecast
      fc_naive <- forecast::naive(ts_train, h = n_test, level = 95)

      acc_naive <- forecast::accuracy(fc_naive, ts_data)
      acc_naive <- data.table::as.data.table(acc_naive)[2][, type := "Naive"]

      fc_naive <- data.table::data.table(
        index = data[(.N-n_test+1):.N, index],
        value = as.numeric(fc_naive$mean),
        lo95 = as.numeric(fc_naive$lower),
        hi95 = as.numeric(fc_naive$upper)
      )
      fc_naive[, `:=` (key = "predict", type = "Naive")]

      # Seasonal naive forecast
      fc_snaive <- forecast::snaive(ts_train, h = n_test, level = 95)

      acc_snaive <- forecast::accuracy(fc_snaive, ts_data)
      acc_snaive <- data.table::as.data.table(acc_snaive)[2][, type := "Snaive"]

      fc_snaive <- data.table::data.table(
        index = data[(.N-n_test+1):.N, index],
        value = as.numeric(fc_snaive$mean),
        lo95 = as.numeric(fc_snaive$lower),
        hi95 = as.numeric(fc_snaive$upper)
      )
      fc_snaive[, `:=` (key = "predict", type = "Snaive")]

      # Mean Forecast
      fc_mean <- forecast::meanf(ts_train, h = n_test, level = 95)

      acc_mean <- forecast::accuracy(fc_mean, ts_data)
      acc_mean <- data.table::as.data.table(acc_mean)[2][, type := "Mean"]

      fc_mean <- data.table::data.table(
        index = data[(.N-n_test+1):.N, index],
        value = as.numeric(fc_mean$mean),
        lo95 = as.numeric(fc_mean$lower),
        hi95 = as.numeric(fc_mean$upper)
      )
      fc_mean[, `:=` (key = "predict", type = "Mean")]

      # Simple exponential smoothing
      fc_ses <- forecast::ses(ts_train, h = n_test, level = 95)

      acc_ses <- forecast::accuracy(fc_ses, ts_data)
      acc_ses <- data.table::as.data.table(acc_ses)[2][, type := "SES"]

      fc_ses <- data.table::data.table(
        index = data[(.N-n_test+1):.N, index],
        value = as.numeric(fc_ses$mean),
        lo95 = as.numeric(fc_ses$lower),
        hi95 = as.numeric(fc_ses$upper)
      )
      fc_ses[, `:=` (key = "predict", type = "SES")]

      # Exponential smoothing with trend: Holt's trend
      fc_holt <- forecast::holt(ts_train, h = n_test, level = 95)

      acc_holt <- forecast::accuracy(fc_holt, ts_data)
      acc_holt <- data.table::as.data.table(acc_holt)[2][, type := "Holt"]

      fc_holt <- data.table::data.table(
        index = data[(.N-n_test+1):.N, index],
        value = as.numeric(fc_holt$mean),
        lo95 = as.numeric(fc_holt$lower),
        hi95 = as.numeric(fc_holt$upper)
      )
      fc_holt[, `:=` (key = "predict", type = "Holt")]

      ### Output
      fc <- rbind(data, fc_naive, fc_snaive, fc_mean, fc_ses, fc_holt, fill = TRUE)
      acc <- rbind(acc_naive, acc_snaive, acc_mean, acc_ses, acc_holt)

      list(forecast = fc, accuracy = acc)
    }
  )

  return(resample)
}
