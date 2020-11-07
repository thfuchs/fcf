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
      DT_train <- rsample::analysis(split)[1:n_train]
      # DT_val <- rsample::analysis(split)[(n_train+1):.N]
      DT_test <- rsample::assessment(split)

      DT <- rbind(DT_train, DT_test)

      # Normalization
      data <- if (normalize)
        ts_normalization(DT, n_val, n_test, metrics = FALSE) else DT

      # Reshaping to ts object
      min_date <- data[, c(year(min(index)), quarter(min(index)))]
      max_date <- data[, c(year(max(index)), quarter(max(index)))]

      ts_data <- stats::ts(data$value, frequency = 4, start = min_date, end = max_date)
      ts_train <- subset(ts_data, end = n_train+n_val)

      old_names <- c("Point Forecast", "Lo 95", "Hi 95")
      new_names <- c("fc", "lo95", "hi95")
      old_names_acc <- c("Training set", "Test set")
      new_names_acc <- c("train", "test")

      # Naive forecast
      fc_naive <- forecast::naive(ts_train, h = n_test, level = 95)
      acc_naive <- forecast::accuracy(fc_naive, ts_data)

      fc_naive <- data.table::as.data.table(fc_naive, keep.rownames = "index")
      data.table::setnames(fc_naive, old_names, paste("naive", new_names, sep = "_"))

      acc_naive <- data.table::as.data.table(acc_naive)[2][, index := "Naive"]

      # Seasonal naive forecast
      fc_snaive <- forecast::snaive(ts_train, h = n_test, level = 95)
      acc_snaive <- forecast::accuracy(fc_snaive, ts_data)

      fc_snaive <- data.table::as.data.table(fc_snaive, keep.rownames = "index")
      data.table::setnames(fc_snaive, old_names, paste("snaive", new_names, sep = "_"))

      acc_snaive <- data.table::as.data.table(acc_snaive)[2][, index := "Snaive"]

      # Mean Forecast
      fc_mean <- forecast::meanf(ts_train, h = n_test, level = 95)
      acc_mean <- forecast::accuracy(fc_mean, ts_data)

      fc_mean <- data.table::as.data.table(fc_mean, keep.rownames = "index")
      data.table::setnames(fc_mean, old_names, paste("mean", new_names, sep = "_"))

      acc_mean <- data.table::as.data.table(acc_mean)[2][, index := "Mean"]

      # Simple exponential smoothing
      fc_ses <- forecast::ses(ts_train, h = n_test, level = 95)
      acc_ses <- forecast::accuracy(fc_ses, ts_data)

      fc_ses <- data.table::as.data.table(fc_ses, keep.rownames = "index")
      data.table::setnames(fc_ses, old_names, paste("ses", new_names, sep = "_"))

      acc_ses <- data.table::as.data.table(acc_ses)[2][, index := "SES"]

      # Exponential smoothing with trend: Holt's trend
      fc_holt <- forecast::holt(ts_train, h = n_test, level = 95)
      acc_holt <- forecast::accuracy(fc_holt, ts_data)

      fc_holt <- data.table::as.data.table(fc_holt, keep.rownames = "index")
      data.table::setnames(fc_holt, old_names, paste("holt", new_names, sep = "_"))

      acc_holt <- data.table::as.data.table(acc_holt)[2][, index := "Holt"]

      ### Output
      fc <- fc_naive[fc_snaive, on = "index"][fc_mean, on = "index"][
        fc_ses, on = "index"][fc_holt, on = "index"]

      acc <- rbind(acc_naive, acc_snaive, acc_mean, acc_ses, acc_holt)

      list(forecast = fc, accuracy = acc)
    }
  )

  return(resample)
}
