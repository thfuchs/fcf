#' Evaluation Measures for keras sequential model forecasts
#'
#' @param DT actual data (including train, validation and test set)
#' @param forecast forecast values
#' @param n_train length of training set
#' @param n_val length of validation set
#' @param n_test length of test set
#'
#' @return
#' @export
#'
#' @examples
evaluate_keras_sequential <- function(DT, forecast, n_train, n_val, n_test) {

  DT_test <- DT[(.N-n_test+1):.N]
  n_initial <- n_train + n_val

  # Point Forecast Measures
  acc_MAPE <- sapply(
    multiple_h,
    function(h) mape(actual = DT_test[h,value], forecast = forecast[h,value]))
  acc_sMAPE <- sapply(
    multiple_h,
    function(h) smape(actual = DT_test[h,value], forecast = forecast[h,value]))
  acc_MASE <- sapply(
    multiple_h, function(h) mase(
      data = DT[1:(n_initial+max(h)),value],
      forecast = forecast[h,value],
      m = frequency
    )
  )

  # # Prediction Interval Measures
  # acc_SMIS <- sapply(
  #   multiple_h, function(h) smis(
  #     data = DT[1:(n_initial+max(h)),value],
  #     lower = forecast[h,lo95],
  #     upper = forecast[h,hi95],
  #     h = max(h), m = frequency, level = 0.95)
  # )
  # acc_ACD <- sapply(multiple_h, function(h) acd(
  #   actual = DT_test[h,value],
  #   lower = forecast[h,lo95],
  #   upper = forecast[h,hi95],
  #   level = 0.95)
  # )

  # Accuracy result
  acc <- data.table::data.table(
    type = "simple", h = names(multiple_h),
    mape = acc_MAPE, smape = acc_sMAPE, mase = acc_MASE
    # smis = acc_SMIS, acd = acc_ACD
  )

  return(acc)
}
