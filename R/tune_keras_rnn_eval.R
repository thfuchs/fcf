#' Evaluate (tuned) recurrent neural networks per split/sample
#'
#' @param fc_sample forecasts per `rsample` split
#' @param cv_setting list of "periods_train", "periods_val", "periods_test" and
#'   "skip_span" for \link[rsample]{rolling_origin}
#' @param bayes_best_par tuned hyperparameters, from `tune_keras_rnn_bayesoptim()`
#' @param col_id Optional ID column in `data`, default to "ticker"
#' @param col_date Date column in `data`, default to "index"
#' @param col_value Value column in `data`, default to "value"
#' @param multiple_h NULL if forecast horizon equals cv_setting$n_test, else
#'   named list of forecast horizons for accuracy measures
#' @param frequency time series frequency, e.g. 4 for quarters and 12 for months
#' @param level level for prediction interval in percentage
#'
#' @return list of time series accuracy measures (MAPE, sMAPE, MASE, sMIS, ACD)
#'   optionally for different forecast horizons (if `multiple_h` specified)
#' @export
tune_keras_rnn_eval <- function(
                                fc_sample,
                                cv_setting,
                                bayes_best_par,
                                col_id = NULL,
                                col_date = "index",
                                col_value = "value",
                                multiple_h = NULL,
                                frequency = 4,
                                level = 95) {

  # Checks ---------------------------------------------------------------------
  testr::check_class(fc_sample, "list", "tune_keras_rnn_eval")
  testr::check_class(fc_sample[[1]], "data.frame", "tune_keras_rnn_eval")
  testr::check_class(cv_setting, "list", "tune_keras_rnn_eval")
  testr::check_class(bayes_best_par, "list", "tune_keras_rnn_eval")
  testr::check_class(col_id, "character", "tune_keras_rnn_eval", allowNULL = TRUE)
  testr::check_class(col_date, "character", "tune_keras_rnn_eval")
  testr::check_class(col_value, "character", "tune_keras_rnn_eval")
  testr::check_class(multiple_h, "list", "tune_keras_rnn_eval", allowNULL = TRUE)
  testr::check_class(frequency, "numeric", "tune_keras_rnn_eval")
  testr::check_class(level, "numeric", "tune_keras_rnn_eval")

  # `fc_sample` must be split-named list
  if (names(fc_sample)[1] != "Slice1") {
    rlang::abort(
      message = "`fc_sample` must be a list named by each `rsample` split",
      class = "tune_keras_rnn_eval_fc_sample_error"
    )
  }
  # "cv_setting" contains "periods_train", "periods_val", "periods_test" and
  # "skip_span"
  if (all(names(cv_setting)[order(names(cv_setting))] !=
    c("periods_test", "periods_train", "periods_val", "skip_span"))) {
    rlang::abort(
      message = "`data` must be a data.frame with 2 columns only: \"index\" and \"value\"",
      class = "tune_keras_rnn_eval_data_error"
    )
  }
  # `bayes_best_par` must be split-named list
  if (names(bayes_best_par)[1] != "Slice1") {
    rlang::abort(
      message = "`bayes_best_par` must be a list named by each `rsample` split",
      class = "tune_keras_rnn_eval_bayes_best_par_error"
    )
  }

  # Function -------------------------------------------------------------------
  n_test <- cv_setting$periods_test

  resample <- purrr::map2(
    fc_sample, bayes_best_par,
    purrr::possibly(function(fc, best_par) {
      best_par <- as.list(best_par)
      best_lag_setting <- sort(best_par$lag_1:best_par$lag_2)
      lag_upper <- max(best_lag_setting)

      n_train <- cv_setting$periods_train - max(lag_upper)

      DT_predict <- fc[key == "predict"]
      DT_actual <- fc[key == "actual"]
      DT_test <- DT_actual[(.N - n_test + 1):.N, .SD, .SDcols = c(col_date, col_value)]

      value <- lo95 <- hi95 <- type <- NULL

      # Point Forecast Measures
      acc_MAPE <- sapply(
        multiple_h,
        function(h) mape(actual = DT_test[h, get(col_value)], forecast = DT_predict[h, value])
      )
      acc_sMAPE <- sapply(
        multiple_h,
        function(h) smape(actual = DT_test[h, get(col_value)], forecast = DT_predict[h, value])
      )
      acc_MASE <- sapply(
        multiple_h, function(h) {
          mase(
            data = DT_actual[1:(n_train + max(h)), get(col_value)],
            forecast = DT_predict[h, get(col_value)], m = frequency
          )
        }
      )

      # Prediction Interval Measures
      acc_SMIS <- sapply(
        multiple_h, function(h) {
          smis(
            data = DT_actual[1:(n_train + max(h)), get(col_value)],
            lower = DT_predict[h, lo95],
            upper = DT_predict[h, hi95],
            h = max(h), m = frequency, level = level / 100
          )
        }
      )
      acc_ACD <- sapply(multiple_h, function(h) {
        acd(
          actual = DT_test[h, get(col_value)],
          lower = DT_predict[h, lo95],
          upper = DT_predict[h, hi95],
          level = level / 100
        )
      })

      acc <- data.table::data.table(
        type = unique(DT_predict[, type]),
        h = names(multiple_h),
        mape = acc_MAPE, smape = acc_sMAPE, mase = acc_MASE,
        smis = acc_SMIS, acd = acc_ACD
      )

      return(acc)
    }, otherwise = NULL, quiet = FALSE)
  )

  return(resample)
}
