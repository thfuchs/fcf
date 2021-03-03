#' Evaluate (tuned) recurrent neural networks per cross-validation split/sample
#' for time series data
#'
#' @param fc_sample forecasts per `rsample` split
#' @param cv_setting list of "periods_train", "periods_val", "periods_test" and
#'   "skip_span" for \link[rsample]{rolling_origin}
#' @param bayes_best_par tuned hyperparameters, from `tune_keras_rnn_bayesoptim()`
#' @param col_id Optional ID column in `data`, default to "ticker"
#' @param col_date Date column in `data`, default to "index"
#' @param col_value Value column in `data`, default to "value"
#' @param h NULL if forecast horizon equals cv_setting$n_test, else named list
#'   of forecast horizons for accuracy measures
#' @param frequency time series frequency, e.g. 4 for quarters and 12 for months
#' @param level level for prediction interval in percentage
#'
#' @family RNN tuning with Keras
#' @return list of time series accuracy measures (MAPE, sMAPE, MASE, sMIS, ACD)
#'   optionally for different forecast horizons (if `h` specified)
#' @export
#'
#' @examples
#' result_predict <- readRDS(system.file(
#' "tinytest_data/test_tune_keras_rnn_predict_simple.rds", package = "tsRNN"
#' ))
#' cv_setting <- list(
#'   periods_train = 90,
#'   periods_val = 10,
#'   periods_test = 10,
#'   skip_span = 5
#' )
#' bayes_best_par <- purrr::map(
#'   readRDS(system.file("tinytest_data/apple_bayesoptim.rds", package = "tsRNN")),
#'   "Best_Par"
#' )
#'
#' # Result
#' tune_keras_rnn_eval(
#'   fc_sample = result_predict,
#'   cv_setting = cv_setting,
#'   bayes_best_par = bayes_best_par
#' )
#'
#' # Specify multiple forecast horizons
#' tune_keras_rnn_eval(
#'   fc_sample = result_predict,
#'   cv_setting = cv_setting,
#'   bayes_best_par = bayes_best_par,
#'   h = list(short = 1:2, long = 3:6)
#' )
#'
tune_keras_rnn_eval <- function(
                                fc_sample,
                                cv_setting,
                                bayes_best_par,
                                col_id = NULL,
                                col_date = "index",
                                col_value = "value",
                                h = NULL,
                                frequency = 4,
                                level = 95) {

  # Checks ---------------------------------------------------------------------
  testr::check_class(fc_sample, "list")
  for (DT in fc_sample) testr::check_class(DT, "data.frame")
  testr::check_class(cv_setting, "list")
  testr::check_class(bayes_best_par, "list")
  testr::check_class(col_id, "character", n = 1, allowNULL = TRUE)
  testr::check_class(col_date, "character", n = 1)
  testr::check_class(col_value, "character", n = 1)
  testr::check_num_int(frequency, n = 1)
  testr::check_num_int(level, n = 1)

  # `fc_sample` must be split-named list
  if (names(fc_sample)[1] != "Slice1") {
    rlang::abort(
      message = "`fc_sample` must be a list named by each `rsample` split.",
      class = "tune_keras_rnn_eval_fc_sample_error"
    )
  }

  for (DT in fc_sample) {
    data.table::setDT(DT)
    check_data_structure(DT, col_id, col_date, col_value)
  }
  check_cv_setting(cv_setting)

  # `h`: If NULL: periods_test, else numeric or list of numeric vectors
  if (is.null(h)) h <- list(1:cv_setting$periods_test)
  if (!rlang::inherits_any(h, c("list", "numeric", "integer"))) rlang::abort(
    message = sprintf(
      "`h` must be list, numeric or integer, not of class \"%s\".",
      paste(class(h), collapse = " / ")
    ),
    class = "tune_keras_rnn_eval_h_error"
  )
  if (is.numeric(h)) h <- list(h)
  if (inherits(h, "list")) for (single_h in h) {
    if (!rlang::inherits_any(single_h, c("numeric", "integer"))) rlang::abort(
      message = sprintf(
        "Elements of `h` must be numeric or integer, not of class \"%s\".",
        paste(class(single_h), collapse = " / ")
      ),
      class = "tune_keras_rnn_eval_h_error"
    )
    # warning if `h > cv_setting$periods_test`
    if (any(single_h > cv_setting[["periods_test"]])) rlang::warn(
      message = "At least one element of `h` is larger than cv_setting[[\"periods_test\"]].",
      class = "tune_keras_rnn_eval_h_warning"
    )
  }

  # check "bayes_best_par"
  if (names(bayes_best_par)[1] != "Slice1") {
    rlang::abort(
      message = "`bayes_best_par` must be a list named by each `rsample` split.",
      class = "tune_keras_rnn_eval_bayes_best_par_error"
    )
  }
  for(bayes_i in 1:length(bayes_best_par)) {
    bayes_name <- names(bayes_best_par)[bayes_i]
    bayes_slice <- bayes_best_par[[bayes_i]]

    # 1. check class numeric
    if (!inherits(bayes_slice, "numeric")) {
      rlang::abort(
        message = sprintf(
          "`bayes_best_par[[\"%s\"]]` must be numeric, not of class \"%s\".",
          bayes_name, paste(class(bayes_slice), collapse = " / ")
        ),
        class = "tune_keras_rnn_eval_bayes_best_par_error"
      )
    }
    # 2. check for completeness
    bayes_slice_names <- c(
      "lag_1", "lag_2", "n_units", "n_epochs", "optimizer_type", "dropout",
      "recurrent_dropout", "learning_rate"
    )
    bayes_slice_check <- bayes_slice_names %in% names(bayes_slice)
    if (!all(bayes_slice_check)) rlang::abort(
      message = sprintf(
        "`bayes_best_par[[\"%s\"]] must contain all required parameters.\nMisses \"%s\".",
        bayes_name,
        paste(bayes_slice_names[!bayes_slice_check], collapse = "\", \"")
      ),
      class = "tune_keras_rnn_eval_bayes_best_par_error"
    )
  }

  # `frequency` positive
  if (frequency <= 0) rlang::abort(
    message = "`frequency` must be positive.",
    class = "tune_keras_rnn_eval_frequency_error"
  )
  # `level` in range (0, 100)
  if (level <= 0 || level >= 100) rlang::abort(
    message = "`level` must be within interval (0, 100).",
    class = "tune_keras_rnn_eval_level_error"
  )

  # Function -------------------------------------------------------------------
  n_test <- cv_setting$periods_test

  resample <- purrr::map2(
    fc_sample, bayes_best_par,
    purrr::possibly(function(fc, best_par) {
      best_par <- as.list(best_par)
      best_lag_setting <- sort(best_par$lag_1:best_par$lag_2)
      lag_upper <- max(best_lag_setting)

      # n_train <- cv_setting$periods_train - max(lag_upper)
      n_initial <- cv_setting$periods_train + cv_setting$periods_val - max(lag_upper)

      DT_predict <- fc[key == "predict"]
      DT_actual <- fc[key == "actual"]
      DT_test <- DT_actual[(.N - n_test + 1):.N, .SD, .SDcols = c(col_date, col_value)]

      value <- lo95 <- hi95 <- type <- NULL

      # Point Forecast Measures
      acc_MAPE <- sapply(h, function(x) {
        mape(actual = DT_test[x, get(col_value)], forecast = DT_predict[x, value])
      })
      acc_sMAPE <- sapply(h, function(x) {
        smape(actual = DT_test[x, get(col_value)], forecast = DT_predict[x, value])
      })
      acc_MASE <- sapply(h, function(x) {
        mase(
          data = DT[1:n_initial, get(col_value)],
          actual = DT_test[x, get(col_value)],
          forecast = DT_predict[x, get(col_value)],
          m = frequency
        )
      })

      # Prediction Interval Measures
      acc_SMIS <- sapply(h, function(x) {
        smis(
          data = DT[1:n_initial, get(col_value)],
          actual = DT_test[x, get(col_value)],
          lower = DT_predict[x, lo95],
          upper = DT_predict[x, hi95],
          m = frequency,
          level = level / 100
        )
      })
      acc_ACD <- sapply(h, function(x) {
        acd(
          actual = DT_test[x, get(col_value)],
          lower = DT_predict[x, lo95],
          upper = DT_predict[x, hi95],
          level = level / 100
        )
      })

      acc <- data.table::data.table(
        type = unique(DT_predict[, type]),
        h = names(h),
        mape = acc_MAPE, smape = acc_sMAPE, mase = acc_MASE,
        smis = acc_SMIS, acd = acc_ACD
      )

      return(acc)
    }, otherwise = NULL, quiet = FALSE)
  )

  return(resample)
}
