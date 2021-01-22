#' Automatic cross-validated training and prediction process for recurrent
#' neural networks for time series data
#'
#' Use tuned RNN parameters with Keras functional API to train best performing
#' model(s) and generate forecasts
#'
#' @param data Univariate time series (data.frame) with date and value column,
#'   specified in `col_date` and `col_value`
#' @param model_type One of "simple", "gru" or "lstm"
#' @param cv_setting list of "periods_train", "periods_val", "periods_test" and
#'   "skip_span" for \link[rsample]{rolling_origin}
#' @param bayes_best_par tuned hyperparameters, from `tune_keras_rnn_bayesoptim()`
#' @param col_id Optional ID column in `data`, default to "ticker"
#' @param col_date Date column in `data`, default to "index"
#' @param col_value Value column in `data`, default to "value"
#' @param iter number of neural networks to train per split with same
#'   hyperparameters
#' @param iter_dropout number of iterations for prediction intervals calculated
#'   by monte carlo dropout
#' @param level level for prediction interval in percentage
#' @param save_model Automatically save tuned models? Specify NULL for No or
#' character vector with path to directory for yes
#' @param save_model_id optional id for model filename
#'
#' @import data.table
#' @import keras
#' @import reticulate
#' @importFrom zeallot %<-%
#'
#' @family RNN tuning with Keras
#' @return list of forecasts per split
#' @export
#'
#' @examples
#' \dontrun{
#' apple <- tsRNN::DT_apple
#'
#' bayes_best_par <- purrr::map(
#'   readRDS(system.file("tinytest_data/apple_bayesoptim.rds", package = "tsRNN")),
#'   "Best_Par"
#' )
#' cv_setting <- list(
#'   periods_train = 90,
#'   periods_val = 10,
#'   periods_test = 10,
#'   skip_span = 5
#' )
#'
#' result <- tune_keras_rnn_predict(
#'   data = apple,
#'   model_type = "simple",
#'   cv_setting = cv_setting,
#'   bayes_best_par = bayes_best_par
#' )
#' result
#' }
tune_keras_rnn_predict <- function(
                                   data,
                                   model_type,
                                   cv_setting,
                                   bayes_best_par,
                                   col_id = NULL,
                                   col_date = "index",
                                   col_value = "value",
                                   level = 95,
                                   iter = 10,
                                   iter_dropout = 1000,
                                   save_model = NULL,
                                   save_model_id = NULL) {

  # Checks ---------------------------------------------------------------------

  testr::check_class(data, "data.frame")
  testr::check_class(model_type, "character", n = 1)
  model_type <- rlang::arg_match(model_type, c("simple", "gru", "lstm"))
  testr::check_class(cv_setting, "list")
  testr::check_class(bayes_best_par, "list")
  testr::check_class(col_id, "character", n = 1, allowNULL = TRUE)
  testr::check_class(col_date, "character", n = 1)
  testr::check_class(col_value, "character", n = 1)
  testr::check_num_int(level, n = 1)
  testr::check_num_int(iter, n = 1)
  testr::check_num_int(iter_dropout, n = 1)
  testr::check_class(save_model, "character", n = 1, allowNULL = TRUE)
  testr::check_class(save_model_id, "character", n = 1, allowNULL = TRUE)

  data.table::setDT(data)

  check_data_structure(data, col_id, col_date, col_value)
  check_cv_setting(cv_setting)

  # check "bayes_best_par"
  if (names(bayes_best_par)[1] != "Slice1") {
    rlang::abort(
      message = "`bayes_best_par` must be a list named by each `rsample` split.",
      class = "tune_keras_rnn_predict_bayes_best_par_error"
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
        class = "tune_keras_rnn_predict_bayes_best_par_error"
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
      class = "tune_keras_rnn_predict_bayes_best_par_error"
    )
  }

  # `level` in range (0, 100)
  if (level <= 0 || level >= 100) rlang::abort(
    message = "`level` must be within interval (0, 100).",
    class = "tune_keras_rnn_predict_level_error"
  )

  # `iter` and `iter_dropout` positive
  if (iter < 1) rlang::abort(
    message = "`iter` must be a positive integer.",
    class = "tune_keras_rnn_predict_iter_error"
  )
  if (iter_dropout < 1) rlang::abort(
    message = "`iter_dropout` must be a positive integer.",
    class = "tune_keras_rnn_predict_iter_dropout_error"
  )

  # Check whether directory exists
  if (!is.null(save_model) && !dir.exists(save_model)) {
    rlang::abort(
      message = "Directory specified in `save_model` does not exist.",
      class = "tune_keras_rnn_predict_save_model_error"
    )
  }

  # Function -------------------------------------------------------------------

  patterns <- function(...) NULL # to address data.table R CMD check Note

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
    purrr::possibly(function(split) {
      bayes <- as.list(bayes_best_par[[split$id$id]])

      # Add lagged values to data
      best_lag_setting <- sort(bayes$lag_1:bayes$lag_2)
      lag_upper <- max(best_lag_setting)

      DT <- setDT(rbind(rsample::analysis(split), rsample::assessment(split)))
      add_shift(DT, cols = col_value, nlags = best_lag_setting, type = "lag")
      DT <- DT[!is.na(get(paste0("value_lag", lag_upper)))]
      DT[, key := "actual"]

      # Normalization
      data_split <- metrics_norm <- NULL
      c(data_split, metrics_norm) %<-%
        ts_normalization(DT, n_val, n_test, metrics = TRUE)

      # Use optimized parameters to train model on entire data set (excluding
      # test set)
      X <- Y <- NULL

      c(X, Y) %<-% ts_nn_preparation(
        data_split,
        tsteps = length(best_lag_setting),
        length_val = as.integer(n_val),
        length_test = as.integer(n_test)
      )
      best_optimizer <- switch(
        bayes$optimizer_type,
        `1` = optimizer_rmsprop(lr = bayes$learning_rate),
        `2` = optimizer_adam(lr = bayes$learning_rate),
        `3` = optimizer_adagrad(lr = 0.01)
      )

      ### Train model with tuned hyperparameters 10 times
      best_models <- lapply(1:iter, function(i) {
        model <- keras_rnn(
          X, Y,
          model_type = model_type,
          tsteps = length(best_lag_setting),
          n_epochs = bayes$n_epochs,
          n_units = bayes$n_units,
          loss = "mse",
          dropout_in_test = TRUE,
          optimizer = best_optimizer,
          dropout = bayes$dropout,
          recurrent_dropout = bayes$recurrent_dropout
        )

        # Save model if valid directoy specified
        if (!is.null(save_model)) {
          keras::save_model_hdf5(
            model,
            filepath = file.path(save_model, paste0(
              format(Sys.time(), "%Y%m%d_%H%M%S_"),
              if (!is.null(save_model_id)) paste0(save_model_id, "_"),
              model_type, "_",
              if (!is.null(col_id)) paste0(unique(data_split[[col_id]]), "_"),
              split$id$id, "_", sprintf("%02d", i), ".hdf5"
            ))
          )
        }

        model
      })

      # Change recurrent dropout and dropout to 0 for test phase and predict
      # using the 10 trained models
      n_train_internal <- n_train - lag_upper

      fc_mc <- lapply(best_models, function(model) {
        model_mean <- py_dropout_model(model, 0)
        prediction_test <- model_mean(X$test)
        prediction_train <- model_mean(X$train)

        list(
          predict = as.matrix(prediction_test),
          resid = as.matrix(prediction_train) - Y$train
        )
      })

      fc_predict <- matrix(
        unlist(purrr::map(fc_mc, "predict")),
        nrow = n_test, byrow = FALSE
      )
      fc_resid <- matrix(
        unlist(purrr::map(fc_mc, "resid")),
        nrow = n_train_internal, byrow = FALSE
      )

      median_fc_predict <-
        apply(fc_predict, 1, stats::median) * metrics_norm$scale + metrics_norm$center

      ### Monte Carlo Dropout - Source:
      # https://medium.com/hal24k-techblog/how-to-generate-neural-network-confidence-intervals-with-keras-e4c0b78ebbdf

      # 1. Apply dropout vector from 0.1 to 0.6 (steps 0.05) to find dropout
      #    distribution best matching residual distribution
      dropout_dist <- seq(0.1, 0.6, 0.05)

      dropout_dist_result <- vapply(dropout_dist, function(dropout_rate) {
        fc_dropout_dist <- lapply(best_models, function(model) {
          model_dropout <- py_dropout_model(model, dropout_rate)
          predict <- vapply(1:100, function(i) {
            as.numeric(model_dropout(X$train)[, 1])
          }, FUN.VALUE = numeric(n_train_internal))
          predict_median <- apply(predict, 1, stats::median)

          return(predict - predict_median)
        })

        fc_dropout_resid <- matrix(unlist(fc_dropout_dist), nrow = n_train_internal, byrow = FALSE)
        ks_result <- suppressWarnings(stats::ks.test(fc_resid, fc_dropout_resid))
        return(ks_result$statistic)
      }, FUN.VALUE = numeric(1))

      test_dropout <- dropout_dist[which.min(dropout_dist_result)]

      # 2. Apply best fitting dropout rate (`test_dropout`) to test set
      #    (`iter_dropout` iterations). Source:
      fc_dropout_mc <- lapply(best_models, function(model) {
        model_dropout <- py_dropout_model(model, test_dropout)
        vapply(1:iter_dropout, function(i) {
          as.numeric(model_dropout(X$test)[, 1])
        }, FUN.VALUE = numeric(n_test))
      })

      fc_keras_predict <- matrix(unlist(fc_dropout_mc), nrow = n_test, byrow = FALSE)

      # Use dropout rate to predict lower and upper prediction bound
      fc_lower <- apply(
        fc_keras_predict, 1, stats::quantile, 0.5 - level / 200,
        type = 8
      ) *
        metrics_norm$scale + metrics_norm$center
      fc_upper <- apply(
        fc_keras_predict, 1, stats::quantile, 0.5 + level / 200,
        type = 8
      ) *
        metrics_norm$scale + metrics_norm$center

      # Forecast results
      fc <- data.table::data.table(
        index = data_split[(.N - n_test + 1):.N, get(col_date)],
        value = as.numeric(median_fc_predict),
        lo95 = as.numeric(fc_lower),
        hi95 = as.numeric(fc_upper)
      )
      fc[, `:=`(key = "predict", type = model_type)]
      fc <- rbind(
        DT[, .SD, .SDcols = -patterns("_lag[0-9]")],
        fc,
        fill = TRUE
      )
      if (!is.null(col_id)) fc[, paste(col_id) := unique(DT[[col_id]])]

      # Output
      return(fc)
    }, otherwise = NULL, quiet = FALSE)
  )

  resample <- purrr::set_names(resample, rolling_origin_resamples$id)
  return(resample)
}
