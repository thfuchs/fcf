#' Tune recurrent neural network with Keras functional API and Bayes
#' Optimization and select best performing model
#'
#' @param data Univariate time series (data.frame) with columns index and value
#' @param cv_setting list of "periods_train", "periods_val", "periods_test" and
#'   "skip_span" for \link[rsample]{rolling_origin}
#' @param model_type One of "basic", "gru" or "lstm"
#' @param tuning_bounds list of tuning parameters - see section "Tuning Bounds"
#' @param frequency time series frequency, e.g. 4 for quarters and 12 for months
#' @param multiple_h NULL if forecast horizon equals cv_setting$n_test, else
#'   named list of forecast horizons for accuracy measures
#' @param iterations number of iterations for dropout-based prediction interval
#' @param level level for prediction interval in percentage
#' @param test_dropout specify dropout-rate during testing for prediction
#'   interval
#' @param save_model Automatically save tuned models? Specify NULL for No or
#' character vector with path to directory for yes
#'
#' @section Tuning Bounds:
#' The following parameters are (currently) available for tuning.
#' - lag_1 (integer(2)) lower bounds
#' - lag_2 (integer(2)) upper bounds
#' - n_units (integer(2)) lower and upper bound for rnn units (cells)
#' - n_epochs (integer(2)) lower and upper bound for epochs
#' - optimizer_type (integer) 1 = "rmsprop", 2 = "adam"
#' - dropout = (numeric(2)) lower and upper bound for dropout rate
#' - recurrent_dropout = (numeric(2)) lower and upper bound for recurrent dropout rate
#' - learning_rate = (numeric(2)) lower and upper bound for learning rate
#'
#' @importFrom magrittr %>%
#' @importFrom zeallot %<-%
#' @import data.table
#'
#' @return list of "results" and "min_params"
#' @export
tune_keras_rnn <- function(
  data,
  model_type,
  cv_setting,
  tuning_bounds,
  frequency = 4,
  iterations = 2000,
  multiple_h = NULL,
  level = 95,
  test_dropout = 0.1,
  save_model = NULL
) {

  # Checks ---------------------------------------------------------------------

  testr::check_class(data, "data.frame", "tune_keras_rnn")
  testr::check_class(model_type, "character", "tune_keras_rnn")
  testr::check_class(cv_setting, "list", "tune_keras_rnn")
  testr::check_class(tuning_bounds, "list", "tune_keras_rnn")
  testr::check_class(frequency, "numeric", "tune_keras_rnn")
  testr::check_class(iterations, "numeric", "tune_keras_rnn")
  testr::check_class(multiple_h, "list", "tune_keras_rnn", allowNULL = TRUE)
  testr::check_class(test_dropout, "numeric", "tune_keras_rnn")
  testr::check_class(save_model, "character", "tune_keras_rnn", allowNULL = TRUE)

  # "data" contains columns "index" and "value" only (univariate time series)
  if (all(names(data)[order(names(data))] != c("index", "value"))) rlang::abort(
    message = "`data` must be a data.frame with 2 columns only: \"index\" and \"value\"",
    class = "tune_keras_rnn_data_error"
  )
  # "model_type" must be one of "simple", "gru" or "lstm"
  model_type <- rlang::arg_match(model_type, c("simple", "gru", "lstm"))
  # "cv_setting" contains "periods_train", "periods_val", "periods_test" and
  # "skip_span"
  if (all(names(cv_setting)[order(names(cv_setting))] !=
          c("periods_test", "periods_train", "periods_val", "skip_span"))) {
    rlang::abort(
      message = "`data` must be a data.frame with 2 columns only: \"index\" and \"value\"",
      class = "tune_keras_rnn_data_error"
    )
  }
  # Check whether directory exists
  if (!dir.exists(save_model)) rlang::abort(
    message = "Directory specified in `save_model` does not exist",
    class = "tune_keras_rnn_save_model_error"
  )

  # Function -------------------------------------------------------------------

  n_train <- cv_setting$periods_train
  n_val <- cv_setting$periods_val
  n_initial <- n_train + n_val
  n_test <- cv_setting$periods_test
  lag_lower <- min(tuning_bounds$lag_1, tuning_bounds$lag_2)
  lag_upper <- max(tuning_bounds$lag_1, tuning_bounds$lag_2)

  # Add lagged values to data
  data_lag <- data.table::copy(data)
  add_shift(data_lag, cols = "value", nlags = c(lag_lower:lag_upper), type = "lag")
  data_lag <- data_lag[!is.na(get(paste0("value_lag", lag_upper)))]

  rolling_origin_resamples <- rsample::rolling_origin(
    data_lag,
    initial    = n_initial,
    assess     = n_test,
    cumulative = FALSE,
    skip       = cv_setting$skip_span
  )

  resample <- purrr::map(
    rolling_origin_resamples$splits,
    purrr::possibly(function(split) {

      # Train-Test Split
      DT_train <- rsample::analysis(split)[1:n_train]
      DT_val <- rsample::analysis(split)[(n_train+1):.N]
      DT_test <- rsample::assessment(split)

      DT <- rbind(DT_train, DT_val, DT_test)
      DT[, key := "actual"]

      # Normalization
      c(data_split, metrics_norm) %<-%
        ts_normalization(DT, n_val, n_test, metrics = TRUE)

      # Set environment of help function to current environment
      environment(internal_keras_fun) <- environment()
      # Bayes Optimization
      bayes <- rBayesianOptimization::BayesianOptimization(
        FUN = internal_keras_fun,
        bounds = tuning_bounds,
        init_points = 5,
        n_iter = 10,
        acq = "ucb",
        verbose = TRUE
      )

      # Use optimized parameters to train model on entire data set (excluding
      # test set)
      best_lag_setting <- sort(bayes$Best_Par["lag_1"]:bayes$Best_Par["lag_2"])
      c(X, Y) %<-% ts_nn_preparation(
        data_split,
        tsteps = length(best_lag_setting),
        length_val = 0L,
        length_test = as.integer(n_test)
      )
      best_optimizer_type <- switch(
        bayes$Best_Par["optimizer_type"],
        `1` = "rmsprop",
        `2` = "adam"
      )

      best_model <- keras_rnn(
        X, Y,
        model_type = model_type,
        tsteps = length(best_lag_setting),
        n_epochs = bayes$Best_Par["n_epochs"],
        n_units = bayes$Best_Par["n_units"],
        dropout_in_test = TRUE,
        optimizer_type = best_optimizer_type,
        dropout = bayes$Best_Par["dropout"],
        recurrent_dropout = bayes$Best_Par["recurrent_dropout"],
        learning_rate = bayes$Best_Par["learning_rate"]
      )

      # Save model if valid directoy specified
      if (!is.null(save_model)) {
        keras::save_model_hdf5(
          best_model,
          filepath = file.path(save_model, paste0(
            format(Sys.time(), "%Y%m%d_%H%M%S_"),
            model_type, "_", unique(data_split$ticker), split$id$id, ".hdf5")
          )
        )
      }

      # Change recurrent dropout to 0 for test phase and dropout to 0 for
      # mean predictions and to `test_dropout` for prediction intervals
      mean_model <- py_dropout_model(best_model, 0)
      dropout_model <- py_dropout_model(best_model, test_dropout)

      fc_mean <-
        predict(mean_model, X$test) * metrics_norm$scale + metrics_norm$center

      fc_iter <- vapply(1:iterations, function(i) {
        predict(dropout_model, X$test) * metrics_norm$scale + metrics_norm$center
      }, FUN.VALUE = numeric(n_test))

      fc_lower <- apply(fc_iter, 1, quantile, 0.5 - level / 200, type = 8)
      fc_upper <- apply(fc_iter, 1, quantile, 0.5 + level / 200, type = 8)

      fc <- data.table::data.table(
        index = data_split[(.N-n_test+1):.N, index],
        value = as.numeric(fc_mean),
        lo95 = as.numeric(fc_lower),
        hi95 = as.numeric(fc_upper)
      )
      fc[, `:=` (key = "predict", type = model_type)]
      fc <- rbind(
        DT[, .SD, .SDcols = -patterns("_lag[0-9]")],
        fc,
        fill = TRUE
      )
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
          h = max(h), m = frequency, level = level/100)
      )
      acc_ACD <- sapply(multiple_h, function(h) acd(
        actual = DT_test[h,value],
        lower = fc_values[h,lo95],
        upper = fc_values[h,hi95],
        level = level/100)
      )

      # Accuracy result
      acc <- data.table::data.table(
        type = model_type, h = names(multiple_h),
        mape = acc_MAPE, smape = acc_sMAPE, mase = acc_MASE,
        smis = acc_SMIS, acd = acc_ACD
      )

      ### Output
      list(
        forecast = fc,
        accuracy = acc,
        tuning_params = as.list(bayes$Best_Par)
      )
    }, otherwise = NULL, quiet = FALSE)
  )

  return(resample)
}


# Internal Function
internal_keras_fun <- function(
  n_units, n_epochs, lag_1, lag_2, dropout, recurrent_dropout,
  optimizer_type, learning_rate
) {

  lag_setting <- sort(lag_1:lag_2)
  optimizer <- switch(
    optimizer_type,
    `1` = optimizer_rmsprop(lr = learning_rate),
    `2` = optimizer_adam(lr = learning_rate)
  )

  # Reshaping
  c(X, Y) %<-% ts_nn_preparation(
    data_split,
    tsteps = length(lag_setting),
    length_val = as.integer(n_val),
    length_test = as.integer(n_test)
  )

  input <- layer_input(shape = c(length(lag_setting), 1))

  hidden_layer <- if (model_type == "simple") {
    layer_simple_rnn(
      units             = n_units,
      input_shape       = c(length(lag_setting), 1),
      dropout           = dropout,
      recurrent_dropout = recurrent_dropout
    )
  } else if (model_type == "gru") {
    layer_gru(
      units             = n_units,
      input_shape       = c(length(lag_setting), 1),
      dropout           = dropout,
      recurrent_dropout = recurrent_dropout
    )
  } else if (model_type == "lstm") {
    layer_lstm(
      units             = n_units,
      input_shape       = c(length(lag_setting), 1),
      dropout           = dropout,
      recurrent_dropout = recurrent_dropout
    )
  }

  output <- input %>% hidden_layer %>% layer_dense(units = 1)

  model <- keras_model(input, output)

  model %>% compile(optimizer = optimizer, loss = "mse")

  history <- model %>% fit(
    x               = X$train,
    y               = Y$train,
    steps_per_epoch = 1,
    epochs          = n_epochs,
    batch_size      = NULL,
    verbose         = 0,
    shuffle         = FALSE,
    validation_data = list(X$val, Y$val),
    view_metrics    = FALSE
  )

  return(list(Score = -history$metrics$val_loss[n_epochs], Pred = 0))
}
