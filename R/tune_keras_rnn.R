#' Tune recurrent neural network with Keras functional API and Bayes
#' Optimization and select best performing model
#'
#' @param data Univariate time series (data.frame) with date and value column,
#'   specified in `col_date` and `col_value`
#' @param cv_setting list of "periods_train", "periods_val", "periods_test" and
#'   "skip_span" for \link[rsample]{rolling_origin}
#' @param model_type One of "basic", "gru" or "lstm"
#' @param tuning_bounds list of tuning parameters - see section "Tuning Bounds"
#' @param col_id Optional ID column in `data`, default to "ticker"
#' @param col_date Date column in `data`, default to "index"
#' @param col_value Value column in `data`, default to "value"
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
#' @import data.table
#' @importFrom magrittr %>%
#' @importFrom zeallot %<-%
#'
#' @return list of "results" and "min_params"
#' @export
tune_keras_rnn <- function(
  data,
  model_type,
  cv_setting,
  tuning_bounds,
  col_id = "ticker",
  col_date = "index",
  col_value = "value",
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
  testr::check_class(col_id, "character", "tune_keras_rnn")
  testr::check_class(col_date, "character", "tune_keras_rnn")
  testr::check_class(col_value, "character", "tune_keras_rnn")
  testr::check_class(tuning_bounds, "list", "tune_keras_rnn")
  testr::check_class(frequency, "numeric", "tune_keras_rnn")
  testr::check_class(iterations, "numeric", "tune_keras_rnn")
  testr::check_class(multiple_h, "list", "tune_keras_rnn", allowNULL = TRUE)
  testr::check_class(test_dropout, "numeric", "tune_keras_rnn")
  testr::check_class(save_model, "character", "tune_keras_rnn", allowNULL = TRUE)

  # "data" contains columns "index" and "value" (and optionally "id")
  # (univariate time series)
  data.table::setDT(data)
  if (
    !is.null(col_id) && is.null(data[[col_id]]) ||
    !is.null(col_id) && !inherits(data[[col_id]], "numeric")
  ) rlang::abort(
    message = "Variable specified by `col_id` must be class \"character\".",
    class = "tune_keras_rnn_col_id_error"
  )
  if (
    is.null(data[[col_date]]) ||
    !rlang::inherits_any(data[[col_date]], c("Date", "POSIXct"))
  ) rlang::abort(
    message = "Variable specified by `col_date` must be class \"Date\" or \"POSIXct\".",
    class = "tune_keras_rnn_col_date_error"
  )
  if (is.null(data[[col_value]]) || !inherits(data[[col_value]], "numeric"))
    rlang::abort(
      message = "Variable specified by `col_value` must be class \"numeric\".",
      class = "tune_keras_rnn_col_value_error"
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

  patterns <- function(...) NULL # to address data.table R CMD check Note

  n_train <- cv_setting$periods_train
  n_val <- cv_setting$periods_val
  n_initial <- n_train + n_val
  n_test <- cv_setting$periods_test
  lag_lower <- min(tuning_bounds$lag_1, tuning_bounds$lag_2)
  lag_upper <- max(tuning_bounds$lag_1, tuning_bounds$lag_2)

  # Add lagged values to data
  data_lag <- data.table::copy(data)
  add_shift(data_lag, cols = col_value, nlags = c(lag_lower:lag_upper), type = "lag")
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
      data_split <- metrics_norm <- NULL
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
      X <- Y <- NULL

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
            model_type, "_", unique(data_split[[col_id]]), split$id$id, ".hdf5")
          )
        )
      }

      # Change recurrent dropout to 0 for test phase and dropout to 0 for
      # mean predictions and to `test_dropout` for prediction intervals
      mean_model <- py_dropout_model(best_model, 0)
      dropout_model <- py_dropout_model(best_model, test_dropout)

      fc_mean <-
        stats::predict(mean_model, X$test) * metrics_norm$scale + metrics_norm$center

      fc_iter <- vapply(1:iterations, function(i) {
        stats::predict(dropout_model, X$test) * metrics_norm$scale + metrics_norm$center
      }, FUN.VALUE = numeric(n_test))

      fc_lower <- apply(fc_iter, 1, stats::quantile, 0.5 - level / 200, type = 8)
      fc_upper <- apply(fc_iter, 1, stats::quantile, 0.5 + level / 200, type = 8)

      fc <- data.table::data.table(
        index = data_split[(.N-n_test+1):.N, get(col_date)],
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
      if (!is.null(col_id)) fc[, paste(col_id) := unique(DT[[col_id]])]

      ### Accuracy Measures
      fc_values <- fc[key == "predict"]
      value <- lo95 <- hi95 <- NULL

      # Point Forecast Measures
      acc_MAPE <- sapply(
        multiple_h,
        function(h) mape(actual = DT_test[h, get(col_value)], forecast = fc_values[h,value]))
      acc_sMAPE <- sapply(
        multiple_h,
        function(h) smape(actual = DT_test[h, get(col_value)], forecast = fc_values[h,value]))
      acc_MASE <- sapply(
        multiple_h, function(h) mase(
          data = DT[1:(n_initial+max(h)), get(col_value)],
          forecast = fc_values[h, get(col_value)], m = frequency)
      )

      # Prediction Interval Measures
      acc_SMIS <- sapply(
        multiple_h, function(h) smis(
          data = DT[1:(n_initial+max(h)), get(col_value)],
          lower = fc_values[h,lo95],
          upper = fc_values[h,hi95],
          h = max(h), m = frequency, level = level/100)
      )
      acc_ACD <- sapply(multiple_h, function(h) acd(
        actual = DT_test[h, get(col_value)],
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
  X <- Y <- NULL
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
