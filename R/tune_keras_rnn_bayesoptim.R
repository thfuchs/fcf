#' Tune recurrent neural network with Keras functional API and Bayes
#' Optimization to select best performing model
#'
#' @param data Univariate time series (data.frame) with date and value column,
#'   specified in `col_date` and `col_value`
#' @param model_type One of "simple", "gru" or "lstm"
#' @param cv_setting list of "periods_train", "periods_val", "periods_test" and
#'   "skip_span" for \link[rsample]{rolling_origin}
#' @param tuning_bounds list of tuning parameters - see section "Tuning Bounds"
#' @param col_id Optional ID column in `data`, default to "ticker"
#' @param col_date Date column in `data`, default to "index"
#' @param col_value Value column in `data`, default to "value"
#' @param save Automatically save tuning results? Specify NULL if not or
#' character vector with path to directory for yes
#' @param save_id optional id for model filename
#'
#' @section Tuning Bounds:
#' The following parameters are (currently) available for tuning.
#' - lag_1 (integer(2)) lower bounds
#' - lag_2 (integer(2)) upper bounds
#' - n_units (integer(2)) lower and upper bound for rnn units (cells)
#' - n_epochs (integer(2)) lower and upper bound for epochs
#' - optimizer_type (integer(2)) lower and upper bound for optimizer:
#'   1 = "rmsprop", 2 = "adam", 3 = "adagrad"
#' - dropout = (numeric(2)) lower and upper bound for dropout rate
#' - recurrent_dropout = (numeric(2)) lower and upper bound for recurrent dropout rate
#' - learning_rate = (numeric(2)) lower and upper bound for learning rate
#'
#' @import data.table
#' @import keras
#' @importFrom magrittr %>%
#' @importFrom zeallot %<-%
#'
#' @return list of Bayes Optimization results per split
#' @export
tune_keras_rnn_bayesoptim <- function(
  data,
  model_type,
  cv_setting,
  tuning_bounds,
  col_id = NULL,
  col_date = "index",
  col_value = "value",
  save = NULL,
  save_id = NULL
) {

  # Checks ---------------------------------------------------------------------
  testr::check_class(data, "data.frame", "tune_keras_rnn_bayesoptim")
  testr::check_class(model_type, "character", "tune_keras_rnn_bayesoptim")
  testr::check_class(cv_setting, "list", "tune_keras_rnn_bayesoptim")
  testr::check_class(col_id, "character", "tune_keras_rnn_bayesoptim", allowNULL = TRUE)
  testr::check_class(col_date, "character", "tune_keras_rnn_bayesoptim")
  testr::check_class(col_value, "character", "tune_keras_rnn_bayesoptim")
  testr::check_class(tuning_bounds, "list", "tune_keras_rnn_bayesoptim")

  # "data" contains columns "index" and "value" (and optionally "id")
  # (univariate time series)
  data.table::setDT(data)
  if (
    !is.null(col_id) && is.null(data[[col_id]]) ||
    !is.null(col_id) && !inherits(data[[col_id]], "numeric")
  ) rlang::abort(
    message = "Variable specified by `col_id` must be class \"character\".",
    class = "tune_keras_rnn_bayesoptim_col_id_error"
  )
  if (
    is.null(data[[col_date]]) ||
    !rlang::inherits_any(data[[col_date]], c("Date", "POSIXct"))
  ) rlang::abort(
    message = "Variable specified by `col_date` must be class \"Date\" or \"POSIXct\".",
    class = "tune_keras_rnn_bayesoptim_col_date_error"
  )
  if (is.null(data[[col_value]]) || !inherits(data[[col_value]], "numeric"))
    rlang::abort(
      message = "Variable specified by `col_value` must be class \"numeric\".",
      class = "tune_keras_rnn_bayesoptim_col_value_error"
    )

  # "model_type" must be one of "simple", "gru" or "lstm"
  model_type <- rlang::arg_match(model_type, c("simple", "gru", "lstm"))

  # "cv_setting" contains "periods_train", "periods_val", "periods_test" and
  # "skip_span"
  if (all(names(cv_setting)[order(names(cv_setting))] !=
          c("periods_test", "periods_train", "periods_val", "skip_span"))) {
    rlang::abort(
      message = "`data` must be a data.frame with 2 columns only: \"index\" and \"value\"",
      class = "tune_keras_rnn_bayesoptim_data_error"
    )
  }

  # Check whether directory exists
  if (!is.null(save) && !dir.exists(save)) rlang::abort(
    message = "Directory specified in `save` does not exist",
    class = "tune_keras_rnn_bayesoptim_save_error"
  )

  # Function -------------------------------------------------------------------
  n_train <- cv_setting$periods_train
  n_val <- cv_setting$periods_val
  n_initial <- n_train + n_val
  n_test <- cv_setting$periods_test
  lag_lower <- min(tuning_bounds$lag_1, tuning_bounds$lag_2)
  lag_upper <- max(tuning_bounds$lag_1, tuning_bounds$lag_2)

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

      # Add lagged values to data
      DT <- setDT(rbind(rsample::analysis(split), rsample::assessment(split)))
      add_shift(DT, cols = col_value, nlags = c(lag_lower:lag_upper), type = "lag")
      DT <- DT[!is.na(get(paste0("value_lag", lag_upper)))]
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
        n_iter = 50,
        acq = "ucb", #ei,
        verbose = FALSE
      )
      if (!is.null(save)) {
        save(bayes, file = file.path(save, paste0(
          "bayes_",
          format(Sys.time(), "%Y%m%d_%H%M%S_"),
          if (!is.null(save_id)) paste0(save_id, "_"),
          model_type, "_",
          if (!is.null(col_id)) paste0(unique(data_split[[col_id]]), "_"),
          split$id$id, ".rda"
        )))
      }

      return(bayes)

    }, otherwise = NULL, quiet = FALSE)
  )

  purrr::set_names(resample, rolling_origin_resamples$id)
  return(resample)
}

# Internal Function ------------------------------------------------------------
internal_keras_fun <- function(
  n_units, n_epochs, lag_1, lag_2, dropout, recurrent_dropout,
  optimizer_type, learning_rate
) {

  lag_setting <- sort(lag_1:lag_2)
  optimizer <- switch(
    optimizer_type,
    `1` = optimizer_rmsprop(lr = learning_rate),
    `2` = optimizer_adam(lr = learning_rate),
    `3` = optimizer_adagrad(lr = 0.01)
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

  recurrent_layer <- if (model_type == "simple") {
    layer_simple_rnn(
      units             = n_units,
      input_shape       = c(length(lag_setting), 1),
      recurrent_dropout = recurrent_dropout
    )
  } else if (model_type == "gru") {
    layer_gru(
      units             = n_units,
      input_shape       = c(length(lag_setting), 1),
      recurrent_dropout = recurrent_dropout
    )
  } else if (model_type == "lstm") {
    layer_lstm(
      units             = n_units,
      input_shape       = c(length(lag_setting), 1),
      recurrent_dropout = recurrent_dropout
    )
  }

  dropout_layer <- layer_dropout(rate = dropout)

  output <- input %>% recurrent_layer %>% dropout_layer %>% layer_dense(units = 1)

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
