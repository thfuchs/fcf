#' Automatic cross-validated tuning of recurrent neural networks for time series
#' data
#'
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
#' @param save_id optional character id for model filename
#'
#' @section Tuning Bounds:
#' The following parameters are (currently) available for tuning.\cr
#' - lag_1 (integer(2)) lower bounds - default `c(1L, 2L)`
#' - lag_2 (integer(2)) upper bounds - default `c(1L, 2L)`
#' - n_units (integer(2)) lower and upper bound for rnn units (cells) - default `c(8L, 32L)`
#' - n_epochs (integer(2)) lower and upper bound for epochs - default `c(20L, 50L)`
#' - optimizer_type (integer(2)) lower and upper bound for optimizer:
#'   1 = "rmsprop", 2 = "adam", 3 = "adagrad" - default `c(1L, 3L)`
#' - dropout = (numeric(2)) lower and upper bound for dropout rate - default `c(0, 0.5)`
#' - recurrent_dropout = (numeric(2)) lower and upper bound for recurrent dropout rate - default `c(0, 0.5)`
#' - learning_rate = (numeric(2)) lower and upper bound for learning rate - default `c(0.001, 0.01)`
#' Keep attention to the correct type (numeric length 2 / integer length 2).
#' All bounds are to be set, otherwise default serves as fallback.
#'
#' @import data.table
#' @import keras
#' @importFrom magrittr %>%
#' @importFrom zeallot %<-%
#'
#' @family RNN tuning with Keras
#' @return list of Bayes Optimization results per split
#' @export
#'
#' @examples
#' \dontrun{
#' apple <- tsRNN::DT_apple
#'
#' cv_setting <- list(
#'   periods_train = 90,
#'   periods_val = 10,
#'   periods_test = 10,
#'   skip_span = 5
#' )
#'
#' bayes <- tune_keras_rnn_bayesoptim(apple, model_type = "simple", cv_setting)
#' bayes
#' }
tune_keras_rnn_bayesoptim <- function(
                                      data,
                                      model_type,
                                      cv_setting,
                                      tuning_bounds = list(),
                                      col_id = NULL,
                                      col_date = "index",
                                      col_value = "value",
                                      save = NULL,
                                      save_id = NULL) {

  # Checks ---------------------------------------------------------------------
  testr::check_class(data, "data.frame")
  testr::check_class(model_type, "character", n = 1)
  model_type <- rlang::arg_match(model_type, c("simple", "gru", "lstm"))
  testr::check_class(cv_setting, "list")
  testr::check_class(tuning_bounds, "list")
  testr::check_class(col_id, "character", n = 1, allowNULL = TRUE)
  testr::check_class(col_date, "character", n = 1)
  testr::check_class(col_value, "character", n = 1)
  testr::check_class(save, "character", n = 1, allowNULL = TRUE)
  testr::check_class(save_id, "character", n = 1, allowNULL = TRUE)

  data.table::setDT(data)

  check_data_structure(data, col_id, col_date, col_value)
  check_cv_setting(cv_setting)

  # set defaults for "tuning_bounds"
  if (is.null(tuning_bounds[["lag_1"]])) tuning_bounds[["lag_1"]] <- c(1L, 2L)
  if (is.null(tuning_bounds[["lag_2"]])) tuning_bounds[["lag_2"]] <- c(1L, 2L)
  if (is.null(tuning_bounds[["n_units"]])) tuning_bounds[["n_units"]] <- c(8L, 32L)
  if (is.null(tuning_bounds[["n_epochs"]])) tuning_bounds[["n_epochs"]] <- c(20L, 50L)
  if (is.null(tuning_bounds[["optimizer_type"]])) tuning_bounds[["optimizer_type"]] <- c(1L, 3L)
  if (is.null(tuning_bounds[["dropout"]])) tuning_bounds[["dropout"]] <- c(0, 0.5)
  if (is.null(tuning_bounds[["recurrent_dropout"]])) tuning_bounds[["recurrent_dropout"]] <- c(0, 0.5)
  if (is.null(tuning_bounds[["learning_rate"]])) tuning_bounds[["learning_rate"]] <- c(0.001, 0.01)

  # check "tuning_bounds"
  testr::check_class(tuning_bounds[["lag_1"]], "integer", n = 2)
  testr::check_class(tuning_bounds[["lag_2"]], "integer", n = 2)
  testr::check_class(tuning_bounds[["n_units"]], "integer", n = 2)
  testr::check_class(tuning_bounds[["n_epochs"]], "integer", n = 2)
  testr::check_class(tuning_bounds[["optimizer_type"]], "integer", n = 2)
  testr::check_class(tuning_bounds[["dropout"]], "numeric", n = 2)
  testr::check_class(tuning_bounds[["recurrent_dropout"]], "numeric", n = 2)
  testr::check_class(tuning_bounds[["learning_rate"]], "numeric", n = 2)

  with(tuning_bounds, {
    if (any(lag_1 < 1)) rlang::abort(
      message = "tuning_bounds[[\"lag_1\"]] must be a positive integer.",
      class = "tune_keras_rnn_bayesoptim_tuning_bounds[[\"lag_1\"]]_error"
    )
    if (any(lag_2 < 1)) rlang::abort(
      message = "tuning_bounds[[\"lag_2\"]] must be a positive integer.",
      class = "tune_keras_rnn_bayesoptim_tuning_bounds[[\"lag_2\"]]_error"
    )
    if (any(n_units < 1)) rlang::abort(
      message = "tuning_bounds[[\"n_units\"]] must be a positive integer.",
      class = "tune_keras_rnn_bayesoptim_tuning_bounds[[\"n_units\"]]_error"
    )
    if (any(n_epochs < 1)) rlang::abort(
      message = "tuning_bounds[[\"n_epochs\"]] must be a positive integer.",
      class = "tune_keras_rnn_bayesoptim_tuning_bounds[[\"n_epochs\"]]_error"
    )
    if (optimizer_type < 1 || optimizer_type > 3) rlang::abort(
      message = "tuning_bounds[[\"optimizer_type\"]] must be within interval [1L, 3L].",
      class = "tune_keras_rnn_bayesoptim_tuning_bounds[[\"optimizer_type\"]]_error"
    )
    if (dropout < 0 || dropout > 1) rlang::abort(
      message = "tuning_bounds[[\"dropout\"]] must be within interval [0, 1].",
      class = "tune_keras_rnn_bayesoptim_tuning_bounds[[\"dropout\"]]_error"
    )
    if (recurrent_dropout < 0 || recurrent_dropout > 1) rlang::abort(
      message = "tuning_bounds[[\"recurrent_dropout\"]] must be within interval [0, 1].",
      class = "tune_keras_rnn_bayesoptim_tuning_bounds[[\"recurrent_dropout\"]]_error"
    )
    if (learning_rate < 0 || learning_rate > 1) rlang::abort(
      message = "tuning_bounds[[\"learning_rate\"]] must be within interval [0, 1].",
      class = "tune_keras_rnn_bayesoptim_tuning_bounds[[\"learning_rate\"]]_error"
    )
  })

  # Check whether directory exists
  if (!is.null(save) && !dir.exists(save)) {
    rlang::abort(
      message = "Directory specified in `save` does not exist.",
      class = "tune_keras_rnn_bayesoptim_save_error"
    )
  }

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
        n_iter = 30,
        acq = "ucb",
        verbose = FALSE
      )
      if (!is.null(save)) {
        saveRDS(bayes, file = file.path(save, paste0(
          "bayes_",
          format(Sys.time(), "%Y%m%d_%H%M%S_"),
          if (!is.null(save_id)) paste0(save_id, "_"),
          model_type, "_",
          if (!is.null(col_id)) paste0(unique(data_split[[col_id]]), "_"),
          split$id$id, ".rds"
        )))
      }

      return(bayes)
    }, otherwise = NULL, quiet = FALSE)
  )

  resample <- purrr::set_names(resample, rolling_origin_resamples$id)
  return(resample)
}

# Internal Function ------------------------------------------------------------
internal_keras_fun <- function(
                               n_units,
                               n_epochs,
                               lag_1,
                               lag_2,
                               dropout,
                               recurrent_dropout,
                               optimizer_type,
                               learning_rate) {

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
      units = n_units,
      input_shape = c(length(lag_setting), 1),
      recurrent_dropout = recurrent_dropout
    )
  } else if (model_type == "gru") {
    layer_gru(
      units = n_units,
      input_shape = c(length(lag_setting), 1),
      recurrent_dropout = recurrent_dropout
    )
  } else if (model_type == "lstm") {
    layer_lstm(
      units = n_units,
      input_shape = c(length(lag_setting), 1),
      recurrent_dropout = recurrent_dropout
    )
  }

  dropout_layer <- layer_dropout(rate = dropout)

  output <- input %>%
    recurrent_layer() %>%
    dropout_layer() %>%
    layer_dense(units = 1)

  model <- keras_model(input, output)

  model %>% compile(optimizer = optimizer, loss = "mse")

  history <- model %>% fit(
    x = X$train,
    y = Y$train,
    steps_per_epoch = 1,
    epochs = n_epochs,
    batch_size = NULL,
    verbose = 0,
    shuffle = FALSE,
    validation_data = list(X$val, Y$val),
    view_metrics = FALSE
  )

  return(list(Score = -history$metrics$val_loss[n_epochs], Pred = 0))
}
