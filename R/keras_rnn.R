#' Train Recurrent Neural Network with Gated Recurrent Unit (GRU) or
#' Long-Short Term Memory (LSTM) using Keras framework
#'
#' @param X list of "train", "val", and "test" with 3D (keras) arrays
#' @param Y list of "train", "val", and "test" with 2D (keras) arrays
#' @param model_type One of "simple", "gru" and "lstm"
#' @param tsteps number of time steps for keras input shape
#' @param n_epochs default 200
#' @param loss default "mse"
#' @param metrics default NULL
#' @param optimizer from keras, e.g. `optimizer_rmsprop()`
#' @param dropout dropout rate
#' @param recurrent_dropout Dropout rate applied to reccurent layer. Default 0
#' @param n_units 32 (currently fixed)
#' @param dropout_in_test apply dropout during training only (default) or during
#' testing also? Required for dropout-based prediction intervals (bayesian RNN)
#' @param live_plot plot loss and validation metric during training? False by
#' default
#'
#' @import keras
#'
#' @return evaluation scores for training, validation and test set
#' @export
keras_rnn <- function(
  X, Y,
  model_type,
  tsteps,
  n_epochs = 200,
  n_units = 32,
  loss = "mse",
  metrics = NULL,
  dropout_in_test = FALSE,
  optimizer = optimizer_rmsprop(),
  dropout = 0,
  recurrent_dropout = 0,
  live_plot = FALSE
) {

  # Hyperparameter -------------------------------------------------------------

  if (is.null(n_epochs)) n_epochs <- 200
  if (is.null(n_units)) n_units <- 32
  if (is.null(optimizer)) optimizer <- optimizer_rmsprop()
  if (is.null(dropout)) dropout <- 0
  if (is.null(recurrent_dropout)) recurrent_dropout <- 0

  # Training and Evaluation ----------------------------------------------------

  input <- layer_input(shape = c(tsteps, 1))

  recurrent_layer <- if (model_type == "simple") {
    layer_simple_rnn(
      units             = n_units,
      input_shape       = c(tsteps, 1),
      # dropout           = dropout,
      recurrent_dropout = recurrent_dropout
    )
  } else if (model_type == "gru") {
    layer_gru(
      units             = n_units,
      input_shape       = c(tsteps, 1),
      # dropout           = dropout,
      recurrent_dropout = recurrent_dropout
    )
  } else if (model_type == "lstm") {
    layer_lstm(
      units             = n_units,
      input_shape       = c(tsteps, 1),
      # dropout           = dropout,
      recurrent_dropout = recurrent_dropout
    )
  }

  dropout_layer <- layer_dropout(rate = dropout)

  # Apply dropout only during training (Keras default) or during testing also?
  output <- if (dropout_in_test) {
    input %>% recurrent_layer %>% dropout_layer(training = TRUE) %>% layer_dense(units = 1)
  } else {
    input %>% recurrent_layer %>% dropout_layer %>% layer_dense(units = 1)
  }

  model <- keras_model(input, output)

  model %>% compile(optimizer = optimizer, loss = loss, metrics = metrics)

  model %>% fit(
    x               = X$train,
    y               = Y$train,
    steps_per_epoch = 1,
    epochs          = n_epochs,
    batch_size      = NULL,
    verbose         = 0,
    shuffle         = FALSE,
    validation_data = if (dim(X$val)[1] > 0) list(X$val, Y$val),
    view_metrics    = live_plot
  )

  return(model)
}
