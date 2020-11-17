#' Train Recurrent Neural Network with Gated Recurrent Unit (GRU) or
#' Long-Short Term Memory (LSTM) using Keras framework
#'
#' @param X list of "train", "val", and "test" with 3D (keras) arrays
#' @param Y list of "train", "val", and "test" with 2D (keras) arrays
#' @param model_type One of "basic", "gru" and "lstm"
#' @param tsteps number of time steps for keras input shape
#' @param n_epochs default 200
#' @param loss default "mae"
#' @param metrics default "mse"
#' @param optimizer_type One of "rmsprop" (default) and "adam"
#' @param dropout dropout rate
#' @param recurrent_dropout Dropout rate applied to reccurent layer. Default 0
#' @param n_units 32 (currently fixed)
#' @param patience when to stop early (default 10)
#'
#' @import keras
#'
#' @return evaluation scores for training, validation and test set
#' @export
keras_sequential <- function(
  X, Y,
  model_type,
  tsteps,
  n_epochs = 200,
  n_units = 32,
  loss = "mse",
  metrics = NULL,
  optimizer_type = "rmsprop",
  dropout = 0,
  recurrent_dropout = 0,
  patience = NULL,
  live_plot = FALSE
) {

  # Hyperparameter -------------------------------------------------------------

  if (is.null(n_epochs)) n_epochs <- 200
  if (is.null(n_units)) n_units <- 32
  if (is.null(optimizer_type)) optimizer_type <- "rmsprop"
  if (is.null(dropout)) dropout <- 0
  if (is.null(recurrent_dropout)) recurrent_dropout <- 0
  if (is.null(patience)) patience <- 10

  optimizer <- switch(
    optimizer_type,
    adam = optimizer_adam(),
    rmsprop = optimizer_rmsprop()
  )

  callbacks <- list(
    callback_early_stopping(patience = patience)
  )

  # Training and Evaluation ----------------------------------------------------

  model <- keras_model_sequential()

  if (model_type == "simple") {
    model %>%
      layer_simple_rnn(
        units             = n_units,
        input_shape       = c(tsteps, 1),
        dropout           = dropout,
        recurrent_dropout = recurrent_dropout
      ) %>%
      layer_dense(units = 1)

  } else if (model_type == "gru") {
    # a. GRU
    model %>% layer_gru(
      units             = n_units,
      input_shape       = c(tsteps, 1),
      dropout           = dropout,
      recurrent_dropout = recurrent_dropout
    ) %>%
      layer_dense(units = 1)

  } else if (model_type == "lstm") {
    # b. LSTM
    model %>%
      layer_lstm(
        units             = n_units,
        input_shape       = c(tsteps, 1),
        dropout           = dropout,
        recurrent_dropout = recurrent_dropout
      ) %>%
      layer_dense(units = 1)
  }

  model %>% compile(
    optimizer = optimizer,
    loss = loss,
    metrics = metrics
  )

  model %>% fit(
    x               = X$train,
    y               = Y$train,
    steps_per_epoch = 1,
    epochs          = n_epochs,
    batch_size      = NULL,
    verbose         = 0,
    shuffle         = FALSE,
    validation_data = list(X$val, Y$val),
    callbacks       = callbacks,
    view_metrics    = live_plot
  )

  return(model)

  # output <- list(
  #   model = if (return_model) model,
  #   # train = evaluate(model, X$train, Y$train, verbose = 0),
  #   val = if (dim(X$val)[1] > 0) evaluate(model, X$val, Y$val, verbose = 0),
  #   test = if (dim(X$test)[1] > 0) evaluate(model, X$test, Y$test, verbose = 0)
  # )
  #
  # return(output)
}
