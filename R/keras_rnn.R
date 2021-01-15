#' Train Recurrent Neural Network with Keras
#'
#' Currently supports "simple unit", "gated recurrent unit"
#' (GRU) and "Long-Short Term Memory" (LSTM) using Keras framework (with
#' TensorFlow backend)
#'
#' @param X list of "train", "val", and "test" with 3D (keras) arrays
#' @param Y list of "train", "val", and "test" with 2D (keras) arrays
#' @param model_type One of "simple", "gru" and "lstm"
#' @param tsteps number of time steps for keras input shape
#' @param n_epochs default 200
#' @param loss default "mse"
#' @param metrics default NULL
#' @param optimizer from keras, e.g. \link[keras]{optimizer_rmsprop}
#' @param dropout dropout rate
#' @param recurrent_dropout Dropout rate applied to reccurent layer. Default 0
#' @param n_units 32 (currently fixed)
#' @param dropout_in_test apply dropout during training only (default) or during
#' testing also? Required for dropout-based prediction intervals (bayesian RNN)
#' @param history in addition to model, return model history? Beware that output
#'   changes from `model` to `list(model, history)` if  `history = TRUE`
#' @param live_plot plot loss and validation metric during training? False by
#' default
#'
#' @seealso [Keras Documentation](https://keras.io/api/layers/recurrent_layers/)
#'
#' @import keras
#'
#' @return Keras model by default (`history = FALSE`) else list with Keras model
#'   and history
#' @export
#'
#' @references \itemize{
#'   \item Chollet, Francois and others (2015). Keras. \url{https://keras.io}
#'   \item Hochreiter, S., & Schmidhuber, J. (1997). Long Short-Term Memory.
#'   Neural Computation, 9 (8), 1735-1780.
#'   \url{https://doi.org/10.1162/neco.1997.9.8.1735}
#'   \item Chung, J., Gulcehre, C., Cho, K., & Bengio, Y. (2014). Empirical
#'   Evaluation of Gated Recurrent Neural Networks on Sequence Modeling.
#'   \url{https://arxiv.org/pdf/1412.3555}
#' }
#'
#' @examples
#' data <- tsRNN::DT_apple
#' data[, value_lag1 := data.table::shift(value, type = "lag", n = 1)]
#' data <- data[!is.na(get(paste0("value_lag1")))]
#'
#' nn_arrays <- ts_nn_preparation(data, tsteps = 1L, length_val = 6L, length_test = 6L)
#' keras_rnn(nn_arrays$x, nn_arrays$y, model_type = "simple", tsteps = 1, n_epochs = 20)
#'
#' # return model and history
#' result <- keras_rnn(
#'   nn_arrays$x, nn_arrays$y, model_type = "simple", tsteps = 1, n_epochs = 20, history = TRUE
#' )
#'
#' result$model
#' result$history
#'
#' \dontrun{
#' # Plot result
#' plot(result$history)
#' }
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
                      history = FALSE,
                      live_plot = FALSE) {

  # Checks ---------------------------------------------------------------------

  testr::check_class(X, "list", n = 3)
  testr::check_class(X[["train"]], "array")
  testr::check_class(X[["val"]], "array")
  testr::check_class(Y, "list", n = 3)
  testr::check_class(Y[["train"]], "array")
  testr::check_class(Y[["val"]], "array")
  testr::check_class(model_type, "character", n = 1)
  model_type <- rlang::arg_match0(model_type, c("simple", "lstm", "gru"))
  testr::check_num_int(tsteps, n = 1)
  testr::check_num_int(n_epochs, n = 1, allowNULL = TRUE)
  testr::check_num_int(n_units, n = 1, allowNULL = TRUE)
  testr::check_class(loss, "character", n = 1)
  testr::check_class(metrics, "character", allowNULL = TRUE)
  testr::check_class(dropout_in_test, "logical")
  testr::check_class(optimizer, "python.builtin.object", allowNULL = TRUE)
  testr::check_num_int(dropout, n = 1, allowNULL = TRUE)
  testr::check_num_int(recurrent_dropout, n = 1, allowNULL = TRUE)
  testr::check_class(history, "logical")
  testr::check_class(live_plot, "logical")

  # compare "tsteps" and a dim(X)
  if (any(c(dim(X$train)[2], dim(X$val)[2], dim(X$test)[2]) != tsteps)) {
    rlang::abort(
      message = "Dimensions in `X` must match with `tsteps`.",
      class = "keras_rnn_tsteps_error"
    )
  }

  # Hyperparameter -------------------------------------------------------------

  if (is.null(n_epochs)) n_epochs <- 200
  if (is.null(n_units)) n_units <- 32
  if (is.null(optimizer)) optimizer <- optimizer_rmsprop()
  if (is.null(dropout)) dropout <- 0
  if (is.null(recurrent_dropout)) recurrent_dropout <- 0

  # "dropout" and "recurrent dropout within [0,1]
  if (dropout < 0 || dropout > 1) rlang::abort(
    message = "`dropout` must be within interval [0, 1].",
    class = "keras_rnn_dropout_error"
  )
  if (recurrent_dropout < 0 || recurrent_dropout > 1) rlang::abort(
    message = "`recurrent_dropout` must be within interval [0, 1].",
    class = "keras_rnn_recurrent_dropout_error"
  )

  # Training and Evaluation ----------------------------------------------------

  input <- layer_input(shape = c(tsteps, 1))

  recurrent_layer <- if (model_type == "simple") {
    layer_simple_rnn(
      units = n_units,
      input_shape = c(tsteps, 1),
      # dropout           = dropout,
      recurrent_dropout = recurrent_dropout
    )
  } else if (model_type == "gru") {
    layer_gru(
      units = n_units,
      input_shape = c(tsteps, 1),
      # dropout           = dropout,
      recurrent_dropout = recurrent_dropout
    )
  } else if (model_type == "lstm") {
    layer_lstm(
      units = n_units,
      input_shape = c(tsteps, 1),
      # dropout           = dropout,
      recurrent_dropout = recurrent_dropout
    )
  }

  dropout_layer <- layer_dropout(rate = dropout)

  # Apply dropout only during training (Keras default) or during testing also?
  output <- if (dropout_in_test) {
    input %>%
      recurrent_layer() %>%
      dropout_layer(training = TRUE) %>%
      layer_dense(units = 1)
  } else {
    input %>%
      recurrent_layer() %>%
      dropout_layer() %>%
      layer_dense(units = 1)
  }

  model <- keras_model(input, output)

  model %>% compile(optimizer = optimizer, loss = loss, metrics = metrics)

  result <- model %>% fit(
    x = X$train,
    y = Y$train,
    steps_per_epoch = 1,
    epochs = n_epochs,
    batch_size = NULL,
    verbose = 0,
    shuffle = FALSE,
    validation_data = if (dim(X$val)[1] > 0) list(X$val, Y$val),
    view_metrics = live_plot
  )

  if (history) return(list(model = model, history = result)) else return(model)
}
