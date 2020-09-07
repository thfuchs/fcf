#' Basic sequential NN by Keras framework using \link[tfruns](flags)
#'
#' @param X list of "train", "val", and "test" with 3D (keras) arrays
#' @param Y list of "train", "val", and "test" with 2D (keras) arrays
#'
#' @section FLAGS:
#' The following parameters are currently required:
#' - optimizer
#' - loss
#' - metrics
#' - patience
#'
#' @import keras
#'
#' @return history
#' @export
keras_basic_sequential <- function(X, Y) {

  # Hyperparameter flags ---------------------------------------------------

  FLAGS <- flags(
    flag_integer("n_epochs", 200),
    flag_string("loss", "mae"),
    flag_string("metrics", "mse"),
    flag_string("optimizer_type", "rmsprop"),
    flag_integer("n_units", 32),
    flag_numeric("lr", 0.003),
    flag_numeric("momentum", 0.9),
    flag_integer("patience", 10)
  )

  optimizer <- switch(
    FLAGS$optimizer_type,
    adam = optimizer_adam(),
    sgd = optimizer_sgd(lr = FLAGS$lr, momentum = FLAGS$momentum),
    rmsprop = optimizer_rmsprop()
  )

  callbacks <- list(
    callback_early_stopping(patience = FLAGS$patience)
  )

  # Training and Evaluation ----------------------------------------------------

  model <- keras_model_sequential()
  model %>%
    layer_dense(units = 32, activation = "relu") %>%
    layer_dense(units = 1)

  model %>% compile(
    optimizer = optimizer,
    loss = FLAGS$loss,
    metrics = FLAGS$metrics
  )

  history <- model %>% fit(
    x               = X$train,
    y               = Y$train,
    steps_per_epoch = 1,
    epochs          = FLAGS$n_epochs,
    batch_size      = NULL,
    verbose         = 1,
    shuffle         = FALSE,
    validation_data = list(X$val, Y$val),
    callbacks       = callbacks
  )

  score <- model %>% evaluate(X$test, Y$test, verbose = 0)

  cat('Test loss:', score["loss"], '\n')
  cat('Test accuracy:', score["mse"], '\n')

  return(history)
}
