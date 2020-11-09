#' Basic sequential NN by Keras framework
#'
#' Using \link[tfruns](flags)
#'
#' @param X list of "train", "val", and "test" with 3D (keras) arrays
#' @param Y list of "train", "val", and "test" with 2D (keras) arrays
#' @param n_epochs default 200
#' @param loss default "mae"
#' @param metrics default "mse"
#' @param optimizer_type One of "rmsprop" (default) and "adam"
#' @param dropout dropout rate
#' @param n_units 32 (currently fixed)
#' @param patience when to stop early (default 10)
#'
#' @import keras
#'
#' @return evaluation scores for training, validation and test set
#' @export
keras_basic_sequential <- function(
  X, Y,
  return_model = FALSE,
  n_epochs = 200,
  loss = "mae",
  metrics = c("mse"),
  optimizer_type = "rmsprop",
  dropout = 0.2,
  n_units = 32,
  patience = 10
) {

  # Hyperparameter -------------------------------------------------------------

  if (is.null(n_epochs)) n_epochs <- 200
  if (is.null(optimizer_type)) optimizer_type <- "rmsprop"
  if (is.null(dropout)) dropout <- 0.2
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
  model %>%
    layer_dense(units = 32, activation = "relu") %>%
    layer_dense(units = 1)

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
    verbose         = 1,
    shuffle         = FALSE,
    validation_data = list(X$val, Y$val),
    callbacks       = callbacks
  )

  output <- list(
    model = if (return_model) model,
    train = evaluate(model, X$train, Y$train, verbose = 0),
    val = if (dim(X$val)[1] > 0) evaluate(model, X$val, Y$val, verbose = 0),
    test = if (dim(X$test)[1] > 0) evaluate(model, X$test, Y$test, verbose = 0)
  )

  return(output)
}
