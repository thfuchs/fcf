#' Change dropout rate in recurrent layer or dropout layer of trained model
#'
#' Wrapper for python function. Model requires fixed training mode (else dropout
#' turned off automatically during testing and changing dropout rate has no
#' effect)
#'
#' @param model Keras model, required
#' @param dropout new dropout rate
#'
#' @return Keras model
#' @export
#'
#' @examples
#' # Load Model with permanently activated dropout and generate test data
#' model <- keras::load_model_hdf5("inst/tinytest_data/simple_dropout_rnn.hdf5")
#' test <- array(runif(18, 0.1, 3), dim = c(6L, 3L, 1L))
#'
#' # 1. Deactivate dropout for testing
#' model_01 <- py_dropout_model(best_model, 0)
#' predict(model_01, test)
#'
#' # 2. Change dropout level
#' model_02 <- py_dropout_model(best_model, 0.8)
#' predict(model_02, test)
py_dropout_model <- function(model, dropout = 0.1) {

  python_path <- system.file("python", "dropout_model.py", package = "fcf")
  reticulate::source_python(python_path)

  dropout_model(model, dropout)
}