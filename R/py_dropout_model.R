#' Change dropout rate in recurrent layer or dropout layer of trained model
#'
#' Wrapper for python function. Model requires fixed training mode (else dropout
#' turned off automatically during testing and changing dropout rate has no
#' effect)
#'
#' @details Currently requires python warnings wrapper to suppress tensorflow
#'   warning
#'
#' @param model Keras model, required
#' @param dropout new dropout rate
#'
#' @return Keras model
#' @export
#'
#' @examples
#' # Load Model with permanently activated dropout and generate test data
#' model <- keras::load_model_hdf5(
#'   system.file("tinytest_data/simple_rnn_dropout.hdf5", package = "tsRNN")
#' )
#' test <- array(runif(18, 0.1, 3), dim = c(6L, 2L, 1L))
#'
#' # 1. Deactivate dropout for testing
#' model_01 <- py_dropout_model(model, 0)
#' model_01(test)
#'
#' # 2. Change dropout level
#' model_02 <- py_dropout_model(model, 0.8)
#' model_02(test)
py_dropout_model <- function(model, dropout = 0.1) {

  ### Checks -------------------------------------------------------------------

  # TensorFlow version
  tf_v <- parse_tf_version()
  if (tf_v < 2.3) rlang::abort(
    message = "TensorFlow version below 2.3 detected. Please update to at least version 2.3.0",
    class = "py_dropout_model_tensorflow_error"
  )
  # Class of arguments
  testr::check_class(model, "python.builtin.object")
  testr::check_class(model, "keras.engine.training.Model")
  testr::check_num_int(dropout)

  ### Function -----------------------------------------------------------------

  # for R binding
  dropout_model <- NULL

  # read Python file
  python_path <- system.file("python", "dropout_model.py", package = "tsRNN")
  reticulate::source_python(python_path)

  # Output
  dropout_model(model, dropout)
}
