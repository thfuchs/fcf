#' predict for keras models
#'
#' `py_suppress_warnings` wrapped around predict
#'
#' @param model keras model
#' @param x array (with shape similar to input)
#'
#' @export
#' @return keras array
dropout_predict <- function(model, x) {
  reticulate::py_suppress_warnings(stats::predict(model, x))
}
