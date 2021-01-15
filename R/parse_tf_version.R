#' Parse installed TensorFlow version
#'
#' @export
#' @return numeric vector length 1
#'
#' @examples
#' parse_tf_version()
parse_tf_version <- function() {
  tf <- tensorflow::tf_version()
  as.numeric(gsub("^(\\d+)[\\.-](\\d+)([\\.-]\\d+)*$", "\\1.\\2", tf))
}
