#' Normalize univariate timeseries
#'
#' @param data univariate time series (data.frame / data.table)
#' @param length_val length for validation set
#' @param length_test length for test set
#' @param value_col column(s) to normalize, searched by starting pattern. E.g.
#'   `value_col = "index"` will catch column "index" and "index_2" but not
#'   "2_index"
#' @param joined joined normalization for same pattern? TRUE by default. See
#' section "Joined value columns" for details
#' @param metrics return data only or list of data and metrics?
#'
#' @section Joined value columns:
#' Joined means to normalize all columns detected by pattern with the one column
#' exactly matching. Watch out for this condition to hold if `joined = TRUE`.\cr
#' `joined` is of particular use for lagged time series. E.g. column "value"
#' should be used to normalize not only column "value" but also "value_lag1" etc.
#'
#' @import data.table
#'
#' @return Depending on `metrics`, processed DT object or list of "data" and
#'   "metrics" (center and scale)
#' @export
#'
#' @examples
#' # without metrics
#' DT_norm <- ts_normalization(tsRNN::DT_apple, 10, 10); DT_norm
#'
#' # with metrics
#' ts_normalization(tsRNN::DT_apple, 10, 10, metrics = TRUE)
#'
ts_normalization <- function(data,
                             length_val,
                             length_test,
                             value_col = "value",
                             joined = TRUE,
                             metrics = FALSE) {

  ### Checks -------------------------------------------------------------------

  testr::check_class(data, "data.frame")
  testr::check_num_int(length_val, n = 1)
  testr::check_num_int(length_test, n = 1)
  testr::check_class(value_col, "character", allowNULL = TRUE)
  testr::check_class(joined, "logical")
  testr::check_class(metrics, "logical")

  DT <- data.table::copy(data)
  data.table::setDT(DT)
  n <- nrow(DT) - length_val - length_test

  if (n <= 1) rlang::abort(
    message = paste(
      "Number of rows of \"data\" must be higher then sum of \"length_val\"",
      "and \"length_test\"."),
    class = "ts_normalization_data_error"
  )

  # if joined TRUE, "value_col" must match column in data
  if (joined && any(!value_col %in% names(data))) rlang::abort(
    message = "If `joined = TRUE`, \"value_col\" must match column in \"data\".",
    class = "ts_normalization_value_col_error"
  )

  # Check class of "value_col"
  cols <- names(DT)[grepl(paste0("^", value_col, collapse = "|"), names(DT))]
  cols_check <- sapply(DT[, .SD, .SDcols = cols], is.numeric)
  if (!all(cols_check)) {
    rlang::abort(
      message = sprintf(
        "%s \"%s\" matched by pattern of \"value_col\" in \"data\" must be numeric.",
        if (sum(!cols_check) > 1) "Columns" else "Column",
        paste0(names(DT)[!cols_check], collapse = "\", \"")
      ),
      class = "ts_normalization_value_col_error"
    )
  }

  ### Function -----------------------------------------------------------------

  train <- DT[1:n]

  scale_metrics <- if (joined) {

    mean <- train[, lapply(.SD, mean, na.rm = TRUE), .SDcols = value_col]
    std <- train[, lapply(.SD, stats::sd, na.rm = TRUE), .SDcols = value_col]

    set_value_col <- sapply(cols, function(x) {
      value_col[sapply(value_col, function(y) grepl(paste0("^", y), x))]
    }, simplify = FALSE)
    set_value_col <- unlist(set_value_col)

    list(
      center = sapply(set_value_col, function(x) mean[[x]]),
      scale = sapply(set_value_col, function(x) std[[x]])
    )
  } else {
    list(
      center = train[, lapply(.SD, mean, na.rm = TRUE), .SDcols = cols],
      scale = train[, lapply(.SD, stats::sd, na.rm = TRUE), .SDcols = cols]
    )
  }

  DT[, (cols) := purrr::map2(
    .SD,
    purrr::transpose(scale_metrics),
    ~ scale(.x, .y$center, .y$scale)
  ), .SDcols = cols]

  # Remove "duplicates" in scale_metrics due to matching process
  if (joined) scale_metrics <- purrr::map(scale_metrics, ~ .x[value_col])

  # Output
  if (!metrics) return(DT)
  return(list(data = DT, metrics = scale_metrics))
}
