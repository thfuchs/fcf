#' Add shifts to time series data.table object for specified columns
#'
#' @param DT  data.table object with indexed time variable
#' @param cols columns to use for shifting (character)
#' @param nlags number of lags
#' @param type one of "lag", "lead" and "shift"
#'
#' @import data.table
#' @noRd
#' @return nothing returned, DT edited by reference
add_shift <- function(DT, cols, nlags, type = "lag") {
  for (colname in cols) {
    new_name <- paste0(colname, "_", type, nlags)
    DT[, (new_name) := shift(get(colname), type = type, n = nlags)]
  }
}

#' Add differences to time series data.table object for specified columns
#'
#' @param DT  data.table object with indexed time variable
#' @param cols columns to use for differencing (character)
#' @param ndiff number of lags
#'
#' @import data.table
#' @noRd
#' @return nothing returned, DT edited by reference
add_diffs <- function(DT, cols, ndiff) {
  for (colname in cols) {
    new_name <- paste0(colname, "_diff", ndiff)
    DT[, (new_name) := get(colname) - shift(get(colname), type = "lag", n = ndiff)]
  }
}

#' Add growth rates to time series data.table object for specified columns
#'
#' @param DT  data.table object with indexed time variable
#' @param cols columns to use for growth rates
#' @param ndiff number of lags
#'
#' @import data.table
#' @noRd
#' @return nothing returned, DT edited by reference
add_growth_rates <- function(DT, cols, ndiff) {
  for (colname in cols) {
    new_name <- paste0(colname, "_pctchg", ndiff)
    DT[, (new_name) := (get(colname) / shift(get(colname), type = "lag", n = ndiff)) - 1]
  }
}

#' Helper function to check `cv_setting`
#'
#' @param cv_setting from parent function
#'
#' @noRd
#' @return NULL if as expected, else throwing error
check_cv_setting <- function(cv_setting) {
  fun_name <- if (sys.parent() > 0) deparse(sys.call(sys.parent())[[1]])
  # 1. check type list
  testr::check_class(cv_setting, "list")
  # 2. Check names
  if (is.null(names(cv_setting)) ||
      !all(c("periods_train", "periods_val", "periods_test", "skip_span") %in%
           names(cv_setting))
  ) {
    rlang::abort(
      message = paste0(
        "`cv_setting` must be a named list containing \"periods_test\", ",
        "\"periods_train\", \"periods_val\", \"skip_span\"."),
      class = paste0(paste0(fun_name, "_", recycle0 = TRUE), "cv_setting_error")
    )
  }
  # 3. Check classes
  for (i in seq_along(cv_setting)) {
    if (
      !rlang::inherits_any(cv_setting[[i]], c("numeric", "integer")) ||
      length(cv_setting[[i]]) != 1
    ) {
      rlang::abort(
        message = sprintf(
          "`%s` in `cv_setting` must be numeric(1), not %s(%s).",
          names(cv_setting)[i],
          paste(class(cv_setting[[i]]), collapse = " / "),
          length(cv_setting[[i]])
        ),
        class = paste0(paste0(fun_name, "_", recycle0 = TRUE), "cv_setting_error")
      )
    }
  }
}

#' Helper function to cross-check "data" columns in cross-validation functions
#'
#' Used by `cv_baselines`, `cv_arima`, `tune_keras_rnn_bayesoptim`,
#' `tune_keras_rnn_predict`
#'
#' @param data Univariate time series (data.frame)
#' @param col_id Optional ID column in `data`, default to "ticker"
#' @param col_date Date column in `data`, default to "index"
#' @param col_value Value column in `data`, default to "value"
#'
#' @noRd
#' @return NULL if as expected, else throwing error
check_data_structure <- function(data, col_id, col_date, col_value) {
  fun_name <- if (sys.parent() > 0) deparse(sys.call(sys.parent())[[1]])

  if (
    !is.null(col_id) && is.null(data[[col_id]]) ||
    !is.null(col_id) && !inherits(data[[col_id]], "character")
  ) {
    rlang::abort(
      message = "Variable specified by `col_id` must be class \"character\".",
      class = paste0(fun_name, "_col_id_error")
    )
  }
  if (
    is.null(data[[col_date]]) ||
    !rlang::inherits_any(data[[col_date]], c("Date", "POSIXct"))
  ) {
    rlang::abort(
      message = "Variable specified by `col_date` must be class \"Date\" or \"POSIXct\".",
      class = paste0(fun_name, "_col_date_error")
    )
  }
  if (is.null(data[[col_value]]) || !inherits(data[[col_value]], "numeric")) {
    rlang::abort(
      message = "Variable specified by `col_value` must be class \"numeric\".",
      class = paste0(fun_name, "_col_value_error")
    )
  }
}
