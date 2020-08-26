#' Add differences to time series data.table object for specified columns
#'
#' @param DT  data.table object with indexed time variable
#' @param cols columns to use for differencing
#' @param ndiff number of lags
#'
#' @return nothing returned but data.table edited by reference
add_diffs <- function(DT, cols, ndiff){
  for (colname in cols){
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
#' @return nothing returned but data.table edited by reference
add_growth_rates <- function(DT, cols, ndiff){
  for (colname in cols){
    new_name <- paste0(colname, "_pctchg", ndiff)
    DT[, (new_name) := (get(colname) / shift(get(colname), type = "lag", n = ndiff)) - 1]
  }
}
