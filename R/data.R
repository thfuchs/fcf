#' US index Dow Jones 30 quarterly cashflow data from 1990 to 2020.
#'
#' @docType data
#' @usage data(dow30)
#' @keywords datasets
#'
#' @format A data.table with 3600 rows and 9 variables (xz-compressed):
#' \describe{
#'   \item{company}{Dow Jones listed companies in (August 2020)}
#'   \item{ticker}{bbreviation of company title}
#'   \item{date}{Publishing date of (quarterly) report in POSIX format.
#'     Month and year of interest}
#'   \item{ebit}{Earnings before interest and taxes}
#'   \item{ebitda}{Earnings before intterest, taxes, depreciation and amortization}
#'   \item{net_income}{Net Income}
#'   \item{revenue}{Revenue}
#'   \item{is_adjusted_operating_income}{adjustment}
#'   \item{is_ni_available_to_common_adj}{adjusted net income}
#' }
"dow30"

#' US index Dow Jones 30 quarterly EBIT from 1990 to 2020, cleaned by missing
#' data and outliers
#'
#' @docType data
#' @usage data(dow30_clean)
#' @keywords datasets
#'
#' @format A data.table with 2880 rows and 3 variables (xz-compressed):
#' \describe{
#'   \item{ticker}{bbreviation of company title}
#'   \item{index}{Publishing date of (quarterly) report in POSIX format.
#'     Month and year of interest}
#'   \item{value}{Earnings before interest and taxes}
#' }
"dow30_clean"

#' Quarterly reported EBIT from Apple as ts object from 1995 to 2020.
#'
#' @docType data
#' @usage data(ts_apple)
#' @keywords datasets
#'
#' @format A quarterly ts object from 1995 to 2020
"ts_apple"

#' Quarterly reported EBIT from Apple as data.table object from 1995
#' to 2020.
#'
#' @docType data
#' @usage data(DT_apple)
#' @keywords datasets
#'
#' @format A quarterly data.table object from 1995 to 2020
"DT_apple"

#' Quarterly reported EBIT from UNH as ts object from 1995 to 2020.
#'
#' @docType data
#' @usage data(ts_unh)
#' @keywords datasets
#'
#' @format A quarterly ts object from 1995 to 2020
"ts_unh"

#' Quarterly reported EBIT from UNH as data.table object from 1995 to 2020.
#'
#' @docType data
#' @usage data(DT_unh)
#' @keywords datasets
#'
#' @format A quarterly data.table object from 1995 to 2020
"DT_unh"

#' Baseline Forecast (data.table object) with SNAIVE and Holt's Trend
#'
#' @docType data
#' @usage data(fc_baseline)
#' @keywords datasets
#'
#' @format A data.table with 34 rows and 3 variables:
#' \describe{
#'   \item{index}{Publishing date of (quarterly) report}
#'   \item{value}{Earnings before interest and taxes}
#'   \item{type}{Actual or Forecast group}
#' }
"fc_baseline"

#' Univaritate time series with actual datapoints and ARIMA forecast (data.table
#' object)
#'
#' @docType data
#' @usage data(fc_arima)
#' @keywords datasets
#'
#' @format A data.table with 58 rows and 5 variables:
#' \describe{
#'   \item{index}{Publishing date of (quarterly) report}
#'   \item{value}{Earnings before interest and taxes}
#'   \item{key}{"actual" and "predict"}
#'   \item{lo95}{95% confidence interval lower barrier}
#'   \item{hi95}{95% confidence interval upper barrier}
#' }
"fc_arima"
