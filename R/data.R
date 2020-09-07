#' US index Dow Jones 30 quarterly cashflow data from 1990 to 2020.
#'
#' @docType data
#' @usage data(dow30)
#' @keywords datasets
#'
#' @format A data.table with 3600 rows and 8 variables:
#' \describe{
#'   \item{company}{Dow Jones listed companies in (August 2020)}
#'   \item{ticker}{bbreviation of company title}
#'   \item{date}{Publishing date of (quarterly) report in POSIX format.
#'     Month and year of interest}
#'   \item{fcf}{Free Cash Flow - Bloomberg: CF_FREE_CASH_FLOW}
#'   \item{cfo}{Operating Cash Flow - Bloomberg: CF_CASH_FROM_OPER}
#'   \item{cfi}{Investing Cash Flow - Bloomberg: CF_CASH_FROM_INV_ACT}
#'   \item{cff}{Financing Cash Flow - Bloomberg: CF_CASH_FROM_FNC_ACT}
#'   \item{cap}{Bloomberg: CF_CAP_EXPEND_PRPTY_ADD}
#' }
"dow30"

#' Quarterly reported free cash flow from Apple as ts object from 1995 to 2020.
#'
#' @docType data
#' @usage data(ts_apple)
#' @keywords datasets
#'
#' @format A quarterly ts object from 1995 to 2020
"ts_apple"

#' Quarterly reported free cash flow from Apple as data.table object from 1995
#' to 2020.
#'
#' @docType data
#' @usage data(DT_apple)
#' @keywords datasets
#'
#' @format A quarterly data.table object from 1995 to 2020
"DT_apple"
