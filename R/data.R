#' US index Dow Jones 30 quarterly cashflow data from 1990 to 2020.
#'
#' @docType data
#' @usage data(dow30)
#' @keywords datasets
#'
#' @format A data.table with 3600 rows and 7 variables:
#' \describe{
#'   \item{company}{Dow Jones listed companies in (August 2020)}
#'   \item{date}{Publishing date of (quarterly) report in POSIX format.
#'     Month and year of interest}
#'   \item{cap}{Bloomberg: CF_CAP_EXPEND_PRPTY_ADD}
#'   \item{cff}{Financing Cash Flow - Bloomberg: CF_CASH_FROM_FNC_ACT}
#'   \item{cfi}{Investing Cash Flow - Bloomberg: CF_CASH_FROM_INV_ACT}
#'   \item{cfo}{Operating Cash Flow - Bloomberg: CF_CASH_FROM_OPER}
#'   \item{fcf}{Free Cash Flow - Bloomberg: CF_FREE_CASH_FLOW}
#' }
"dow30"
