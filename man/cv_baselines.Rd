% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cv_baselines.R
\name{cv_baselines}
\alias{cv_baselines}
\title{Cross validated prediction and evaluation with baseline forecasts}
\usage{
cv_baselines(
  data,
  cv_setting,
  col_id = NULL,
  col_date = "index",
  col_value = "value",
  transform = NULL,
  frequency = 4,
  h = NULL
)
}
\arguments{
\item{data}{Univariate time series (data.frame)}

\item{cv_setting}{cross validation settings. Named list requiring \code{periods_train},
\code{periods_val} \code{periods_test} and \code{skip_span}. See section "Cross validation
settings" for details.}

\item{col_id}{Optional ID column in \code{data}, default to "ticker"}

\item{col_date}{Date column in \code{data}, default to "index"}

\item{col_value}{Value column in \code{data}, default to "value"}

\item{transform}{Transform data before estimation? One of NULL (default)
and "normalize"}

\item{frequency}{time series frequency, e.g. 4 for quarters and 12 for months}

\item{h}{NULL if forecast horizon equals cv_setting$n_test, else named list
of forecast horizons for accuracy measures}
}
\value{
list of \code{type} (model), \code{h} (forecast horizon, if specified),
\link{mape}, \link{smape}, \link{mase}, \link{smis} and \link{acd}
}
\description{
Get results for the following simple forecasting methods:
\itemize{
\item Naive forecast (\link[forecast]{naive})
\item Seasonal naive forecast
\item Mean forecast (\link[forecast]{meanf})
\item Simple exponential smoothing forecasts (\link[forecast]{ses})
\item Exponential smoothing with Holt's trend
}
}
\section{Cross validation settings}{

Using \link[rsample]{rolling_origin} to split the time series. Requiring:
\itemize{
\item \code{periods_train}: Length of training set per split
\item \code{periods_val}: Length of validation set per split
\item \code{periods_test}: Length of test/hold-out set per split
\item \code{skip_span}: Gaps between overlapping splits to reduce computational
intensity and recundancy between data splits.\cr \cr
}
Note: \code{periods_val} only relevant for deep learning models.
\link{cv_baselines} and \link{cv_arima} use sum of \code{periods_train} and
\code{periods_val} for training and only \code{periods_test} as hold-out test set (no
learning and feedback through validation by traditional statistical models)
}

\examples{
cv_setting <- list(
  periods_train = 90,
  periods_val = 10,
  periods_test = 10,
  skip_span = 5
)

fc_01 <- cv_baselines(
  data = tsRNN::DT_apple,
  cv_setting = cv_setting
)
fc_01

# Multiple forecast horizons
\dontrun{
fc_02 <- cv_baselines(
  data = tsRNN::DT_apple,
  cv_setting = cv_setting,
  h = list(short = 1:2, long = 3:6)
)
fc_02
}
}
