dow30 <- fcf::dow30

# Data: AAPL with date and value (FCF) since 1995
# apple <- dow30[ticker == "AAPL" & date > as.POSIXct("1995-01-01"), .(date, fcf)]
apple <- dow30[
  ticker == "AAPL" & date > as.POSIXct("1995-01-01"),
  -c("company", "ticker")
]

apple[is.na(apple$fcf)]

myts <- ts(apple[, -1], start = c(1995, 1), frequency = 4)
myts_fcf <- ts(apple[, 2], start = c(1995, 1), frequency = 4)

### Plotting -------------------------------------------------------------------
library(ggplot2)
library(forecast)

# all variables
autoplot(myts, facets = TRUE)
autoplot(diff(myts), facets = TRUE)
ggAcf(myts)

# FCF only
autoplot(myts_fcf)
autoplot(diff(myts_fcf))
ggseasonplot(myts_fcf)
ggsubseriesplot(myts_fcf)
gglagplot(myts_fcf)
ggAcf(myts_fcf)

# FCF windows
myts_fcf_win <- window(myts_fcf, 2011)
autoplot(myts_fcf_win)
autoplot(diff(myts_fcf_win))
ggseasonplot(myts_fcf_win)
ggsubseriesplot(myts_fcf_win)


### Autocorrelation ------------------------------------------------------------

ggAcf(myts_fcf)
ggAcf(diff(myts_fcf))

# Ljung-Box test
Box.test(myts_fcf, lag = 10, type = "Ljung")
Box.test(diff(myts_fcf), lag = 10, type = "Ljung")


### Data Preparation -----------------------------------------------------------

train <- subset(myts_fcf, end = length(myts_fcf) - 8)


### Simple Forecasting ---------------------------------------------------------

# 1. Naive forecast
fc_naive_fcf <- naive(train, h = 8)
autoplot(fc_naive_fcf)
autoplot(fc_naive_fcf) + autolayer(fitted(fc_naive_fcf))
summary(fc_naive_fcf)
checkresiduals(fc_naive_fcf)
accuracy(fc_naive_fcf, myts_fcf)

# 2. Seasonal naive forecast
fc_snaive_fcf <- snaive(train, h = 8)
autoplot(fc_snaive_fcf)
autoplot(fc_snaive_fcf) + autolayer(fitted(fc_snaive_fcf))
summary(fc_snaive_fcf)
checkresiduals(fc_snaive_fcf)
accuracy(fc_snaive_fcf, myts_fcf)

# 3. Mean Forecast
fc_mean_fcf <- meanf(train, h = 8)
autoplot(fc_mean_fcf)
autoplot(fc_mean_fcf) + autolayer(fitted(fc_mean_fcf))
summary(fc_mean_fcf)
checkresiduals(fc_mean_fcf)
accuracy(fc_mean_fcf, myts_fcf)

# 4. Simple exponential smoothing
fc_ses_fcf <- ses(train, h = 8)
autoplot(fc_ses_fcf)
autoplot(fc_ses_fcf) + autolayer(fitted(fc_ses_fcf))
summary(fc_ses_fcf)
checkresiduals(fc_ses_fcf)
accuracy(fc_ses_fcf, myts_fcf)

# 5. Exponential smoothing with trend: Holt's trend
fc_holt_fcf <- holt(train, h = 8)
autoplot(fc_holt_fcf)
autoplot(fc_holt_fcf) + autolayer(fitted(fc_holt_fcf))
summary(fc_holt_fcf)
checkresiduals(fc_holt_fcf)
accuracy(fc_holt_fcf, myts_fcf)

# 6. Holt-Winters for time series with damped trend and additive seasonality
fc_hw_fcf <- hw(train, damped = TRUE, seasonal = "additive", h = 8)
autoplot(fc_hw_fcf)
autoplot(fc_hw_fcf) + autolayer(fitted(fc_hw_fcf))
summary(fc_hw_fcf)
checkresiduals(fc_hw_fcf)
accuracy(fc_hw_fcf, myts_fcf)

# 7. ETS
ets_fcf <- ets(train)
autoplot(forecast(ets_fcf))
summary(fc_hw_fcf)
checkresiduals(fc_hw_fcf)
accuracy(fc_hw_fcf, myts_fcf)

# Cross Validation (Forecast evaluation on a rolling origin) for best
# simple approach (seasonal naive forecast)
e <- tsCV(myts_fcf, forecastfunction = snaive, h = 8)
mse <- colMeans(e^2, na.rm = TRUE)

# Plot the MSE values against the forecast horizon
data.frame(h = 1:8, MSE = mse) %>%
  ggplot(aes(x = 1:8, y = MSE)) + geom_point()
