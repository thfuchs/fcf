library(ggplot2)
library(forecast)

data <- fcf::ts_unh
train <- subset(data, end = length(data) - 8)

### Plotting -------------------------------------------------------------------

# all variables
# autoplot(myts, facets = TRUE)
# autoplot(diff(myts), facets = TRUE)
# ggAcf(myts)

autoplot(data)
autoplot(diff(data))
ggseasonplot(data)
ggsubseriesplot(data)
gglagplot(data)
ggAcf(data)

# Window
# data_win <- window(data, 2011)
# autoplot(data_win)
# autoplot(diff(data_win))
# ggseasonplot(data_win)
# ggsubseriesplot(data_win)


### Autocorrelation ------------------------------------------------------------

ggAcf(data)
ggAcf(diff(data))

# Ljung-Box test
Box.test(data, lag = 10, type = "Ljung")
Box.test(diff(data), lag = 10, type = "Ljung")


### Simple Forecasting ---------------------------------------------------------

# 1. Naive forecast
fc_naive_fcf <- naive(train, h = 8)
autoplot(fc_naive_fcf)
autoplot(fc_naive_fcf) + autolayer(fitted(fc_naive_fcf))
summary(fc_naive_fcf)
checkresiduals(fc_naive_fcf)
accuracy(fc_naive_fcf, data)

# 2. Seasonal naive forecast
fc_snaive_fcf <- snaive(train, h = 8)
autoplot(fc_snaive_fcf)
autoplot(fc_snaive_fcf) + autolayer(fitted(fc_snaive_fcf))
summary(fc_snaive_fcf)
checkresiduals(fc_snaive_fcf)
accuracy(fc_snaive_fcf, data)

# 3. Mean Forecast
fc_mean_fcf <- meanf(train, h = 8)
autoplot(fc_mean_fcf)
autoplot(fc_mean_fcf) + autolayer(fitted(fc_mean_fcf))
summary(fc_mean_fcf)
checkresiduals(fc_mean_fcf)
accuracy(fc_mean_fcf, data)

# 4. Simple exponential smoothing
fc_ses_fcf <- ses(train, h = 8)
autoplot(fc_ses_fcf)
autoplot(fc_ses_fcf) + autolayer(fitted(fc_ses_fcf))
summary(fc_ses_fcf)
checkresiduals(fc_ses_fcf)
accuracy(fc_ses_fcf, data)

# 5. Exponential smoothing with trend: Holt's trend
fc_holt_fcf <- holt(train, h = 8)
autoplot(fc_holt_fcf)
autoplot(fc_holt_fcf) + autolayer(fitted(fc_holt_fcf))
summary(fc_holt_fcf)
checkresiduals(fc_holt_fcf)
accuracy(fc_holt_fcf, data)

# 6. Holt-Winters for time series with damped trend and additive seasonality
fc_hw_fcf <- hw(train, damped = TRUE, seasonal = "additive", h = 8)
autoplot(fc_hw_fcf)
autoplot(fc_hw_fcf) + autolayer(fitted(fc_hw_fcf))
summary(fc_hw_fcf)
checkresiduals(fc_hw_fcf)
accuracy(fc_hw_fcf, data)

# 7. ETS
ets_fcf <- ets(train)
autoplot(forecast(ets_fcf))
summary(ets_fcf)
checkresiduals(ets_fcf)
accuracy(ets_fcf, data)

# Summary:
forecast_baseline(data = train, test_size = 8, acc_measure = c("RMSE", "MAE"))


# Cross Validation (Forecast evaluation on a rolling origin) for best
# simple approach (seasonal naive forecast)
e <- tsCV(data, forecastfunction = snaive, h = 8)
mse <- colMeans(e^2, na.rm = TRUE)

# Plot the MSE values against the forecast horizon
data.frame(h = 1:8, MSE = mse) %>%
  ggplot(aes(x = 1:8, y = MSE)) + geom_point()
