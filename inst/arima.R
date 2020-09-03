apple <- fcf::apple_fcf
train <- subset(apple, end = length(apple) - 8)

library(forecast)
library(ggplot2)

# 1. auto.arima
arima_fcf <- auto.arima(train, stepwise = FALSE, parallel = TRUE, num.cores = NULL)
autoplot(forecast(arima_fcf))
summary(arima_fcf)
checkresiduals(arima_fcf)
accuracy(forecast(arima_fcf), apple)

# 2. CV and auto.arima

# https://www.tidymodels.org/learn/models/time-series/
