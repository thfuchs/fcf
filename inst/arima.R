library(forecast)
library(ggplot2)

apple <- fcf::ts_apple
train <- subset(apple, end = length(apple) - 8)

# 1. auto.arima
arima_fcf <- auto.arima(train, stepwise = FALSE, parallel = TRUE, num.cores = NULL)
autoplot(forecast(arima_fcf))
summary(arima_fcf)
checkresiduals(arima_fcf)
accuracy(forecast(arima_fcf), apple)

# 2. CV and auto.arima

# https://www.tidymodels.org/learn/models/time-series/
