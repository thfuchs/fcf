# Save ticker UNH to DT_unh (data.table object containing date (index) and
# value (ebit)) and ts_unh (ts object - univariate time series)

DT_unh <- tsRNN::dow30_clean[
  ticker == "UNH" & date > as.POSIXct("1990-01-01"),
  .SD, .SDcols = c("date", "ebit")
]
data.table::setnames(DT_unh, c("date", "ebit"), c("index", "value"))
# save(DT_unh, file = "data/DT_unh.rda")

ts_unh <- ts(DT_unh[,value], frequency = 4, start = c(1990, 3))
# save(ts_unh, file = "data/ts_unh.rda")
