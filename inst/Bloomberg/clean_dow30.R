# Data investigations - Save data to dow30_clean once all missing values and
# time series leaps are removed

### Overview -------------------------------------------------------------------
data <- tsRNN::dow30

# summarytools::descr(
#   data,
#   stats = c("mean", "sd", "min", "med", "max", "n.valid", "pct.valid"),
#   transpose = TRUE,
#   headings = TRUE
# ) %>% summarytools::tb()

min_date <- min(dow30$date, na.rm = TRUE)
max_date <- max(dow30$date, na.rm = TRUE)
n <- round(as.numeric(difftime(max_date, min_date, units = "weeks")/52.25) * 4) + 1


### Cleaning missing values ----------------------------------------------------

# How many NA's do we have per column per ticker?
data[, .(
  n_net_income = sum(complete.cases(net_income)),
  n_revenue = sum(complete.cases(revenue)),
  n_ebit = sum(complete.cases(ebit)),
  n_ebitda = sum(complete.cases(ebitda))
), by = "ticker"]

# Filter all ticker containing at least 1 NA for NI, Rev, EBIT or EBITDA
dow30_na <- data[
  , .SD[sum(complete.cases(.SD)) == n],
  by = ticker,
  .SDcols = c("date", "net_income", "ebit", "ebitda", "revenue")
]

# Still any missing value?
dow30_na[!complete.cases(ebit, ebitda, revenue, net_income), .N, by = "ticker"]


### Detect and remove outlier --------------------------------------------------

dow30_outlier <- tsEDA::detect_outlier(dow30_na, value_col = "ebit")
replacement_values <- dow30_outlier[Outlier == "replacement", ebit]
dow30_outlier[Outlier == "yes", ebit := replacement_values]
dow30_clean <- dow30_outlier[Outlier != "replacement", .SD, .SDcols = c("ticker", "date", "ebit")]
data.table::setnames(dow30_clean, c("date", "ebit"), c("index", "value"))

### Save cleaned data ----------------------------------------------------------
save(dow30_clean, file = "data/dow30_clean.rda")
