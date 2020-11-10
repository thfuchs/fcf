# Data investigations - Save data to dow30_clean once all missing values and
# time series leaps are removed

data <- fcf::dow30

# summarytools::descr(
#   data,
#   stats = c("mean", "sd", "min", "med", "max", "n.valid", "pct.valid"),
#   transpose = TRUE,
#   headings = TRUE
# ) %>% summarytools::tb()

min_date <- min(dow30$date, na.rm = TRUE)
max_date <- max(dow30$date, na.rm = TRUE)
n <- round(as.numeric(difftime(max_date, min_date, units = "weeks")/52.25) * 4) + 1

# How many NA's do we have per column per ticker?
data[, .(
  n_net_income = sum(complete.cases(net_income)),
  n_revenue = sum(complete.cases(revenue)),
  n_ebit = sum(complete.cases(ebit)),
  n_ebitda = sum(complete.cases(ebitda))
), by = "ticker"]

# Filter all ticker containing at least 1 NA for NI, Rev, EBIT or EBITDA
dow30_clean <- data[
  , .SD[sum(complete.cases(.SD)) == n],
  by = ticker,
  .SDcols = c("date", "net_income", "ebit", "ebitda", "revenue")
]

# Still any missing value?
dow30_clean[!complete.cases(ebit, ebitda, revenue, net_income), .N, by = "ticker"]

# save(dow30_clean, file = "data/dow30_clean.rda")
