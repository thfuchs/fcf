companies <- as.character(data.table::fread(
  file = "inst/data/cf_dow_30y.csv",
  na.strings = "#N/A N/A",
  header = FALSE,
  nrows = 1,
  drop = 1,
  stringsAsFactors = FALSE
))
companies <- unique(companies)
length(companies)

raw <- data.table::fread(
  file = "inst/data/cf_dow_30y.csv",
  na.strings = "#N/A N/A",
  skip = 1,
  header = TRUE,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

raw[, Dates := as.Date(Dates, "%d.%m.%Y")]

data_per_cf_type <- sapply(unique(names(raw[, -c(1)])), function(filter_obj) {
  type <- raw[, c(1, which(names(raw) == filter_obj)), with = FALSE]
  data.table::setnames(type, new = c("date", companies))
}, simplify = FALSE, USE.NAMES = TRUE)

# Per Company
apple <- data.table::data.table(
  date = data_per_cf_type$CF_FREE_CASH_FLOW$date,
  fcf = data_per_cf_type$CF_FREE_CASH_FLOW$`AAPL UW Equity`,
  cfo = data_per_cf_type$CF_CASH_FROM_OPER$`AAPL UW Equity`,
  cfi = data_per_cf_type$CF_CASH_FROM_INV_ACT$`AAPL UW Equity`,
  cff = data_per_cf_type$CF_CASH_FROM_FNC_ACT$`AAPL UW Equity`,
  cap = data_per_cf_type$CF_CAP_EXPEND_PRPTY_ADD$`AAPL UW Equity`,
  check.names = FALSE
)

# Transformations
apple[, cfo := as.numeric(cfo)]
apple[, check := cfo + cap]

# Checks
apple[fcf != check]
apple[, check := NULL]

apple[!complete.cases(apple)]
apple[is.na(fcf)]
