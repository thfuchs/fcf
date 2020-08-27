companies <- as.character(data.table::fread(
  file = "inst/data/cf_dow_30y.csv",
  na.strings = "#N/A N/A",
  header = FALSE,
  nrows = 1,
  drop = 1,
  sep = ";",
  dec = ",",
  stringsAsFactors = FALSE
))
companies <- unique(companies)
length(companies)

raw <- data.table::fread(
  file = "inst/data/cf_dow_30y.csv",
  na.strings = "#N/A N/A",
  skip = 1,
  colClasses=list(numeric=2:150),
  sep = ";",
  dec = ",",
  header = TRUE,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

raw[, Dates := as.POSIXct(as.Date(Dates, "%d.%m.%Y"), tz = "UTC")]

cf_types <- unique(names(raw[, -c(1)]))
data_per_cf_type <- sapply(cf_types, function(filter_obj) {
  type <- raw[, c(1, which(names(raw) == filter_obj)), with = FALSE]
  data.table::setnames(type, new = c("date", companies))
}, simplify = FALSE, USE.NAMES = TRUE)

data_per_company <- data.table::rbindlist(data_per_cf_type, idcol = "type")

data_long <- data.table::melt(
  data_per_company,
  id.vars = c("type", "date"),
  variable.name = "company"
)
dow30 <- data.table::dcast(data_long, company + date ~ type)

# Transformations
data.table::setnames(
  dow30,
  old = cf_types,
  new = c("fcf", "cfo", "cfi", "cff", "cap")
)

dow30[, ticker := gsub("(^\\w*).*", "\\1", company)]

# Checks
# dow30[, check := round(cfo + cap - fcf)]
# dow30[!is.na(check) & check != 0]
#
# dups <- duplicated(dow30, by = c("company", "cap"))
# dow30[!is.na(cap) & dups & company == "AAPL UW Equity"]

# dow30[, counter := data.table::rowid(data.table::rleid(cap))]
# dow30[!is.na(cap) & counter > 1]


# Save data
data.table::setcolorder(
  dow30,
  neworder = c("company", "ticker", "date", "fcf", "cfo", "cff", "cfi", "cap")
)
# save(dow30, file = "data/dow30.rda")
