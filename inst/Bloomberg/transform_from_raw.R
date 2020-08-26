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
data <- data.table::dcast(data_long, company + date ~ type)

# Transformations
data.table::setnames(
  data,
  old = cf_types,
  new = c("fcf", "cfo", "cfi", "cff", "cap")
)

# Checks
# data[, check := round(cfo + cap - fcf)]
# data[!is.na(check) & check != 0]
#
# dups <- duplicated(data, by = c("company", "cap"))
# data[!is.na(cap) & dups & company == "AAPL UW Equity"]

# data[, counter := data.table::rowid(data.table::rleid(cap))]
# data[!is.na(cap) & counter > 1]


# Save data
# dow30 <- data
# save(dow30, file = "data/dow30.rda")
