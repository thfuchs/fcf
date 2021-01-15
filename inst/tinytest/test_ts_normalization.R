DT_apple <- tsRNN::DT_apple
DF_apple <- as.data.frame(DT_apple)

### Check successful -----------------------------------------------------------

### data
# a. data.frame
DF_norm <- ts_normalization(DF_apple, length_val = 10, length_test = 10)
expect_equivalent(
  DF_norm[c(1, 10, 100)],
  structure(list(
    index = structure(c(654480000, 725760000, 1435622400), class = c("POSIXct", "POSIXt")),
    value = c(-0.529175083, -0.510408113, 2.08036117)
  ), row.names = c(NA, -3L), class = c("data.table", "data.frame"))
)
list_norm <- ts_normalization(DF_apple, 10, 10, metrics = TRUE)
expect_identical(DF_norm, list_norm$data)
expect_equivalent(list_norm$metrics, list(center = 2967.84328, scale = 5342.89759))

# data.table
DT_norm <- ts_normalization(tsRNN::DT_apple, 10, 10, metrics = FALSE)
expect_equivalent(DF_norm, as.data.frame(DT_norm))
list_norm_DT <- ts_normalization(tsRNN::DT_apple, 10, 10, metrics = TRUE)
expect_identical(DT_norm, list_norm_DT$data)
expect_identical(list_norm, list_norm_DT)

### "value_col" and "joined" for lagged data.table
DT_apple_lag <- copy(DT_apple)[, value_lag1 := shift(value, type = "lag", n = 1)]

# a. joined = TRUE
list_norm_lag_01 <- ts_normalization(
  DT_apple_lag, 10, 10, metrics = TRUE, value_col = "value", joined = TRUE)
expect_identical(list_norm$metrics, list_norm_lag_01$metrics)

# b. joined = FALSE
list_norm_lag_02 <- ts_normalization(
  DT_apple_lag, 10, 10, metrics = TRUE, value_col = "value", joined = FALSE)
expect_identical(
  purrr::map(list_norm$metrics, "value"),
  purrr::map(list_norm_lag_02$metrics, "value")
)
expect_identical(
  purrr::map(list_norm_lag_02$metrics, ~ round(.x[["value_lag1"]], 2)),
  list(center = 2855.57, scale = 5250.18)
)
lag_01_vs_lag_02 <- list_norm_lag_01$data$value_lag1 != list_norm_lag_02$data$value_lag1
expect_true(all(lag_01_vs_lag_02 %in% c(NA, TRUE)))

# c. multiple "value_col"
DT_value_col <- data.table::copy(DT_apple_lag)[, other := value]
expect_identical(
  data.table::copy(list_norm_lag_01$data)[, other := value],
  ts_normalization(DT_value_col, 10, 10, value_col = c("value", "other"), joined = TRUE)
)
expect_identical(
  data.table::copy(list_norm_lag_02$data)[, other := value],
  ts_normalization(DT_value_col, 10, 10, value_col = c("value", "other"), joined = FALSE)
)

### "length_val" and "length_test"

# numeric and integer with same result
expect_identical(ts_normalization(DT_apple, 10, 10, metrics = TRUE), list_norm)
expect_identical(ts_normalization(DT_apple, 10L, 10L, metrics = TRUE), list_norm)

# Normalize entire data set and expect mean and sd of DT_apple in metrics
DT_norm_all <- ts_normalization(DT_apple, 0, 0, metrics = TRUE)
expect_equivalent(DT_norm_all$metrics$center, mean(DT_apple$value))
expect_equivalent(DT_norm_all$metrics$scale, sd(DT_apple$value))

### Error ----------------------------------------------------------------------
expect_error(
  ts_normalization(data),
  class = "ts_normalization_data_error"
)
expect_error(
  ts_normalization(DT_apple, NULL),
  class = "ts_normalization_length_val_error"
)
expect_error(
  ts_normalization(DT_apple, 0, NULL),
  class = "ts_normalization_length_test_error"
)
expect_error(
  ts_normalization(DT_apple, 6, 6, value_col = 2),
  class = "ts_normalization_value_col_error"
)
expect_error(
  ts_normalization(DT_apple, 6, 6, value_col = "valeu"),
  class = "ts_normalization_value_col_error"
)
expect_error(
  ts_normalization(DT_apple, 6, 6, value_col = "index"),
  class = "ts_normalization_value_col_error"
)
expect_error(
  ts_normalization(DT_apple, 6, 6, value_col = c("value", "index")),
  class = "ts_normalization_value_col_error"
)
expect_error(
  ts_normalization(DT_apple, 6, 6, metrics = "No"),
  class = "ts_normalization_metrics_error"
)

# specific "value_col" errors
DT_value_col_fail <- data.table::copy(DT_apple_lag)[, other_val := value]
expect_error(
  ts_normalization(DT_value_col_fail, 10, 10, value_col = c("value", "other")),
  class = "ts_normalization_value_col_error",
  pattern = "If `joined = TRUE`, \"value_col\" must match column in \"data\"\\."
)
expect_error(
  ts_normalization(DT_apple, 10, 10, value_col = c("index", "value")),
  class = "ts_normalization_value_col_error",
  pattern = "Column \"index\" matched by pattern of \"value_col\" in \"data\" must be numeric\\."
)

# specific: "length_val" and "length_test" at limit of nrow(data)
expect_equivalent(
  ts_normalization(DT_apple,length_val =  98, length_test = 20, metrics = TRUE)$metrics,
  list(center = c(value = 184.4535), scale = c(value = 62.1384226103302))
)
expect_error(
  ts_normalization(DT_apple,length_val =  99, length_test = 20),
  class = "ts_normalization_data_error",
  pattern = "Number of rows of \"data\" must be higher then sum of \"length_val\" and \"length_test\"\\."
)
expect_error(
  ts_normalization(DT_apple,length_val =  100, length_test = 20),
  class = "ts_normalization_data_error",
  pattern = "Number of rows of \"data\" must be higher then sum of \"length_val\" and \"length_test\"\\."
)
expect_error(
  ts_normalization(DT_apple,length_val =  101, length_test = 20),
  class = "ts_normalization_data_error",
  pattern = "Number of rows of \"data\" must be higher then sum of \"length_val\" and \"length_test\"\\."
)
