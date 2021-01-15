DT_apple <- tsRNN::DT_apple
DF_apple <- as.data.frame(DT_apple)

### Check successful -----------------------------------------------------------

### data
# 1 Lag
DT_apple_lag1 <- data.table::copy(DT_apple)[
  , value_lag1 := data.table::shift(value, type = "lag", n = 1)]
DT_apple_lag1_NA <- DT_apple_lag1[!is.na(get(paste0("value_lag1")))]

result_lag1 <- ts_nn_preparation(
  DT_apple_lag1_NA[1:5], tsteps = 1L, length_val = 2L, length_test = 2L
)
expect_identical(
  result_lag1,
  list(
    x = list(
      train = structure(140.515, .Dim = c(1L, 1L, 1L)),
      val = structure(c(228.392, 186.545), .Dim = c(2L, 1L, 1L)),
      test = structure(c(135.202, 121.253), .Dim = c(2L, 1L, 1L
      ))
    ), y = list(
      train = structure(228.392, .Dim = c(1L, 1L)),
      val = structure(c(186.545, 135.202), .Dim = 2:1),
      test = structure(c(121.253, 262.654), .Dim = 2:1)
    )
  )
)

# data.frame
expect_identical(
  ts_nn_preparation(
    as.data.frame(DT_apple_lag1_NA[1:5]),
    tsteps = 1L,
    length_val = 2L,
    length_test = 2L
  ),
  result_lag1
)

# 2 Lags
DT_apple_lag2 <- data.table::copy(DT_apple_lag1)[
  , value_lag2 := data.table::shift(value, type = "lag", n = 2)]
DT_apple_lag2_NA <- DT_apple_lag2[!is.na(get(paste0("value_lag2")))]

result_lag2 <- ts_nn_preparation(
  DT_apple_lag2_NA[2:6], tsteps = 2L, length_val = 2L, length_test = 2L
)
expect_identical(
  result_lag2,
  list(
    x = list(
      train = structure(c(186.545, 228.392), .Dim = c(1L, 2L, 1L)),
      val = structure(c(135.202, 121.253, 186.545, 135.202), .Dim = c(2L, 2L, 1L)),
      test = structure(c(262.654, 210.405, 121.253, 262.654), .Dim = c(2L, 2L, 1L))
    ), y = list(
      train = structure(135.202, .Dim = c(1L, 1L)),
      val = structure(c(121.253, 262.654), .Dim = 2:1),
      test = structure(c(210.405, 201.061), .Dim = 2:1)
    )
  )
)

# "length_val" and "length_test": integer and numeric work
expect_identical(
  result_lag1,
  ts_nn_preparation(
    DT_apple_lag1_NA[1:5], tsteps = 1, length_val = 2, length_test = 2
  )
)

### Error ----------------------------------------------------------------------
expect_error(
  ts_nn_preparation(data),
  class = "ts_nn_preparation_data_error"
)
expect_error(
  ts_nn_preparation(DT_apple_lag1, tsteps = TRUE),
  class = "ts_nn_preparation_tsteps_error"
)
expect_error(
  ts_nn_preparation(DT_apple_lag1, tsteps = 1, length_val = NULL),
  class = "ts_nn_preparation_length_val_error"
)
expect_error(
  ts_nn_preparation(DT_apple_lag1, tsteps = 1, length_test = "NULL"),
  class = "ts_nn_preparation_length_test_error"
)

# "data" specific: Incorrect data shape
expect_error(
  ts_nn_preparation(DT_apple, tsteps = 1),
  class = "ts_nn_preparation_data_error",
  pattern = "`data` must be a data.frame with at least `tsteps` lagged columns\\."
)
expect_error(
  ts_nn_preparation(DT_apple_lag1, tsteps = 2),
  class = "ts_nn_preparation_data_error",
  pattern = "`data` must be a data.frame with at least `tsteps` lagged columns\\."
)

# "tsteps" specific: non-negative, length 1
expect_error(
  ts_nn_preparation(data = DT_apple, tsteps = 0),
  class = "ts_nn_preparation_tsteps_error",
  pattern = "`tsteps` must be positive\\."
)
expect_error(
  ts_nn_preparation(DT_apple_lag1, tsteps = -1),
  class = "ts_nn_preparation_tsteps_error",
  pattern = "`tsteps` must be positive\\."
)
expect_error(
  ts_nn_preparation(DT_apple_lag1, tsteps = c(1, 2)),
  class = "ts_nn_preparation_tsteps_error",
  pattern = paste("`tsteps` must be numeric\\(1\\) or integer\\(1\\),",
                  "not of class \"numeric\\(2\\)\"\\.")
)

# "length_val" specific: non-negative, length 1
expect_error(
  ts_nn_preparation(DT_apple_lag1, tsteps = 1, length_val = c(6, 8)),
  class = "ts_nn_preparation_length_val_error",
  pattern = paste("`length_val` must be numeric\\(1\\) or integer\\(1\\),",
                  "not of class \"numeric\\(2\\)\"\\.")
)
expect_error(
  ts_nn_preparation(DT_apple_lag1, tsteps = 1, length_val = -1),
  class = "ts_nn_preparation_length_val_error",
  pattern = "`length_val` must be non-negative\\."
)

# "length_test" specific: non-negative, length 1
expect_error(
  ts_nn_preparation(DT_apple_lag1, tsteps = 1, length_test = c(6, 8)),
  class = "ts_nn_preparation_length_test_error",
  pattern = paste("`length_test` must be numeric\\(1\\) or integer\\(1\\),",
                  "not of class \"numeric\\(2\\)\"\\.")
)
expect_error(
  ts_nn_preparation(DT_apple_lag1, tsteps = 1, length_test = -2),
  class = "ts_nn_preparation_length_test_error",
  pattern = "`length_test` must be non-negative\\."
)

# specific: "length_val" and "length_test" at limit of nrow(data)
result_limit <- ts_nn_preparation(
  data = DT_apple_lag1, tsteps = 1, length_val = 98, length_test = 20
)
expect_identical(dim(result_limit$x$train), c(2L, 1L, 1L))
expect_identical(dim(result_limit$x$val), c(98L, 1L, 1L))
expect_identical(dim(result_limit$x$test), c(20L, 1L, 1L))
expect_identical(dim(result_limit$y$train), c(2L, 1L))
expect_identical(dim(result_limit$y$val), c(98L, 1L))
expect_identical(dim(result_limit$y$test), c(20L, 1L))

result_limit_b <- ts_nn_preparation(
  data = DT_apple_lag1, tsteps = 1, length_val = 99, length_test = 20
)
expect_identical(dim(result_limit_b$x$train), c(1L, 1L, 1L))
expect_identical(result_limit_b$x$train, structure(NA_real_, .Dim = c(1L, 1L, 1L)))
expect_identical(dim(result_limit_b$y$train), c(1L, 1L))

expect_error(
  ts_nn_preparation(DT_apple_lag1, tsteps = 1, length_val = 100, length_test = 20),
  class = "ts_nn_preparation_data_error",
  pattern = paste("Number of rows of \"data\" must be higher then sum of",
                  "\"length_val\" and \"length_test\".")
)
expect_error(
  ts_nn_preparation(DT_apple_lag1, tsteps = 1, length_val = 100, length_test = 21),
  class = "ts_nn_preparation_data_error",
  pattern = paste("Number of rows of \"data\" must be higher then sum of",
                  "\"length_val\" and \"length_test\".")
)
