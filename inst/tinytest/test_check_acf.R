### Intro ----------------------------------------------------------------------
apple <- tsRNN::ts_apple

### Check successful -----------------------------------------------------------

expect_identical(
  check_acf(apple),
  list(
    p.values = structure(
      list(Ljung.Box = c(0, 0, 0), Box.Pierce = c(0, 0, 0)),
      class = "data.frame", row.names = c("lag_8", "lag_12", "lag_16")
    ),
    pass = TRUE
  )
)
expect_true(check_acf(apple, level = 0.01, lag_grid = 1:5)$pass)

### Error ----------------------------------------------------------------------

# Wrong Classes
expect_error(
  check_acf(TRUE),
  class = "check_acf_data_error",
  pattern = "^`data` must be a numeric vector or ts object, not of class \"logical\"\\.$"
)
expect_error(
  check_acf(apple, level = TRUE),
  class = "check_acf_level_error",
  pattern = "^`level` must be numeric or integer, not of class \"logical\"\\.$"
)
expect_error(
  check_acf(apple, lag_grid = TRUE),
  class = "check_acf_lag_grid_error",
  pattern = "^`lag_grid` must be numeric or integer, not of class \"logical\"\\.$"
)

# Wrong data types
expect_error(
  check_acf(apple, lag_grid = 0),
  class = "check_acf_lag_grid_error",
  pattern = "^`lag_grid` must be positive, not \"0\"\\.$"
)

expect_error(
  check_acf(apple, level = -1),
  class = "check_acf_level_error",
  pattern = "^`level` must be between 0 and 1, not \"-1\"\\.$"
)
