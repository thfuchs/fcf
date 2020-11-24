### Checks ---------------------------------------------------------------------

expect_true(inherits(plot_prediction(fcf::fc_arima), "ggplot"))
expect_true(inherits(plot_prediction(as.data.frame(fcf::fc_arima)), "ggplot"))
expect_true(inherits(plot_prediction(fcf::fc_arima, col_date = "index"), "ggplot"))
expect_true(inherits(plot_prediction(fcf::fc_arima, col_value = "value"), "ggplot"))
expect_true(inherits(plot_prediction(fcf::fc_arima, col_group = "key"), "ggplot"))
expect_true(inherits(plot_prediction(fcf::fc_arima, title = "Title"), "ggplot"))
expect_true(inherits(plot_prediction(fcf::fc_arima, size = 5), "ggplot"))
expect_true(inherits(plot_prediction(fcf::fc_arima, alpha = 0.3), "ggplot"))
expect_true(inherits(plot_prediction(fcf::fc_arima, legend = NULL), "ggplot"))
expect_true(inherits(plot_prediction(fcf::fc_arima, legend = "left"), "ggplot"))
expect_true(inherits(plot_prediction(fcf::fc_arima, scale = NULL), "ggplot"))
expect_true(inherits(plot_prediction(
  fcf::fc_arima, scale = as.Date(c("1993-01-01", "2010-01-01"))), "ggplot"))
expect_true(inherits(plot_prediction(fcf::fc_arima, PI = TRUE), "ggplot"))
expect_true(inherits(plot_prediction(fcf::fc_arima, PI = FALSE), "ggplot"))
expect_true(inherits(plot_prediction(
  fcf::fc_arima, PI = TRUE, col_pi_high = "hi95", col_pi_low = "lo95"), "ggplot"))
expect_true(inherits(plot_prediction(
  fcf::fc_arima, PI = FALSE, col_pi_high = "hi95", col_pi_low = "lo95"), "ggplot"))

### Error ----------------------------------------------------------------------

expect_error(
  plot_prediction(data),
  class = "plot_prediction_data_error",
  pattern = "^`data` must be data\\.frame, not of class \"function\"\\.$"
)

expect_error(
  plot_prediction(fcf::fc_arima, col_date = NULL),
  class = "plot_prediction_col_date_error",
  pattern = "^`col_date` must be character, not of class \"NULL\"\\.$"
)
expect_error(
  plot_prediction(fcf::fc_arima, col_date = "value"),
  class = "plot_prediction_col_date_error",
  pattern = "^Variable specified by `col_date` must be class \"Date\"\\.$"
)
expect_error(
  plot_prediction(
    data.table::copy(fcf::fc_arima)[, index := as.POSIXct(index)],
    col_date = "index"),
  class = "plot_prediction_col_date_error",
  pattern = "^Variable specified by `col_date` must be class \"Date\"\\.$"
)

expect_error(
  plot_prediction(fcf::fc_arima, col_value = TRUE),
  class = "plot_prediction_col_value_error",
  pattern = "^`col_value` must be character, not of class \"logical\"\\.$"
)
expect_error(
  plot_prediction(fcf::fc_arima, col_value = "valuee"),
  class = "plot_prediction_col_value_error",
  pattern = "^Variable specified by `col_value` must be class \"numeric\"\\.$"
)
expect_error(
  plot_prediction(
    data.table::copy(fcf::fc_arima)[, value := as.character(value)],
    col_value = "value"),
  class = "plot_prediction_col_value_error",
  pattern = "^Variable specified by `col_value` must be class \"numeric\"\\.$"
)

expect_error(
  plot_prediction(fcf::fc_arima, col_group = data),
  class = "plot_prediction_col_group_error",
  pattern = "^`col_group` must be character, not of class \"function\"\\.$"
)
expect_error(
  plot_prediction(fcf::fc_arima, col_group = "type"),
  class = "plot_prediction_col_group_error",
  pattern = "^Variable specified by `col_group` must be class \"character\"\\.$"
)
expect_error(
  plot_prediction(
    data.table::copy(fcf::fc_arima)[, key := as.factor(key)], col_group = "key"),
  class = "plot_prediction_col_group_error",
  pattern = "^Variable specified by `col_group` must be class \"character\"\\.$"
)

expect_error(
  plot_prediction(fcf::fc_arima, size = TRUE),
  class = "plot_prediction_size_error",
  pattern = "^`size` must be numeric, not of class \"logical\"\\.$"
)

expect_error(
  plot_prediction(fcf::fc_arima, alpha = "5"),
  class = "plot_prediction_alpha_error",
  pattern = "^`alpha` must be numeric, not of class \"character\"\\.$"
)

expect_error(
  plot_prediction(fcf::fc_arima, legend = 12),
  class = "plot_prediction_legend_error",
  pattern = "^`legend` must be character, not of class \"numeric\"\\.$"
)

expect_error(
  plot_prediction(fcf::fc_arima, scale = "2020-01-01"),
  class = "plot_prediction_scale_error",
  pattern = "^`scale` must be Date, not of class \"character\"\\.$"
)

expect_error(
  plot_prediction(fcf::fc_arima, PI = NULL),
  class = "plot_prediction_PI_error",
  pattern = "^`PI` must be logical, not of class \"NULL\"\\.$"
)

expect_error(
  plot_prediction(fcf::fc_arima, PI = TRUE, col_pi_high = TRUE),
  class = "plot_prediction_col_pi_high_error",
  pattern = "^`col_pi_high` must be character, not of class \"logical\"\\.$"
)
expect_error(
  plot_prediction(fcf::fc_arima, PI = TRUE, col_pi_high = "type", col_pi_low = "lo95"),
  class = "plot_prediction_col_pi_high_error",
  pattern = "^Variable specified by `col_pi_high` must be class \"numeric\"\\.$"
)
expect_error(
  plot_prediction(
    data.table::copy(fcf::fc_arima)[, hi95 := as.character(hi95)], PI = TRUE),
  class = "plot_prediction_col_pi_high_error",
  pattern = "^Variable specified by `col_pi_high` must be class \"numeric\"\\.$"
)

expect_error(
  plot_prediction(fcf::fc_arima, PI = TRUE, col_pi_low = data),
  class = "plot_prediction_col_pi_low_error",
  pattern = "^`col_pi_low` must be character, not of class \"function\"\\.$"
)
expect_error(
  plot_prediction(fcf::fc_arima, PI = TRUE, col_pi_high = "hi95", col_pi_low = "key"),
  class = "plot_prediction_col_pi_low_error",
  pattern = "^Variable specified by `col_pi_low` must be class \"numeric\"\\.$"
)
expect_error(
  plot_prediction(
    data.table::copy(fcf::fc_arima)[, lo95 := as.character(lo95)], PI = TRUE),
  class = "plot_prediction_col_pi_low_error",
  pattern = "^Variable specified by `col_pi_low` must be class \"numeric\"\\.$"
)
