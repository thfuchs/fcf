### Check ----------------------------------------------------------------------

expect_true(inherits(plot_baselines(tsRNN::fc_baseline), "ggplot"))

expect_true(inherits(plot_baselines(tsRNN::fc_baseline, col_date = "index"), "ggplot"))

expect_true(inherits(plot_baselines(tsRNN::fc_baseline, col_value = "value"), "ggplot"))

expect_true(inherits(plot_baselines(tsRNN::fc_baseline, col_group = "type"), "ggplot"))

expect_true(inherits(plot_baselines(tsRNN::fc_baseline, title = "Title"), "ggplot"))
expect_true(inherits(plot_baselines(tsRNN::fc_baseline, title = 12), "ggplot"))

expect_true(inherits(plot_baselines(tsRNN::fc_baseline, size = 5), "ggplot"))

expect_true(inherits(plot_baselines(tsRNN::fc_baseline, alpha = 0.3), "ggplot"))
expect_true(inherits(plot_baselines(tsRNN::fc_baseline, alpha = 9), "ggplot"))

expect_true(inherits(plot_baselines(tsRNN::fc_baseline, colors = NULL), "ggplot"))
expect_true(inherits(plot_baselines(tsRNN::fc_baseline, colors = c(
  "Actual" = "black", "Snaive" = "blue", "Holt" = "green")), "ggplot"))
expect_true(inherits(plot_baselines(tsRNN::fc_baseline, colors = c(
  Actual = "green", Snaive = "gray", Holt = "orange")), "ggplot"))

expect_true(inherits(plot_baselines(tsRNN::fc_baseline, legend = "left"), "ggplot"))
expect_true(inherits(plot_baselines(tsRNN::fc_baseline, legend = NULL), "ggplot"))

expect_true(inherits(plot_baselines(tsRNN::fc_baseline, scale = NULL), "ggplot"))
expect_true(inherits(plot_baselines(
  tsRNN::fc_baseline, scale = as.Date(c("1990-01-01", "2010-12-31"))), "ggplot"))

### Error ----------------------------------------------------------------------

expect_error(
  plot_baselines(data),
  class = "plot_baselines_data_error",
  pattern = "`data` must be data\\.frame, not of class \"function\"\\.$"
)

expect_error(
  plot_baselines(tsRNN::fc_baseline, col_date = 12),
  class = "plot_baselines_col_date_error",
  pattern = "`col_date` must be character, not of class \"numeric\"\\.$"
)
expect_error(
  plot_baselines(tsRNN::fc_baseline, col_date = "not_index"),
  class = "plot_baselines_col_date_error",
  pattern = "Variable specified by `col_date` must be class \"Date\".$"
)

expect_error(
  plot_baselines(tsRNN::fc_baseline, col_value = FALSE),
  class = "plot_baselines_col_value_error",
  pattern = "`col_value` must be character, not of class \"logical\"\\.$"
)
expect_error(
  plot_baselines(tsRNN::fc_baseline, col_value = "index"),
  class = "plot_baselines_col_value_error",
  pattern = "Variable specified by `col_value` must be class \"numeric\".$"
)

expect_error(
  plot_baselines(tsRNN::fc_baseline, col_group = NULL),
  class = "plot_baselines_col_group_error",
  pattern = "`col_group` must be character, not of class \"NULL\"\\.$"
)
expect_error(
  plot_baselines(tsRNN::fc_baseline, col_group = "index"),
  class = "plot_baselines_col_group_error",
  pattern = "Variable specified by `col_group` must be class \"character\".$"
)

expect_error(
  plot_baselines(tsRNN::fc_baseline, size = TRUE),
  class = "plot_baselines_size_error",
  pattern = "`size` must be numeric, not of class \"logical\"\\.$"
)

expect_error(
  plot_baselines(tsRNN::fc_baseline, alpha = NULL),
  class = "plot_baselines_alpha_error",
  pattern = "`alpha` must be numeric, not of class \"NULL\"\\.$"
)

expect_error(
  plot_baselines(tsRNN::fc_baseline, colors = 12),
  class = "plot_baselines_colors_error",
  pattern = "`colors` must be character, not of class \"numeric\"\\.$"
)
# expect_error(plot_baselines(tsRNN::fc_baseline, colors = c("Hallo")))

expect_error(
  plot_baselines(tsRNN::fc_baseline, legend = 12),
  class = "plot_baselines_legend_error",
  pattern = "`legend` must be character, not of class \"numeric\"\\.$"
)

expect_error(
  plot_baselines(tsRNN::fc_baseline, scale = "2020-01-01"),
  class = "plot_baselines_scale_error",
  pattern = "`scale` must be Date, not of class \"character\"\\.$"
)
