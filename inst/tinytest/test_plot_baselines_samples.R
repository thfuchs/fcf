test_splits <- list(fcf::fc_baseline, fcf::fc_baseline, fcf::fc_baseline)

### Check ----------------------------------------------------------------------

expect_true(rlang::inherits_all(
  plot_baselines_samples(splits = test_splits),
  c("patchwork", "ggplot")
))

expect_true(rlang::inherits_all(
  plot_baselines_samples(splits = test_splits, title = "Title"),
  c("patchwork", "ggplot")
))
expect_true(rlang::inherits_all(
  plot_baselines_samples(splits = test_splits, title = NULL),
  c("patchwork", "ggplot")
))

expect_true(rlang::inherits_all(
  plot_baselines_samples(
    splits = test_splits,
    colors = c("Actual" = "black", "Snaive" = "blue", "Holt" = "green")),
  c("patchwork", "ggplot")
))
expect_true(rlang::inherits_all(
  plot_baselines_samples(splits = test_splits, colors = NULL),
  c("patchwork", "ggplot")
))

expect_true(rlang::inherits_all(
  plot_baselines_samples(splits = test_splits, date_type = "datetime"),
  c("patchwork", "ggplot")
))
expect_true(rlang::inherits_all(
  plot_baselines_samples(splits = test_splits, date_type = "Date"),
  c("patchwork", "ggplot")
))
expect_true(rlang::inherits_all(
  plot_baselines_samples(splits = test_splits, date_type = "character"),
  c("patchwork", "ggplot")
))

expect_true(rlang::inherits_all(
  plot_baselines_samples(splits = test_splits, ncol = 1L),
  c("patchwork", "ggplot")
))

expect_true(rlang::inherits_all(
  plot_baselines_samples(
    splits = test_splits, scale = as.Date(c("1990-01-01", "2010-12-31"))),
  c("patchwork", "ggplot")
))
expect_true(rlang::inherits_all(
  plot_baselines_samples(splits = test_splits, scale = NULL),
  c("patchwork", "ggplot")
))

### Error ----------------------------------------------------------------------

expect_error(
  plot_baselines_samples(splits = fcf::fc_baseline),
  class = "plot_baselines_samples_splits_error",
  pattern = "`splits` must be list, not of class \"data.table / data.frame\"\\.$"
)

expect_error(
  plot_baselines_samples(splits = test_splits, colors = 12),
  class = "plot_baselines_samples_colors_error",
  pattern = "`colors` must be character, not of class \"numeric\"\\.$"
)
# expect_error(plot_baselines_samples(splits = test_splits, colors = c("Hallo")))

expect_error(
  plot_baselines_samples(splits = test_splits, date_type = NULL),
  class = "plot_baselines_samples_date_type_error",
  pattern = "`date_type` must be character, not of class \"NULL\"\\.$"
)
expect_error(
  plot_baselines_samples(splits = test_splits, date_type = "hello"),
  class = "rlang_error",
  pattern = "^`date_type` must be one of \"Date\", \"datetime\", or \"character\".$"
)

expect_error(
  plot_baselines_samples(splits = test_splits, ncol = "ncol"),
  class = "plot_baselines_samples_ncol_error",
  pattern = "`ncol` must be integer, not of class \"character\"\\.$"
)
expect_error(
  plot_baselines_samples(splits = test_splits, ncol = NULL),
  class = "plot_baselines_samples_ncol_error",
  pattern = "`ncol` must be integer, not of class \"NULL\"\\.$"
)

expect_error(
  plot_baselines_samples(splits = test_splits, scale = "1990-01-01"),
  class = "plot_baselines_samples_scale_error",
  pattern = "`scale` must be Date, not of class \"character\"\\.$"
)
