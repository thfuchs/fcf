### Baseline Models ------------------------------------------------------------

data <- fcf::DT_unh

cv_setting <- list(
  periods_train = 40,
  periods_val = 8,
  periods_test = 8,
  skip_span = 7
)

forecast <- predict_baselines(data, cv_setting, normalize = FALSE)

### test plot
library(ggplot2)

# Plot RMSE for all splits
purrr::map_df(forecast, "accuracy", .id = "split") %>%
  ggplot(aes(x = index, y = RMSE, group = split)) + geom_line()

# Plot RMSE mean
rmse <- purrr::map_df(forecast, "accuracy")[, .(mean = mean(RMSE)), by = index]

rmse %>% ggplot(aes(x = index, y = mean)) + ggplot2::geom_col() +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )
