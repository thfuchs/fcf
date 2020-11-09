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
  ggplot(aes(x = type, y = RMSE, group = split)) + geom_line()

# RMSE means
rmse <- purrr::map_df(forecast, "accuracy")[, .(mean = mean(RMSE)), by = type]
# latex_table(rmse)

rmse %>% ggplot(aes(x = type, y = mean)) + ggplot2::geom_col() +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

# Forecasts (Point Forecast and Prediction Interval)
# 1. Naive
plot_prediction_samples(
  splits = purrr::map(forecast, ~ .x[["forecast"]][is.na(type) | type == "Naive"]),
  title = "Naive Forecast",
  ncol = 3,
  scale = as.Date(c(min(data$index), max(data$index)))
)
# 2. Snaive
plot_prediction_samples(
  splits = purrr::map(forecast, ~ .x[["forecast"]][is.na(type) | type == "Snaive"]),
  title = "Seasonal Naive Forecast",
  ncol = 3,
  scale = as.Date(c(min(data$index), max(data$index)))
)
# 3. Mean
plot_prediction_samples(
  splits = purrr::map(forecast, ~ .x[["forecast"]][is.na(type) | type == "Mean"]),
  title = "Mean Forecast",
  ncol = 3,
  scale = as.Date(c(min(data$index), max(data$index)))
)
# 4. SES
plot_prediction_samples(
  splits = purrr::map(forecast, ~ .x[["forecast"]][is.na(type) | type == "SES"]),
  title = "SES Forecast",
  ncol = 3,
  scale = as.Date(c(min(data$index), max(data$index)))
)
# 4. Holt
plot_prediction_samples(
  splits = purrr::map(forecast, ~ .x[["forecast"]][is.na(type) | type == "Holt"]),
  title = "Holt's Trend Forecast",
  ncol = 3,
  scale = as.Date(c(min(data$index), max(data$index)))
)

# All together
x <- purrr::map(forecast, "forecast")[[1]][is.na(type), type := "Actual"]
x[, index := as.Date(index)]
plot_baselines(
  data = x,
  title = "title",
  colors = c(
    "Actual" = "black",
    "Naive" = "red",
    "Snaive" = "blue",
    "Mean" = "orange",
    "SES" = "purple",
    "Holt" = "green"
  )
)

plot_baselines_samples(
  splits = purrr::map(forecast, ~ .x[["forecast"]][is.na(type), type := "Actual"]),
  title = "Baseline Models",
  colors = c(
    "Actual" = "black",
    "Naive" = "red",
    "Snaive" = "blue",
    "Mean" = "orange",
    "SES" = "purple",
    "Holt" = "green"
  ),
  ncol = 3,
  scale = as.Date(c(min(data$index), max(data$index)))
)
