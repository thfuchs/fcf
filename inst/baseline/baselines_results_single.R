# Forecasts and Evaluations for Baseline Models for selected company (ticker UNH)

### Settings -------------------------------------------------------------------

data <- fcf::DT_unh

cv_setting <- list(
  periods_train = 40,
  periods_val = 8,
  periods_test = 8,
  skip_span = 7
)

### w/o transformation ---------------------------------------------------------

forecast <- predict_baselines(data, cv_setting)

# RMSE
rmse <- purrr::map_df(forecast, "accuracy")[, .(mean = mean(RMSE)), by = type]; rmse
rmse %>% ggplot(aes(x = type, y = mean)) + ggplot2::geom_col() +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

# Point Forecasts
fc <- purrr::map(forecast, ~ .x[["forecast"]][is.na(type), type := "Actual"])

plot_baselines_samples(
  splits = fc,
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

plot_prediction_samples(
  splits = purrr::map(forecast, ~ .x[["forecast"]][type == "Actual" | type == "Holt"]),
  title = "Holt's Trend Forecast including Prediction Interval",
  ncol = 3,
  scale = as.Date(c(min(data$index), max(data$index))),
  PI = TRUE
)


### NN transformation (normalization) ------------------------------------------

forecast <- predict_baselines(data, cv_setting, transform = "normalize")

# RMSE
rmse <- purrr::map_df(forecast, "accuracy")[, .(mean = mean(RMSE)), by = type]; rmse
rmse %>% ggplot(aes(x = type, y = mean)) + ggplot2::geom_col() +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

# Point Forecasts
fc <- purrr::map(forecast, ~ .x[["forecast"]][is.na(type), type := "Actual"])

plot_baselines_samples(
  splits = fc,
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

plot_prediction_samples(
  splits = purrr::map(forecast, ~ .x[["forecast"]][type == "Actual" | type == "Holt"]),
  title = "Holt's Trend Forecast including Prediction Interval",
  ncol = 3,
  scale = as.Date(c(min(data$index), max(data$index))),
  PI = TRUE
)


### Box-Cox Transformation -----------------------------------------------------

forecast <- predict_baselines(data, cv_setting, transform = "box")

# RMSE
rmse <- purrr::map_df(forecast, "accuracy")[, .(mean = mean(RMSE)), by = type]; rmse
rmse %>% ggplot(aes(x = type, y = mean)) + ggplot2::geom_col() +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

# Point Forecasts
fc <- purrr::map(forecast, ~ .x[["forecast"]][is.na(type), type := "Actual"])

plot_baselines_samples(
  splits = fc,
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

plot_prediction_samples(
  splits = purrr::map(forecast, ~ .x[["forecast"]][type == "Actual" | type == "Naive"]),
  title = "Holt's Trend Forecast including Prediction Interval",
  ncol = 3,
  scale = as.Date(c(min(data$index), max(data$index))),
  PI = TRUE
)
