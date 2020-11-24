# Cross validation for timeseries with rsample
# Source: https://www.tidymodels.org/learn/models/time-series/ and
# https://www.business-science.io/timeseries-analysis/2018/07/01/keras-lstm-sunspots-part2.html

apple <- tsRNN::dow30[
  ticker == "AAPL" & date > as.POSIXct("1995-01-01"),
  .SD, .SDcols = c("date", "fcf")
]
data.table::setnames(apple, c("date", "fcf"), c("index", "value"))

periods_train <- 64
periods_test  <- 12
skip_span     <- 7

rolling_origin_resamples <- rsample::rolling_origin(
  apple,
  initial    = periods_train,
  assess     = periods_test,
  cumulative = FALSE,
  skip       = skip_span
)

rolling_origin_resamples

# Visualizing the backtesting strategy
library(ggplot2)
library(cowplot)

# Plotting function for a single split
split1_tbl <- dplyr::bind_rows(
  rsample::training(rolling_origin_resamples$splits[[1]]) %>% dplyr::mutate(key = "training"),
  rsample::testing(rolling_origin_resamples$splits[[1]]) %>% dplyr::mutate(key = "testing")
)

split1_tbl %>%
  dplyr::mutate(index = as.Date(index)) %>%
  ggplot(aes(x = index, y = value, color = key)) +
  geom_line(size = 0.8, alpha = 0.8) +
  labs(
    title    = "Split 1",
    subtitle = sprintf("%s to %s", min(split1_tbl$index), max(split1_tbl$index)),
    y = NULL, x = NULL
  ) +
  scale_colour_manual(values = c("training" = "black", "testing" = "red")) +
  theme(
    plot.background = element_rect(fill = NA),
    panel.background = element_rect(fill = NA, colour = "black"),
    panel.grid.major = element_line(colour = "lightgrey"),
    legend.position = "bottom"
  )  +
  scale_x_date(limits = as.Date(c(min(apple$index), max(apple$index))))

# Plotting function for all splits equivalently
plot_list <- purrr::map(
  rolling_origin_resamples$splits,
  function(x) {
    split_tbl <- dplyr::bind_rows(
      rsample::training(x) %>% dplyr::mutate(key = "training"),
      rsample::testing(x) %>% dplyr::mutate(key = "testing")
    )
    split_tbl %>%
      dplyr::mutate(index = as.Date(index)) %>%
      ggplot(aes(x = index, y = value, color = key)) +
      geom_line(size = 0.7, alpha = 0.8) +
      labs(
        title    = x$id,
        subtitle = sprintf("%s to %s", min(split_tbl$index), max(split_tbl$index)),
        y = NULL, x = NULL
      ) +
      scale_colour_manual(values = c("training" = "black", "testing" = "red")) +
      theme(
        plot.background = element_rect(fill = NA),
        panel.background = element_rect(fill = NA, colour = "black"),
        panel.grid.major = element_line(colour = "lightgrey"),
        legend.position = "none"
      ) +
      scale_x_date(limits = as.Date(c(min(apple$index), max(apple$index))))
  }
)

legend <- get_legend(plot_list[[1]] + theme(legend.position = "bottom"))
p_body <- plot_grid(plotlist = plot_list, ncol = 2)
p_title <- ggdraw() +
  draw_label("Rolling Origin Sampling Plan", size = 14, fontface = "bold")

plot_grid(p_title, p_body, legend, ncol = 1, rel_heights = c(0.05, 1, 0.05))
