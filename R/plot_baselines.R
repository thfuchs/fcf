#' Plot Forecasts by baseline methods
#'
#' @param data data.frame containing "index" (Date), "value" (numeric) and type
#'   (character - "Actual" and forecast type)
#' @param size line size
#' @param alpha line alpha value
#' @param colors named character vector to fit color to type
#' @param legend legend position
#' @param scale NULL or two Date values to scale x-axis
#'
#' @import ggplot2
#'
#' @return ggplot2 object
#' @export
plot_baselines <- function(
  data, title, size = 0.6, alpha = 0.8, colors, legend = "bottom", scale = NULL
) {

  g <- ggplot(data, aes(x = index, y = value, color = type)) +
    geom_line(size = size, alpha = alpha) +
    labs(
      title    = NULL,
      subtitle = sprintf("%s to %s", min(data$index), max(data$index)),
      y = NULL, x = NULL
    ) +
    scale_colour_manual(values = colors) +
    theme(
      plot.background = element_rect(fill = NA),
      panel.background = element_rect(fill = NA, colour = "black"),
      panel.grid.major = element_line(colour = "lightgrey"),
      legend.position = legend
    ) +
    scale_x_date(limits = scale)

  return(g)
}

#' Plot cross validated samples of forecasts by baseline methods
#'
#' @param splits list of prediction data.frames
#' @param title diagram title
#' @param date_type string vector - one of "datetime", "date" or "character"
#' @param ncol Number of columns
#' @param scale NULL or two Date values to scale x-axis
#' @param colors named character vector to fit color to type
#'
#' @return ggplot2 object
#' @export
plot_baselines_samples <- function(
  splits, title, colors, date_type = "datetime", ncol = 2, scale
) {

  plot_list <- purrr::imap(
    splits,
    function(split, position) {
      if (date_type == "datetime") split[, index := as.Date(index)]

      plot_baselines(
        data = split,
        title = paste("Split", position),
        colors = colors,
        legend = "none",
        scale = scale
      )
    }
  )

  legend <- cowplot::get_legend(
    plot_list[[1]] + theme(
      legend.position = "bottom",
      legend.justification = "center"
    )
  )
  p_body <- cowplot::plot_grid(plotlist = plot_list, ncol = ncol)
  p_title <- cowplot::ggdraw() +
    cowplot::draw_label(title, size = 14, fontface = "bold")

  cowplot::plot_grid(
    p_title, p_body, legend, ncol = 1, rel_heights = c(0.05, 1, 0.1))
}
