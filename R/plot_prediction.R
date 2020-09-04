#' Title
#'
#' @param data data.frame containing "index" (Date), "value" (numeric) and key
#'   (character - "actual" and "predict")
#' @param size line size
#' @param alpha line alpha value
#' @param legend legend position
#' @param scale NULL or two Date values to scale x-axis
#'
#' @import ggplot2
#'
#' @return ggplot2 object
#' @export
plot_prediction <- function(
  data, title, size = 0.6, alpha = 0.8, legend = "bottom", scale = NULL
) {

  g <- ggplot(data, aes(x = index, y = value, color = key)) +
    geom_line(size = size, alpha = alpha) +
    labs(
      title    = title,
      subtitle = sprintf("%s to %s", min(data$index), max(data$index)),
      y = NULL, x = NULL
    ) +
    scale_colour_manual(values = c("actual" = "black", "predict" = "red")) +
    theme(
      plot.background = element_rect(fill = NA),
      panel.background = element_rect(fill = NA, colour = "black"),
      panel.grid.major = element_line(colour = "lightgrey"),
      legend.position = legend
    )  +
    scale_x_date(limits = scale)

  return(g)
}

#' Plot multiple splits from same `rsample` split
#'
#' @param splits list of prediction data.frames
#' @param scale NULL or two Date values to scale x-axis
#'
#' @return list of ggplot2 object
#' @export
plot_prediction_samples <- function(splits, title, ncol = 2, scale) {

  plot_list <- purrr::imap(
    splits,
    function(split, position) {
      split[, index := as.Date(index)]

      plot_prediction(
        split,
        title = paste("Split", position),
        legend = "none",
        scale = scale
      )
    }
  )

  legend <- cowplot::get_legend(plot_list[[1]] + theme(legend.position = "bottom"))
  p_body <- cowplot::plot_grid(plotlist = plot_list, ncol = ncol)
  p_title <- cowplot::ggdraw() +
    cowplot::draw_label(title, size = 14, fontface = "bold")

  cowplot::plot_grid(
    p_title, p_body, legend, ncol = 1, rel_heights = c(0.05, 1, 0.05))
}
