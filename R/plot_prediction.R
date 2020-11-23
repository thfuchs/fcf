#' Plot timeseries and forecast for single split and company
#'
#' @param data data.frame containing "index" (Date), "value" (numeric) and key
#'   (character - "actual" and "predict")
#' @param title plot title
#' @param size line size
#' @param alpha line alpha value
#' @param legend legend position
#' @param scale NULL or two Date values to scale x-axis
#' @param PI add prediction interval?
#'
#' @import ggplot2
#'
#' @return ggplot2 object
#' @export
plot_prediction <- function(
  data, title, size = 0.6, alpha = 0.8, legend = "bottom", scale = NULL, PI = FALSE
) {

  g <- ggplot(data, aes(x = index, y = value, color = key)) +
    geom_line(size = size, alpha = alpha)

  if (PI) g <- g + geom_ribbon(
    data = data[key == "predict"],
    aes(ymin = lo95, ymax = hi95), fill = "red", alpha = 0.2, linetype = 0
  )

  g <- g + labs(
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
    ) + scale_x_date(limits = scale)

  return(g)
}

#' Plot multiple splits from same `rsample` split
#'
#' @param splits list of prediction data.frames
#' @param scale NULL or two Date values to scale x-axis
#' @param title diagram title
#' @param date_type string vector - one of "datetime", "date" or "character"
#' @param ncol Number of columns
#' @param PI add prediction interval?
#'
#' @return ggplot2 object
#' @export
plot_prediction_samples <- function(
  splits, title, date_type = "datetime", ncol = 2, scale, PI = FALSE
) {

  plot_list <- purrr::imap(
    splits,
    function(split, position) {
      if (date_type == "datetime") split[, index := as.Date(index)]

      plot_prediction(
        data = split,
        title = paste("Split", position),
        legend = "none",
        scale = scale,
        PI = PI
      )
    }
  )

  combined <- patchwork::wrap_plots(plot_list, ncol = ncol, guides = "collect") &
    theme(legend.position = "bottom")

  if (is.null(title)) return(combined)

  combined + patchwork::plot_annotation(
    title = title,
    theme = ggplot2::theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
  )
}
