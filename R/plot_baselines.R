#' Plot Forecasts by baseline methods
#'
#' @param data data.frame containing "Date", "value" and "group"
#' @param col_date Date column in `data`, default to "index"
#' @param col_value Value column in `data`, default to "value"
#' @param col_group Group column in `data`, default to "type" (e.g.
#'   "Actual", "Naive", ...) - see example
#' @param title diagram title
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
#'
#' @examples
#' plot_baselines(
#'   data = tsRNN::fc_baseline,
#'   colors = c("Actual" = "black", "Snaive" = "blue", "Holt" = "green")
#' )
plot_baselines <- function(
                           data,
                           col_date = "index",
                           col_value = "value",
                           col_group = "type",
                           title = NULL,
                           size = 0.6,
                           alpha = 0.8,
                           colors = NULL,
                           legend = "bottom",
                           scale = NULL) {

  ### Checks -------------------------------------------------------------------
  testr::check_class(data, "data.frame", "plot_baselines")
  testr::check_class(col_date, "character", "plot_baselines")
  testr::check_class(col_value, "character", "plot_baselines")
  testr::check_class(col_group, "character", "plot_baselines")
  testr::check_class(size, "numeric", "plot_baselines")
  testr::check_class(alpha, "numeric", "plot_baselines")
  testr::check_class(colors, "character", "plot_baselines", allowNULL = TRUE)
  testr::check_class(legend, "character", "plot_baselines", allowNULL = TRUE)
  testr::check_class(scale, "Date", "plot_baselines", allowNULL = TRUE)

  # Check column's fit
  if (is.null(data[[col_date]]) || !inherits(data[[col_date]], "Date")) {
    rlang::abort(
      message = "Variable specified by `col_date` must be class \"Date\".",
      class = "plot_baselines_col_date_error"
    )
  }
  if (is.null(data[[col_value]]) || !inherits(data[[col_value]], "numeric")) {
    rlang::abort(
      message = "Variable specified by `col_value` must be class \"numeric\".",
      class = "plot_baselines_col_value_error"
    )
  }
  if (is.null(data[[col_group]]) || !inherits(data[[col_group]], "character")) {
    rlang::abort(
      message = "Variable specified by `col_group` must be class \"character\".",
      class = "plot_baselines_col_group_error"
    )
  }

  ### Function -----------------------------------------------------------------
  g <- ggplot(data, aes_string(x = col_date, y = col_value, color = col_group)) +
    geom_line(size = size, alpha = alpha) +
    labs(
      title = title,
      subtitle = sprintf("%s to %s", min(data[[col_date]]), max(data[[col_date]])),
      y = NULL, x = NULL
    )

  if (!is.null(colors)) g <- g + scale_colour_manual(values = colors)

  g <- g + theme(
    plot.background = element_rect(fill = NA),
    panel.background = element_rect(fill = NA, colour = "black"),
    panel.grid.major = element_line(colour = "lightgrey"),
    legend.position = legend,
    legend.title = element_blank(),
    legend.key = element_blank()
  ) +
    scale_x_date(limits = scale)

  return(g)
}

#' Plot cross validated samples of forecasts by baseline methods
#'
#' @param splits list of prediction data.frames
#' @param col_date Date column in data.frame, default to "index"
#' @param col_value Value column in data.frame, default to "value"
#' @param col_group Group column in data.frame, default to "type" (e.g.
#'   "Actual", "Naive", ...) - see example
#' @param title diagram title
#' @param date_type string vector - one of "datetime", "Date" or "character"
#' @param ncol Number of columns
#' @param scale NULL or two Date values to scale x-axis
#' @param colors named character vector to fit color to type
#'
#' @return ggplot2 object
#' @export
#'
#' @examples
#' data <- tsRNN::fc_baseline
#' plot_baselines_samples(
#'   splits = list(data, data),
#'   colors = c("Actual" = "black", "Snaive" = "blue", "Holt" = "green"),
#'   ncol = 2L
#' )
plot_baselines_samples <- function(
                                   splits,
                                   col_date = "index",
                                   col_value = "value",
                                   col_group = "type",
                                   title = NULL,
                                   colors = NULL,
                                   date_type = "datetime",
                                   ncol = 3L,
                                   scale = NULL) {

  ### Checks -------------------------------------------------------------------

  testr::check_class(splits, "list", "plot_baselines_samples")
  testr::check_class(colors, "character", "plot_baselines_samples", allowNULL = TRUE)
  testr::check_class(date_type, "character", "plot_baselines_samples")
  date_type <- rlang::arg_match(date_type, c("Date", "datetime", "character"))
  testr::check_class(ncol, "integer", "plot_baselines_samples")
  testr::check_class(scale, "Date", "plot_baselines_samples", allowNULL = TRUE)

  ### Function -----------------------------------------------------------------

  plot_list <- purrr::imap(
    splits,
    function(split, position) {
      if (date_type == "datetime" || date_type == "character") {
        split[, paste(col_date) := as.Date(get(col_date))]
      }

      plot_baselines(
        data = split,
        col_date = col_date,
        col_value = col_value,
        col_group = col_group,
        title = paste("Split", position),
        colors = colors,
        legend = "none",
        scale = scale
      )
    }
  )

  combined <- patchwork::wrap_plots(plot_list, ncol = ncol, guides = "collect") &
    theme(legend.position = "bottom")

  if (is.null(title)) {
    return(combined)
  }

  combined + patchwork::plot_annotation(
    title = title,
    theme = ggplot2::theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    )
  )
}
