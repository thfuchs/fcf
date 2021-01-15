#' Plot time series (incl. forecasts) for single cross validation split
#'
#' E.g. plot result for selected split from \link{cv_arima} (by `ggplot2`)
#'
#' @param data data.frame containing "index" (Date), "value" (numeric) and key
#'   (character - "actual" and "predict")
#' @param col_date Date column in `data`, default to "index"
#' @param col_value Value column in `data`, default to "value"
#' @param col_group Group column in `data`, default to "key" ("actual" and
#'   "predict") - see example
#' @param title plot title
#' @param size line size
#' @param alpha line alpha value
#' @param legend legend position
#' @param scale NULL or two Date values to scale x-axis
#' @param PI add prediction interval?
#' @param col_pi_high Upper confidence bound in `data`
#' @param col_pi_low Lower confidence bound in `data`
#'
#' @import ggplot2
#'
#' @return ggplot2 object
#' @export
#'
#' @examples
#' plot_prediction(
#'   data = tsRNN::fc_arima,
#'   title = "ARIMA Forecast versus actual data"
#' )
#'
#' # With Prediction Interval
#' plot_prediction(
#'   data = tsRNN::fc_arima,
#'   title = "ARIMA Forecast with prediction interval",
#'   PI = TRUE
#' )
plot_prediction <- function(
                            data,
                            col_date = "index",
                            col_value = "value",
                            col_group = "key",
                            title = NULL,
                            size = 0.6,
                            alpha = 0.8,
                            legend = "bottom",
                            scale = NULL,
                            PI = FALSE,
                            col_pi_high = "hi95",
                            col_pi_low = "lo95") {

  ### Checks -------------------------------------------------------------------
  testr::check_class(data, "data.frame")
  testr::check_class(col_date, "character")
  testr::check_class(col_value, "character")
  testr::check_class(col_group, "character")
  testr::check_num_int(size)
  testr::check_num_int(alpha)
  testr::check_class(legend, "character", allowNULL = TRUE)
  testr::check_class(scale, "Date", allowNULL = TRUE)
  testr::check_class(PI, "logical")
  if (PI) {
    testr::check_class(col_pi_high, "character")
    testr::check_class(col_pi_low, "character")
  }

  # Check column's fit
  if (is.null(data[[col_date]]) || !inherits(data[[col_date]], "Date")) {
    rlang::abort(
      message = "Variable specified by `col_date` must be class \"Date\".",
      class = "plot_prediction_col_date_error"
    )
  }
  if (is.null(data[[col_value]]) || !inherits(data[[col_value]], "numeric")) {
    rlang::abort(
      message = "Variable specified by `col_value` must be class \"numeric\".",
      class = "plot_prediction_col_value_error"
    )
  }
  if (is.null(data[[col_group]]) || !inherits(data[[col_group]], "character")) {
    rlang::abort(
      message = "Variable specified by `col_group` must be class \"character\".",
      class = "plot_prediction_col_group_error"
    )
  }
  if (PI) {
    if (is.null(data[[col_pi_high]]) || !inherits(data[[col_pi_high]], "numeric")) {
      rlang::abort(
        message = "Variable specified by `col_pi_high` must be class \"numeric\".",
        class = "plot_prediction_col_pi_high_error"
      )
    }
    if (is.null(data[[col_pi_low]]) || !inherits(data[[col_pi_low]], "numeric")) {
      rlang::abort(
        message = "Variable specified by `col_pi_low` must be class \"numeric\".",
        class = "plot_prediction_col_pi_low_error"
      )
    }
  }

  ### Function -----------------------------------------------------------------
  g <- ggplot(data, aes_string(x = col_date, y = col_value, color = col_group)) +
    geom_line(size = size, alpha = alpha)

  if (PI) {
    g <- g + geom_ribbon(
      data = data[key == "predict"],
      aes_string(ymin = col_pi_low, ymax = col_pi_high),
      fill = "red", alpha = 0.2, linetype = 0
    )
  }

  g <- g + labs(
    title = title,
    subtitle = sprintf("%s to %s", min(data[[col_date]]), max(data[[col_date]])),
    y = NULL, x = NULL
  ) +
    scale_colour_manual(values = c("actual" = "black", "predict" = "red")) +
    theme(
      plot.background = element_rect(fill = NA),
      panel.background = element_rect(fill = NA, colour = "black"),
      panel.grid.major = element_line(colour = "lightgrey"),
      legend.position = legend,
      legend.title = element_blank(),
      legend.key = element_blank()
    ) + scale_x_date(limits = scale)

  return(g)
}

#' Plot multiple splits from list with forecast results
#'
#' Plot results from \link{cv_arima} (by `ggplot2`)
#'
#' @param splits list of prediction data.frames
#' @param col_date Date column, default to "index"
#' @param col_value Value column, default to "value"
#' @param col_group Group column , default to "key" ("actual" and
#'   "predict") - see example
#' @param scale NULL or two Date values to scale x-axis
#' @param title diagram title
#' @param date_type string vector - one of "datetime", "date" or "character"
#' @param ncol Number of columns
#' @param PI add prediction interval?
#' @param col_pi_high Upper confidence bound
#' @param col_pi_low Lower confidence bound
#'
#' @return ggplot2 object
#' @export
#'
#' @examples
#' data <- tsRNN::fc_arima
#' plot_prediction_samples(
#'   splits = list(data, data),
#'   ncol = 2L
#' )
#'
#' # with Prediction Interval
#' plot_prediction_samples(
#'   splits = list(data, data),
#'   ncol = 2L,
#'   PI = TRUE
#' )
plot_prediction_samples <- function(
                                    splits,
                                    col_date = "index",
                                    col_value = "value",
                                    col_group = "key",
                                    title = NULL,
                                    date_type = "datetime",
                                    ncol = 3L,
                                    scale = NULL,
                                    PI = FALSE,
                                    col_pi_high = "hi95",
                                    col_pi_low = "lo95") {

  ### Checks -------------------------------------------------------------------

  testr::check_class(splits, "list")
  testr::check_class(date_type, "character")
  date_type <- rlang::arg_match(date_type, c("Date", "datetime", "character"))
  testr::check_class(ncol, "integer")
  testr::check_class(scale, "Date", allowNULL = TRUE)
  testr::check_class(PI, "logical")

  ### Function -----------------------------------------------------------------

  plot_list <- purrr::imap(
    splits,
    function(split, position) {
      if (date_type == "datetime" || date_type == "character") {
        split[, paste(col_date) := as.Date(get(col_date))]
      }

      plot_prediction(
        data = split,
        col_date = col_date,
        col_value = col_value,
        col_group = col_group,
        title = paste("Split", position),
        legend = "none",
        scale = scale,
        PI = PI,
        col_pi_high = col_pi_high,
        col_pi_low = col_pi_low
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
