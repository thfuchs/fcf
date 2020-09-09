#' Title
#'
#' @param DT univariate time series - data.table object with 2 variables:
#'   "index" (Posixct date-time) and "value". Usually single DT from `rsample`
#' @param epochs number of epochs to train model (default to 300)
#' @param lag_setting numeric vector specifying lags to train on
#' @param length_val length of validation set
#' @param length_test length of test set
#' @param save_model save model? If TRUE, `file_path` required
#' @param filepath path to save model. Must end with `.hdf5`
#' @param ... additional parameters passed to Keras
#'
#' @import keras
#' @import data.table
#'
#' @return Predictions
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Prepare data
#' apple <- fcf::dow30[
#' ticker == "AAPL" & date > as.POSIXct("1995-01-01"),
#'   .SD, .SDcols = c("date", "fcf")
#' ]
#' data.table::setnames(apple, c("date", "fcf"), c("index", "value"))
#'
#' # Predict
#' predict_keras_sequential(DT = apple)
#'
#' }
predict_keras_sequential <- function(
  DT,
  epochs = 300,
  lag_setting = 1:4,
  length_val = 16,
  length_test = 8,
  optimizer_type = "rmsprop",
  save_model = FALSE,
  filepath = NULL,
  ...
) {

  ### Checks -----------------------------------------------------------------

  # Variable type
  testr::check_class(DT, "data.table", "predict_keras_sequential")

  if (!rlang::inherits_any(epochs, c("numeric", "integer"))) rlang::abort(
    message = sprintf("`epochs` must be numeric, not of class \"%s\".",
                      paste(class(var), collapse = " / ")),
    class = "predict_keras_sequential_epochs_error")

  if (!rlang::inherits_any(lag_setting, c("numeric", "integer"))) rlang::abort(
    message = sprintf("`lag_setting` must be numeric, not of class \"%s\".",
                      paste(class(var), collapse = " / ")),
    class = "predict_keras_sequential_lag_setting_error")

  if (!rlang::inherits_any(length_val, c("numeric", "integer"))) rlang::abort(
    message = sprintf("`length_val` must be numeric, not of class \"%s\".",
                      paste(class(var), collapse = " / ")),
    class = "predict_keras_sequential_length_val_error")

  if (!rlang::inherits_any(length_test, c("numeric", "integer"))) rlang::abort(
    message = sprintf("`length_test` must be numeric, not of class \"%s\".",
                      paste(class(var), collapse = " / ")),
    class = "predict_keras_sequential_length_test_error")

  testr::check_class(optimizer_type, "character", "predict_keras_sequential")

  testr::check_class(save_model, "logical", "predict_keras_sequential")
  if (save_model)
    testr::check_class(filepath, "character", "predict_keras_sequential")

  # "DT" contains columns "index" and "value" only (univariate time series)
  if (all(names(DT)[order(names(DT))] != c("index", "value"))) rlang::abort(
    message = "`DT` must be a data.frame with 2 columns only: \"index\" and \"value\"",
    class = "predict_keras_sequential_DT_error"
  )

  # Length 1 for "epochs", "length_val", "length_test" and "save_model"
  if (length(epochs) != 1) rlang::abort(
    message = "`epochs` must be a numeric vector of length 1.",
    class = "predict_keras_sequential_epochs_error"
  )
  if (length(length_val) != 1) rlang::abort(
    message = "`length_val` must be a numeric vector of length 1.",
    class = "predict_keras_sequential_length_val_error"
  )
  if (length(length_test) != 1) rlang::abort(
    message = "`length_test` must be a numeric vector of length 1.",
    class = "predict_keras_sequential_length_test_error"
  )
  if (length(optimizer_type) != 1) rlang::abort(
    message = "`optimizer_type` must be a character vector of length 1.",
    class = "predict_keras_sequential_optimizer_type_error"
  )
  if (length(save_model) != 1) rlang::abort(
    message = "`save_model` must be a logical vector of length 1.",
    class = "predict_keras_sequential_save_model_error"
  )
  if (
    save_model && length(filepath) != 1 ||
    save_model && !grepl("\\.hdf5$", filepath)
  ) rlang::abort(
    message = "`filepath` must be a valid path with a valid hdf5 file name.",
    class = "predict_keras_sequential_filepath_error"
  )

  ### Function ---------------------------------------------------------------
  sequential_prediction <- function() {

    # Input Parameters
    metrics <- NULL

    # Pre-Processing: Normalizing the data
    c(data, metrics$normalization) %<-%
      ts_normalization(DT, length_val, length_test, metrics = TRUE)

    # Train-Validation-Test Split
    c(X, Y) %<-% ts_nn_preparation(
      data,
      lag_setting = lag_setting,
      length_val = length_val,
      length_test = length_test
    )

    ### Model
    c(model, metrics$train, metrics$val, metrics$test) %<-% keras_basic_sequential(
      X, Y,
      n_epochs = epochs,
      optimizer_type = optimizer_type,
      patience = 10,
      return_model = TRUE
    )

    # Save Model
    if (save_model) save_model_hdf5(model, filepath, overwrite = FALSE)

    ### Prediction
    pred_out <- model %>%
      predict(X$test) %>%
      .[,1,1]

    # Re-Transform values
    norm_std <- metrics$normalization$scale
    norm_mean <- metrics$normalization$center

    pred_df <- data.frame(
      index   = tail(DT$index, length(Y$test)),
      value   = pred_out * norm_std + norm_mean
    )

    # Combine actual data with predictions
    DT$key <- "actual"
    pred_df$key <- "predict"

    ret <- rbind(DT, pred_df) %>%
      dplyr::arrange(key, index) %>%
      dplyr::mutate(key = forcats::as_factor(key))

    return(list(predictions = ret, metrics = metrics))
  }

  safe_sequential <- purrr::possibly(
    sequential_prediction, otherwise = NA, quiet = FALSE)

  safe_sequential()
}
