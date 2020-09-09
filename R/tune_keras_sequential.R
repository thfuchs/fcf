#' Run keras basic model over all tuning parameters and select best performing
#' model
#'
#' @param data Univariate time series (data.frame) with columns index and value
#' @param cv_setting list of "periods_train", "periods_val", "periods_test" and
#'   "skip_span" for `rsample`
#' @param tuning_grid list of "lags" "optimizer", "dropout"
#'
#' @section Tuning Grid:
#' The following parameters are (currently) available for tuning.
#' - `lags`: named list of numeric vectors with length of lags to use
#' - `optimizer`: character vector specifying optimizer to use. One of "adam"
#'    and "rmsprop"
#' - `dropout`: numeric vector specifying dropout rates
#'
#' @importFrom magrittr %>%
#' @import data.table
#'
#' @return list of "results" and "min_params"
#' @export
tune_keras_sequential <- function(data, cv_setting, tuning_grid = NULL) {

  # Checks ---------------------------------------------------------------------

  # Function -------------------------------------------------------------------
  run <- function(lag_setting, n_epochs, optimizer, dropout, patience) {

    n_train <- cv_setting$periods_train
    n_val <- cv_setting$periods_val
    n_initial <- n_train + n_val
    n_test <- cv_setting$periods_test

    rolling_origin_resamples <- rsample::rolling_origin(
      data,
      initial    = n_initial,
      assess     = n_test,
      cumulative = FALSE,
      skip       = cv_setting$skip_span
    )

    history_resample <- purrr::map(
      rolling_origin_resamples$splits,
      function(split) {
        # Train-Test Split
        DT_train <- rsample::analysis(split)[1:n_train]
        DT_val <- rsample::analysis(split)[(n_train+1):.N]
        DT_test <- rsample::assessment(split)

        DT <- rbind(DT_train, DT_val, DT_test)

        # Normalization
        data <- ts_normalization(DT, n_val, n_test, metrics = FALSE)

        # Reshaping
        c(X, Y) %<-% ts_nn_preparation(
          data,
          lag_setting = lag_setting,
          length_val = n_val,
          length_test = n_test
        )

        keras_basic_sequential(
          X, Y,
          n_epochs = n_epochs,
          optimizer_type = optimizer,
          patience = patience
        )
      }
    )

    history_resample

    eval_train <- data.table::setDT(purrr::map_df(history_resample, "train"))
    eval_val <- data.table::setDT(purrr::map_df(history_resample, "val"))
    eval_test <- data.table::setDT(purrr::map_df(history_resample, "test"))

    eval_DT <- rbind(
      eval_train[, type := "train"],
      eval_val[, type := "val"],
      eval_test[, type := "test"]
    )

    eval_mean <- eval_DT[, lapply(.SD, mean, na.rm=TRUE), by=type]
    eval_std <- eval_DT[, lapply(.SD, sd, na.rm=TRUE), by=type]
    eval_median <- eval_DT[, lapply(.SD, median, na.rm=TRUE), by=type]

    return(list(
      evaluation = eval_DT,
      mean = eval_mean,
      std = eval_std,
      median = eval_median
    ))
  }

  safe_run <- purrr::possibly(run, otherwise = NA, quiet = FALSE)

  # Tuning Process
  tune_results <- tuning_grid %>%
    purrr::cross() %>%
    purrr::map(function(params) {
      if (is.null(params$lags)) params$lags <- 1
      safe_run(
        lag_setting = params$lags,
        n_epochs = params$n_epochs,
        optimizer = params$optimizer,
        dropout = params$dropout,
        patience = params$patience
      )
    })

  results_DT <- tune_results %>%
    purrr::compact() %>%
    purrr::map("mean") %>%
    purrr::map_df(~ .x[.x$type == "test",])

  min_index <- which.min(results_DT$loss)
  min_tune_params <- purrr::cross(tuning_grid)[[min_index]]
  min_tune_params$index <- min_index

  return(list(results = tune_results, min_params = min_tune_params))
}
