#' Tune keras basic model with Bayes Optimization and select best performing
#' model
#'
#' @param data Univariate time series (data.frame) with columns index and value
#' @param cv_setting list of "periods_train", "periods_val", "periods_test" and
#'   "skip_span" for `rsample`
#' @param tuning_grid list of "lags" "optimizer", "dropout"
#' @param model_type One of "basic", "gru" or "lstm"
#'
#' @section Tuning Bounds:
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
tune_keras_sequential <- function(
  data, model_type, cv_setting, tuning_bounds,
  frequency = 4, iterations = 2000, multiple_h = NULL,
  test_dropout, test_recurrent_dropout
) {

  # Checks ---------------------------------------------------------------------

  testr::check_class(data, "data.frame", "tune_keras_sequential")
  testr::check_class(model_type, "character", "tune_keras_sequential")
  testr::check_class(cv_setting, "list", "tune_keras_sequential")
  testr::check_class(tuning_bounds, "list", "tune_keras_sequential")
  testr::check_class(frequency, "numeric", "tune_keras_sequential")
  testr::check_class(iterations, "numeric", "tune_keras_sequential")
  testr::check_class(multiple_h, "list", "tune_keras_sequential", allowNULL = TRUE)
  testr::check_class(test_dropout, "numeric", "tune_keras_sequential")
  testr::check_class(test_recurrent_dropout, "numeric", "tune_keras_sequential")

  # "data" contains columns "index" and "value" only (univariate time series)
  if (all(names(data)[order(names(data))] != c("index", "value"))) rlang::abort(
    message = "`data` must be a data.frame with 2 columns only: \"index\" and \"value\"",
    class = "tune_keras_sequential_data_error"
  )
  # "model_type" must be one of "simple", "gru" or "lstm"
  model_type <- rlang::arg_match(model_type, c("simple", "gru", "lstm"))
  # "cv_setting" contains "periods_train", "periods_val", "periods_test" and
  # "skip_span"
  if (all(names(cv_setting)[order(names(cv_setting))] !=
          c("periods_test", "periods_train", "periods_val", "skip_span"))) {
    rlang::abort(
      message = "`data` must be a data.frame with 2 columns only: \"index\" and \"value\"",
      class = "tune_keras_sequential_data_error"
    )
  }

  # Function -------------------------------------------------------------------

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
    purrr::possibly(otherwise = NULL, quiet = FALSE, .f = function(split) {

      # Train-Test Split
      DT_train <- rsample::analysis(split)[1:n_train]
      DT_val <- rsample::analysis(split)[(n_train+1):.N]
      DT_test <- rsample::assessment(split)

      DT <- rbind(DT_train, DT_val, DT_test)

      # Normalization
      c(data_split, metrics_norm) %<-%
        ts_normalization(DT, n_val, n_test, metrics = TRUE)
      data_split[, key := "actual"]

      # Bayes Optimization
      bayes <- rBayesianOptimization::BayesianOptimization(
        FUN = internal_keras_fun,
        bounds = tuning_bounds,
        init_points = 5,
        n_iter = 10,
        acq = "ucb",
        verbose = TRUE
      )

      # Use optimized parameters to train model on entire data set (excluding
      # test set) 10 times
      best_lag_setting <- sort(bayes$Best_Par["lag_1"]:bayes$Best_Par["lag_2"])
      c(X, Y) %<-% ts_nn_preparation(
        data_split,
        lag_setting = best_lag_setting,
        length_val = 0,
        length_test = n_test
      )
      best_optimizer_type <- switch(
        bayes$Best_Par["optimizer_type"],
        `1` = "rmsprop",
        `2` = "adam"
      )

      best_model <- keras_sequential(
        X, Y,
        model_type = model_type,
        tsteps = length(best_lag_setting),
        n_epochs = bayes$Best_Par["n_epochs"],
        n_units = bayes$Best_Par["n_units"],
        dropout_in_test = TRUE,
        optimizer_type = best_optimizer_type,
        dropout = bayes$Best_Par["dropout"],
        recurrent_dropout = bayes$Best_Par["recurrent_dropout"],
        learning_rate = bayes$Best_Par["learning_rate"]
      )

      reticulate::source_python(file = system.file("create_dropout_model.py"))
      dropout_model <- create_dropout_model(model, test_dropout, test_recurrent_dropout)

      fc_iter <- vapply(1:iterations, function(i) {
        predict(dropout_model, X$test) * metrics_norm$scale + metrics_norm$center
      }, FUN.VALUE = numeric(n_test))

      fc_mean <- apply(fc_iter, 1, mean)
      fc_lower <- apply(fc_iter, 1, quantile, 0.5 - level / 200, type = 8)
      fc_upper <- apply(fc_iter, 1, quantile, 0.5 + level / 200, type = 8)

      fc <- data.table::data.table(
        index = data_split[(.N-n_test+1):.N, index],
        value = fc_mean,
        lo95 = fc_lower,
        hi95 = fc_upper
      )
      fc[, `:=` (key = "predict", type = model_type)]
      fc <- rbind(data_split, fc, fill = TRUE)
      if (!is.null(fc[["ticker"]])) fc[, ticker := unique(DT$ticker)]

      ### Accuracy Measures
      fc_values <- fc[key == "predict"]

      # Point Forecast Measures
      acc_MAPE <- sapply(
        multiple_h,
        function(h) mape(actual = DT_test[h,value], forecast = fc_values[h,value]))
      acc_sMAPE <- sapply(
        multiple_h,
        function(h) smape(actual = DT_test[h,value], forecast = fc_values[h,value]))
      acc_MASE <- sapply(
        multiple_h, function(h) mase(
          data = DT[1:(n_initial+max(h)),value],
          forecast = fc_values[h,value], m = frequency)
      )

      # Prediction Interval Measures
      # acc_SMIS <- sapply(
      #   multiple_h, function(h) smis(
      #     data = DT[1:(n_initial+max(h)),value],
      #     lower = fc_values[h,lo95],
      #     upper = fc_values[h,hi95],
      #     h = max(h), m = frequency, level = 0.95)
      # )
      # acc_ACD <- sapply(multiple_h, function(h) acd(
      #   actual = DT_test[h,value],
      #   lower = fc_values[h,lo95],
      #   upper = fc_values[h,hi95],
      #   level = 0.95)
      # )
      #
      # Accuracy result
      acc <- data.table::data.table(
        type = model_type, h = names(multiple_h),
        mape = acc_MAPE, smape = acc_sMAPE, mase = acc_MASE
        # smis = acc_SMIS, acd = acc_ACD
      )

      ### Output
      list(
        forecast = fc,
        accuracy = acc,
        tuning_params = as.list(bayes$Best_Par)
      )
    })
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

    # eval_mean <- eval_DT[, lapply(.SD, mean, na.rm=TRUE), by=type]
    # eval_std <- eval_DT[, lapply(.SD, stats::sd, na.rm=TRUE), by=type]
    # eval_median <- eval_DT[, lapply(.SD, stats::median, na.rm=TRUE), by=type]
    #
    # return(list(
    #   evaluation = eval_DT,
    #   mean = eval_mean,
    #   std = eval_std,
    #   median = eval_median
    # ))
    # return(eval_DT)

  safe_run <- purrr::possibly(run, otherwise = NA, quiet = FALSE)

  # Tuning Process
  tune_results <- safe_run()

  # tune_results <- tuning_grid %>%
  #   purrr::cross() %>%
  #   purrr::map(function(params) {
  #     if (is.null(params$lags)) params$lags <- 1
  #     safe_run(
  #       lag_setting = params$lags,
  #       n_epochs = params$n_epochs,
  #       n_units = params$n_epochs,
  #       optimizer = params$optimizer,
  #       dropout = params$dropout,
  #       recurrent_dropout = params$dropout,
  #       patience = params$patience
  #     )
  #   })

  results_DT <- purrr::map_df(
    purrr::compact(tune_results),
    ~ .x[type == "test", lapply(.SD, mean, na.rm=TRUE), by = type][, -1]
  )

  min_index <- which.min(results_DT$loss)
  min_tune_params <- purrr::cross(tuning_grid)[[min_index]]
  min_tune_params$index <- min_index

  return(list(results = tune_results, min_params = min_tune_params))
}


internal_keras_fun <- function(
  n_units, n_epochs, lag_1, lag_2, dropout, recurrent_dropout, optimizer_type, learning_rate
) {

  lag_setting <- sort(lag_1:lag_2)
  optimizer <- switch(
    optimizer_type,
    `1` = optimizer_rmsprop(lr = learning_rate),
    `2` = optimizer_adam(lr = learning_rate)
  )

  # Reshaping
  c(X, Y) %<-% ts_nn_preparation(
    data_split,
    lag_setting = lag_setting,
    length_val = n_val,
    length_test = n_test
  )

  model <- keras_model_sequential()

  if (model_type == "simple") {
    model %>%
      layer_simple_rnn(
        units             = n_units,
        input_shape       = c(length(lag_setting), 1),
        dropout           = dropout,
        recurrent_dropout = recurrent_dropout
      ) %>%
      layer_dense(units = 1)

  } else if (model_type == "gru") {
    # a. GRU
    model %>% layer_gru(
      units             = n_units,
      input_shape       = c(length(lag_setting), 1),
      dropout           = dropout,
      recurrent_dropout = recurrent_dropout
    ) %>%
      layer_dense(units = 1)

  } else if (model_type == "lstm") {
    # b. LSTM
    model %>%
      layer_lstm(
        units             = n_units,
        input_shape       = c(length(lag_setting), 1),
        dropout           = dropout,
        recurrent_dropout = recurrent_dropout
      ) %>%
      layer_dense(units = 1)
  }

  model %>% compile(optimizer = optimizer, loss = "mse")

  history <- model %>% fit(
    x               = X$train,
    y               = Y$train,
    steps_per_epoch = 1,
    epochs          = n_epochs,
    batch_size      = NULL,
    verbose         = 0,
    shuffle         = FALSE,
    validation_data = list(X$val, Y$val),
    view_metrics    = FALSE
  )

  return(list(Score = -history$metrics$val_loss[n_epochs], Pred = 0))
}
