### Data -----------------------------------------------------------------------
DT <- fcf::DT_unh
c(data, normalization) %<-% ts_normalization(DT, 6, 6, metrics = TRUE)

### Keras Function -------------------------------------------------------------
keras_simple <- function(
  n_units, n_epochs, lag_1, lag_2, dropout, recurrent_dropout, optimizer_type, learning_rate
) {

  lag_setting <- sort(lag_1:lag_2)

  c(X, Y) %<-% ts_nn_preparation(
    data,
    lag_setting = lag_setting,
    length_val = 6,
    length_test = 6
  )

  model <- keras_model_sequential()

  model %>% layer_simple_rnn(
      units             = n_units,
      input_shape       = c(length(lag_setting), 1),
      dropout           = dropout,
      recurrent_dropout = recurrent_dropout
  ) %>%
    layer_dense(units = 1)

  optimizer <- switch(
    optimizer_type,
    `1` = optimizer_rmsprop(lr = learning_rate),
    `2` = optimizer_adam(lr = learning_rate)
  )

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

### Tuning settings ------------------------------------------------------------
search_bound_keras <- list(
  lag_1 = c(1L, 4L),
  lag_2 = c(1L, 4L),
  n_units = c(8L, 64L),
  n_epochs = c(10L, 50L),
  optimizer_type = c(1L, 2L), # 1 = "rmsprop", 2 = "adam"
  dropout = c(0, 0.7),
  recurrent_dropout = c(0, 0.7),
  learning_rate = c(0.001, 0.1)
)

### Optimization
set.seed(123)
bayes <- rBayesianOptimization::BayesianOptimization(
  FUN = keras_simple,
  bounds = search_bound_keras,
  init_points = 5,
  n_iter = 10,
  acq = "ucb",
  verbose = TRUE
)
