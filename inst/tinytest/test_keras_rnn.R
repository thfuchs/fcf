suppressPackageStartupMessages(library(keras))

load(file = system.file("tinytest_data/rnn_arrays.rda", package = "tsRNN"))
get_keras_layer <- function(...) reticulate::py_to_r(get_config(get_layer(...)))
test_array <- array(runif(12, 0.1, 3), dim = c(6L, 2L, 1L))

# Check succesful --------------------------------------------------------------

### "model_type" and "tsteps"
# simple
model_simple <- keras_rnn(X, Y, model_type = "simple", tsteps = 2, n_epochs = 10)
model_simple_layer_recurrent <- get_keras_layer(model_simple, index = 2)
model_simple_layer_dropout <- get_keras_layer(model_simple, index = 3)

expect_equivalent(
  model_simple, load_model_hdf5(system.file("tinytest_data/simple_keras_rnn.hdf5", package = "tsRNN"))
)
expect_true(rlang::inherits_all(
  model_simple, c("keras.engine.functional.Functional", "keras.engine.training.Model")
))
expect_true(grepl("simple_rnn", model_simple_layer_recurrent$name))
expect_identical(model_simple_layer_recurrent$batch_input_shape, list(NULL, 2L, 1L))
expect_identical(dim(model_simple(test_array)), c(6L, 1L))
expect_true(grepl("dropout", model_simple_layer_dropout$name))
expect_identical(model_simple_layer_dropout$rate, 0)

# lstm
model_lstm <- keras_rnn(X, Y, model_type = "lstm", tsteps = 2, n_epochs = 10)
model_lstm_layer_recurrent <- get_keras_layer(model_lstm, index = 2)
model_lstm_layer_dropout <- get_keras_layer(model_lstm, index = 3)

expect_true(rlang::inherits_all(
  model_lstm, c("keras.engine.functional.Functional", "keras.engine.training.Model")
))
expect_true(grepl("lstm", model_lstm_layer_recurrent$name))
expect_identical(model_lstm_layer_recurrent$batch_input_shape, list(NULL, 2L, 1L))
expect_identical(dim(model_lstm(test_array)), c(6L, 1L))
expect_true(grepl("dropout", model_lstm_layer_dropout$name))
expect_identical(model_lstm_layer_dropout$rate, 0)

# gru
model_gru <- keras_rnn(X, Y, model_type = "gru", tsteps = 2, n_epochs = 10)
model_gru_layer_recurrent <- get_keras_layer(model_gru, index = 2)
model_gru_layer_dropout <- get_keras_layer(model_gru, index = 3)

expect_true(rlang::inherits_all(
  model_gru, c("keras.engine.functional.Functional", "keras.engine.training.Model")
))
expect_true(grepl("gru", model_gru_layer_recurrent$name))
expect_identical(model_gru_layer_recurrent$batch_input_shape, list(NULL, 2L, 1L))
expect_identical(dim(model_gru(test_array)), c(6L, 1L))
expect_true(grepl("dropout", model_gru_layer_dropout$name))
expect_identical(model_gru_layer_dropout$rate, 0)

### "n_units"
model_simple_units <- keras_rnn(X, Y, "simple", tsteps = 2, n_epochs = 10, n_units = 10)
model_simple_units_layer_recurrent <- get_keras_layer(model_simple_units, index = 2)

expect_equivalent(model_simple, model_simple_units)
expect_identical(model_simple_units_layer_recurrent$units, 10L)
expect_equivalent(
  model_simple, keras_rnn(X, Y, "simple", tsteps = 2, n_epochs = 10L, n_units = 10L)
)
expect_equivalent(
  model_simple, keras_rnn(X, Y, "lstm", tsteps = 2, n_epochs = 10L, n_units = NULL)
)

### "n_epochs"
c(model_simple_epochs, history_simple_epochs) %<-% keras_rnn(X, Y, "simple", tsteps = 2, n_epochs = 10L, loss = "mse", metrics = NULL, history = TRUE)

expect_equivalent(model_simple, model_simple_epochs)
expect_identical(history_simple_epochs$params$epochs, 10L)
expect_equivalent(model_simple, keras_rnn(X, Y, "simple", tsteps = 2, n_epochs = NULL))

### "metrics"
c(model_simple_metrics, history_simple_metrics) %<-% keras_rnn(X, Y, "simple", tsteps = 2, n_epochs = 10L, metrics = c("mae", "mse"), history = TRUE)
expect_equivalent(model_simple, model_simple_metrics)
expect_identical(
  names(history_simple_metrics$metrics),
  c("loss", "mae", "mse", "val_loss", "val_mae", "val_mse")
)

### "loss"
expect_equivalent(
  model_simple, keras_rnn(X, Y, "simple", tsteps = 2, n_epochs = 10L, loss = "mae")
)

### "dropout_in_test"
model_simple_dropout_test <- keras_rnn(X, Y, "simple", tsteps = 2, n_epochs = 10L, dropout = 0, dropout_in_test = TRUE)
model_simple_dropout_test_layer_recurrent <- get_keras_layer(model_simple_dropout_test, index = 2)

expect_equivalent(model_simple, model_simple_dropout_test)
expect_identical(model_simple_dropout_test_layer_recurrent$dropout, 0)

expect_equivalent(
  model_simple, keras_rnn(X, Y, "simple", tsteps = 2, n_epochs = 10L, dropout_in_test = FALSE)
)

### "optimizer"
expect_equivalent(
  model_simple, keras_rnn(X, Y, "simple", tsteps = 2, n_epochs = 10L, optimizer = NULL)
)
expect_equivalent(
  model_simple, keras_rnn(X, Y, "simple", tsteps = 2, n_epochs = 10L, optimizer = optimizer_rmsprop())
)
expect_equivalent(
  model_simple, keras_rnn(X, Y, "simple", tsteps = 2, n_epochs = 10L, optimizer = optimizer_adam())
)
expect_equivalent(
  model_simple, keras_rnn(X, Y, "simple", tsteps = 2, n_epochs = 10L, optimizer = optimizer_adagrad())
)

### "dropout"
model_simple_dropout <- keras_rnn(X, Y, "simple", tsteps = 2, n_epochs = 10L, dropout = 0.5)
model_simple_dropout_layer <- get_keras_layer(model_simple_dropout, index = 3)

expect_equivalent(model_simple, model_simple_dropout)
expect_identical(model_simple_dropout_layer$rate, 0.5)

model_simple_dropout02 <- keras_rnn(X, Y, "simple", tsteps = 2, n_epochs = 10L, dropout = NULL)
model_simple_dropout02_layer <- get_keras_layer(model_simple_dropout02, index = 3)

expect_equivalent(model_simple, model_simple_dropout02)
expect_identical(model_simple_dropout02_layer$rate, 0)

### "recurrent_dropout"
model_simple_recurr_dropout <- keras_rnn(
  X, Y, "simple", tsteps = 2, n_epochs = 10L, recurrent_dropout = 0.5)
model_simple_recurr_dropout_layer <- get_keras_layer(model_simple_recurr_dropout, index = 2)
model_simple_recurr_dropout_layer_dropout <- get_keras_layer(model_simple_recurr_dropout, index = 3)

expect_equivalent(model_simple, model_simple_recurr_dropout)
expect_identical(model_simple_recurr_dropout_layer$dropout, 0)
expect_identical(model_simple_recurr_dropout_layer$recurrent_dropout, 0.5)
expect_identical(model_simple_recurr_dropout_layer_dropout$rate, 0)

model_simple_recurr_dropout02 <-
  keras_rnn(X, Y, "simple", tsteps = 2, n_epochs = 10L, recurrent_dropout = NULL)
model_simple_recurr_dropout02_layer <-
  get_keras_layer(model_simple_recurr_dropout02, index = 2)

expect_equivalent(model_simple, model_simple_recurr_dropout02)
expect_identical(model_simple_recurr_dropout02_layer$recurrent_dropout, 0)

### "live_plot"
# expect_equivalent(
#   model_simple,
#   keras_rnn(X, Y, "simple", tsteps = 2, n_epochs = 10L, live_plot = TRUE)
# )
expect_equivalent(
  model_simple, keras_rnn(X, Y, "simple", tsteps = 2, n_epochs = 10L, live_plot = FALSE)
)

# Error ------------------------------------------------------------------------

### "X"
expect_error(keras_rnn("X"), class = "keras_rnn_X_error")
expect_error(keras_rnn(X["train"]), class = "keras_rnn_X_error")

### "Y"
expect_error(keras_rnn(X, "Y"), class = "keras_rnn_Y_error")
expect_error(keras_rnn(X, Y[["test"]]), class = "keras_rnn_Y_error")

### "model_type"
expect_error(
  keras_rnn(X, Y, model_type = TRUE),
  class = "keras_rnn_model_type_error"
)
expect_error(
  keras_rnn(X, Y, model_type = "ltsm"),
  class = "rlang_error",
  pattern = "`model_type` must be one of \"simple\", \"lstm\", or \"gru\"\\."
)

### "tsteps"
expect_error(
  keras_rnn(X, Y, "simple", tsteps = TRUE),
  class = "keras_rnn_tsteps_error"
)
expect_error(
  keras_rnn(X, Y, "simple", tsteps = 1:2),
  class = "keras_rnn_tsteps_error"
)
expect_error(
  keras_rnn(X, Y, "simple", tsteps = -1),
  class = "keras_rnn_tsteps_error",
  pattern = "Dimensions in `X` must match with `tsteps`\\."
)
expect_error(
  keras_rnn(X, Y, "simple", tsteps = 0),
  class = "keras_rnn_tsteps_error",
  pattern = "Dimensions in `X` must match with `tsteps`\\."
)
expect_error(
  keras_rnn(X, Y, "simple", tsteps = -1),
  class = "keras_rnn_tsteps_error",
  pattern = "Dimensions in `X` must match with `tsteps`\\."
)

### "n_epochs"
expect_error(
  keras_rnn(X, Y, "simple", tsteps = 2, n_epochs = "20"),
  class = "keras_rnn_n_epochs_error"
)
expect_error(
  keras_rnn(X, Y, "simple", tsteps = 2, n_epochs = c(10, 20)),
  class = "keras_rnn_n_epochs_error"
)

### n_units
expect_error(
  keras_rnn(X, Y, "simple", tsteps = 2, n_units = FALSE),
  class = "keras_rnn_n_units_error"
)
expect_error(
  keras_rnn(X, Y, "simple", tsteps = 2, n_units = c(20, 30)),
  class = "keras_rnn_n_units_error"
)

### loss
expect_error(
  keras_rnn(X, Y, "simple", tsteps = 2, loss = NULL),
  class = "keras_rnn_loss_error"
)
expect_error(
  keras_rnn(X, Y, "simple", tsteps = 2, loss = c("mse", "mae")),
  class = "keras_rnn_loss_error"
)
expect_error(
  keras_rnn(X, Y, "simple", tsteps = 2, loss = c("XYZ")),
  pattern = "ValueError"
)

### metrics
expect_error(
  keras_rnn(X, Y, "simple", tsteps = 2, metrics = 12),
  class = "keras_rnn_metrics_error"
)
expect_error(
  keras_rnn(X, Y, "simple", tsteps = 2, metrics = c("XYZ")),
  pattern = "ValueError"
)

### "dropout_in_test"
expect_error(
  keras_rnn(X, Y, "simple", tsteps = 2, dropout_in_test = "Yes"),
  class = "keras_rnn_dropout_in_test_error"
)
expect_error(
  keras_rnn(X, Y, "simple", tsteps = 2, dropout_in_test = NULL),
  class = "keras_rnn_dropout_in_test_error"
)

### "optimizer"
expect_error(
  keras_rnn(X, Y, "simple", tsteps = 2, optimizer = "rmsprop"),
  class = "keras_rnn_optimizer_error"
)

### "dropout"
expect_error(
  keras_rnn(X, Y, "simple", tsteps = 2, dropout = "1"),
  class = "keras_rnn_dropout_error"
)
expect_error(
  keras_rnn(X, Y, "simple", tsteps = 2, dropout = -0.1),
  class = "keras_rnn_dropout_error",
  pattern = "`dropout` must be within interval \\[0, 1\\]\\."
)
expect_error(
  keras_rnn(X, Y, "simple", tsteps = 2, dropout = 1.01),
  class = "keras_rnn_dropout_error",
  pattern = "`dropout` must be within interval \\[0, 1\\]\\."
)

### "recurrent_dropout"
expect_error(
  keras_rnn(X, Y, "simple", tsteps = 2, recurrent_dropout = FALSE),
  class = "keras_rnn_recurrent_dropout_error"
)
expect_error(
  keras_rnn(X, Y, "simple", tsteps = 2, recurrent_dropout = -0.1),
  class = "keras_rnn_recurrent_dropout_error",
  pattern = "`recurrent_dropout` must be within interval \\[0, 1\\]\\."
)
expect_error(
  keras_rnn(X, Y, "simple", tsteps = 2, recurrent_dropout = 1.1),
  class = "keras_rnn_recurrent_dropout_error",
  pattern = "`recurrent_dropout` must be within interval \\[0, 1\\]\\."
)

### "history"
expect_error(
  keras_rnn(X, Y, "simple", tsteps = 2, history = "TRUE"),
  class = "keras_rnn_history_error"
)

### "live_plot"
expect_error(
  keras_rnn(X, Y, "simple", tsteps = 2, live_plot = "FALSE"),
  class = "keras_rnn_live_plot_error"
)
