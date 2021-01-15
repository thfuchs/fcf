suppressWarnings(suppressPackageStartupMessages(library("mockery")))
tf_v <- parse_tf_version()

# stop file if tf < 2.3.0
if (tf_v < 2.3) {
  expect_error(py_dropout_model(1), class = "py_dropout_model_tensorflow_error")
  exit_file("TensorFlow version < 2.3.0. Stop testing py_dropout_model")
}

# Intro
model_simple <- keras::load_model_hdf5(
  system.file("tinytest_data/simple_rnn_dropout.hdf5", package = "tsRNN"))
model_lstm <- keras::load_model_hdf5(
  system.file("tinytest_data/lstm_dropout.hdf5", package = "tsRNN"))
test_simple <- array(runif(12, 0.1, 3), dim = c(6L, 2L, 1L))
test_lstm <- array(runif(12, 0.1, 3), dim = c(6L, 4L, 1L))

# Deactivate dropout for testing
# a. simple
model_simple_01 <- py_dropout_model(model_simple, 0)
expect_equivalent(
  model_simple_01(test_simple),
  model_simple_01(test_simple)
)
# b. lstm
model_lstm_01 <- py_dropout_model(model_lstm, 0)
expect_equivalent(
  model_lstm_01(test_lstm),
  model_lstm_01(test_lstm)
)

# Non-applicable Dropout rate
model_err_01 <- py_dropout_model(model_simple, 1.1)
expect_error(model_err_01(test_simple), pattern = "ValueError")

model_err_02 <- py_dropout_model(model_lstm, -0.2)
expect_error(model_err_02(test_lstm), pattern = "ValueError")

### Error ----------------------------------------------------------------------
expect_error(
  py_dropout_model(model_simple, "0.5"),
  class = "py_dropout_model_dropout_error"
)
expect_error(
  py_dropout_model(1, "0.5"),
  class = "py_dropout_model_model_error"
)

# Mocking to TF version 2.2
stub(py_dropout_model, "parse_tf_version", "2.2")
expect_error(
  py_dropout_model(model_simple, 0.5),
  class = "py_dropout_model_tensorflow_error"
)
