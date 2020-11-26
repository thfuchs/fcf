model <- keras::load_model_hdf5(system.file("tinytest_data/simple_rnn_dropout.hdf5", package = "tsRNN"))
test <- array(runif(12, 0.1, 3), dim = c(6L, 2L, 1L))

# Deactivate dropout for testing
model_01 <- py_dropout_model(model, 0)

expect_identical(
  stats::predict(model_01, test),
  stats::predict(model_01, test)
)

# Change dropout level out of range 0-1
expect_error(
  py_dropout_model(model, 1),
  pattern = "rate must be a scalar tensor or a float in the range \\[0, 1\\)"
)
