# apple <- fcf::DT_apple
#
# DT <- ts_normalization(
#   DT = apple, length_val =  8, length_test =  0, metrics = FALSE)
#
# # Train-Validation-Test Split
# c(X, Y) %<-% ts_nn_preparation(
#   DT,
#   lag_setting = 1,
#   length_val = 8,
#   length_test = 0
# )

# Check successful -------------------------------------------------------------

### 1. One lag
load(file.path(system.file("tinytest_data", package = "fcf"), "array_apple.rda"))

expect_silent({
  set.seed(123)
  results <- keras_basic_sequential(
    X, Y,
    n_epochs = 10,
    optimizer_type = "rmsprop",
    patience = 10,
    return_model = TRUE
  )
})

expect_identical(names(results), c("model", "train", "val", "test"))


# Error ------------------------------------------------------------------------
