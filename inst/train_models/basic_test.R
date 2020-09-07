#' Trains a simple deep NN on the Apple FCF time series
#'

library(data.table)
library(keras)


# Data Preparation ---------------------------------------------------

apple <- fcf::DT_apple
lag_setting <- 1:4

# Cross Validation
periods_train <- 70
periods_test  <- 6
skip_span     <- 7

rolling_origin_resamples <- rsample::rolling_origin(
  apple,
  initial    = periods_train,
  assess     = periods_test,
  cumulative = FALSE,
  skip       = skip_span
)

# rolling_origin_resamples
# split <- rolling_origin_resamples$splits[[1]]
#
# DT_train <- rsample::analysis(split)[1:58]
# DT_val <- rsample::analysis(split)[59:.N]
# DT_test <- rsample::assessment(split)
#
# DT <- rbind(DT_train, DT_val, DT_test)
# lag_setting <- 1:4
# length_val <- nrow(DT_val)
# length_test <- nrow(DT_test)

# Model definition, training and evaluation ------------------------------------
purrr::map(
  rolling_origin_resamples$splits,
  function(split) {
    # Train-Test Split
    DT_train <- rsample::analysis(split)[1:58]
    DT_val <- rsample::analysis(split)[59:.N]
    DT_test <- rsample::assessment(split)

    DT <- rbind(DT_train, DT_val, DT_test)
    length_val <- nrow(DT_val)
    length_test <- nrow(DT_test)

    # Normalization
    data <- ts_normalization(DT, length_val, length_test, metrics = FALSE)

    # Reshaping
    c(X, Y) %<-% ts_nn_preparation(
      data,
      lag_setting = lag_setting,
      length_val = length_val,
      length_test = length_test
    )

    keras_basic_sequential(X, Y)
  }
)
