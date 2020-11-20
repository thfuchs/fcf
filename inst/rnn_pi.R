DT <- fcf::DT_unh

cv_setting <- list(
  periods_train = 40,
  periods_val = 6,
  periods_test = 6,
  skip_span = 5
)
multiple_h <- list(short = 1, medium = 1:4, long = 5:6, total = 1:6)
frequency <- 4

n_train <- cv_setting$periods_train
n_val <- cv_setting$periods_val
n_initial <- n_train + n_val
n_test <- cv_setting$periods_test

# Normalization
c(data, metrics_norm) %<-% ts_normalization(DT, n_val, n_test, metrics = TRUE)

c(X, Y) %<-% ts_nn_preparation(
  data,
  lag_setting = 1:4,
  length_val = 6,
  length_test = n_test
)

model <- keras_sequential(
  X, Y,
  model_type = "simple",
  tsteps = 4,
  n_epochs = 100,
  dropout_in_test = TRUE,
  dropout = 0.2,
  recurrent_dropout = 0
)

reticulate::source_python(file = "inst/create_dropout_model.py")

model_new <- create_dropout_model(model, 0.1, 0.1)

## Predict
predict(model_new, X$test) * metrics_norm$scale + metrics_norm$center
