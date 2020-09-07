#' Trains a simple sequential neural network
#'

library(data.table)
library(keras)
library(tfruns)

### Function Inputs
apple <- fcf::DT_apple

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

rolling_origin_resamples
split <- rolling_origin_resamples$splits[[1]]

DT_train <- rsample::analysis(split)[1:58]
DT_val <- rsample::analysis(split)[59:.N]
DT_test <- rsample::assessment(split)

DT <- rbind(DT_train, DT_val, DT_test)
lag_setting <- 1:4
length_val <- nrow(DT_val)
length_test <- nrow(DT_test)


### Input Parameters
metrics <- list()
FLAGS <- flags(
  # tfruns::flag_boolean("stateful", FALSE),
  # tfruns::flag_boolean("stack_layers", FALSE),
  # tfruns::flag_integer("n_timesteps", n_timesteps),
  tfruns::flag_integer("n_epochs", 200),
  # tfruns::flag_numeric("dropout", 0.2),
  # tfruns::flag_numeric("recurrent_dropout", 0.2),
  tfruns::flag_string("loss", "mae"),
  tfruns::flag_string("metrics", "mse"),
  tfruns::flag_string("optimizer_type", "rmsprop"),
  tfruns::flag_integer("n_units", 32),
  tfruns::flag_numeric("lr", 0.003),
  tfruns::flag_numeric("momentum", 0.9),
  tfruns::flag_integer("patience", 10)
)
optimizer <- switch(
  FLAGS$optimizer_type,
  sgd = optimizer_sgd(lr = FLAGS$lr, momentum = FLAGS$momentum),
  rmsprop = optimizer_rmsprop()
)
callbacks <- list(
  callback_early_stopping(patience = FLAGS$patience)
)

### Preprocessing: Normalizing the data
n <- nrow(DT) - length_val - length_test
train <- DT[1:n,]

mean <- mean(train$value)
std <- sd(train$value)

data <- data.table(
  index = DT$index,
  value = scale(DT$value, center = mean, scale = std)[,1]
)

metrics$normalization <- list(center = mean, scale = std)

### Train-Validation-Test Split
c(X, Y) %<-% ts_nn_preparation(
  data,
  lag_setting = lag_setting,
  length_val = length_val,
  length_test = length_test
)

### Model
model <- keras_model_sequential() %>%
  layer_dense(units = FLAGS$n_units, activation = "relu") %>%
  layer_dense(units = 1)

model %>% compile(
  optimizer = optimizer,
  loss = FLAGS$loss,
  metrics = FLAGS$metrics
)

model %>% keras::fit(
  x               = X$train,
  y               = Y$train,
  steps_per_epoch = 1,
  epochs          = FLAGS$n_epochs,
  batch_size      = NULL,
  verbose         = 1,
  shuffle         = FALSE,
  validation_data = list(X$val, Y$val),
  callbacks       = callbacks
)
# Save Model
# if (save_model) save_model_hdf5(model, filepath, overwrite = FALSE)

# ### Accessing model performance
score <- model %>% evaluate(X$test, Y$test, verbose = 0)

cat('Test loss:', score$loss, '\n')
cat('Test accuracy:', score$mse, '\n')
#
# metrics$train <- evaluate(model, X$train, Y$train)
# metrics$val <- evaluate(model, X$val, Y$val)
# metrics$test <- evaluate(model, X$test, Y$test)
#
# ### Prediction
# pred_out <- model %>%
#   predict(X$test) %>%
#   .[,1,1]
#
# # Re-Transform values
# pred_df <- data.frame(
#   index   = tail(DT$index, length(Y$test)),
#   value   = pred_out * std + mean
# )
#
# # Combine actual data with predictions
# DT$key <- "actual"
# pred_df$key <- "predict"
#
# # Create time_bind_rows() to solve dplyr issue
# ret <- rbind(DT, pred_df) %>%
#   dplyr::arrange(key, index) %>%
#   dplyr::mutate(key = forcats::as_factor(key))
#
# return(list(predictions = ret, metrics = metrics))
