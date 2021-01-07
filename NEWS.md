## tsRNN 0.1.4

### Important Fix
* `predict_baselines`, `predict_arima`, `tune_keras_rnn_bayesoptim`, `tune_keras_rnn_predict`  
  All functions checked id column for class numeric. Fixed bug to check for class character

### Minor Changes
* `tune_keras_rnn_bayesoptim` Change bayes optimization iterations from 50 to 30
* `tune_keras_rnn_bayesoptim` and `tune_keras_rnn_predict`: Fix resample naming once purrr loop finished

## tsRNN 0.1.3

Splitting `tune_keras_rnn`

### New Features
* `tune_keras_rnn_bayesoptim`  
  tuning process by Bayesian Optimization
* `tune_keras_rnn_predict`  
  Train and forecast based on tuning parameters
* `tune_keras_rnn_eval`  
  evaluation of trained models

## tsRNN 0.1.2

### Minor Fixes
* `tune_keras_rnn` development of grid search for PI dropout rate finished
* `py_dropout_model` slight bug fixed
* `keras_rnn` enhanced by "optimizer" argument

## tsRNN 0.1.1

### Minor Fixes
* `predict_arima` and `predict_baselines`: Now using `stats::window` for `subset.ts`

# tsRNN 0.1.0

First (official) release of package tsRNN (former fcf) for time series recurrent neural network training and estimation

### New Features
* `check_acf`  
  Check for autocorrelation (Ljung-Box tests for 8, 12 and 16 lags)
* `forecast_baseline`  
  Baseline models to get benchmark results for advanced models
* `keras_rnn`  
  Train Recurrent Neural Network with Gated Recurrent Unit (GRU) or Long-Short Term Memory (LSTM) using Keras framework
* `acd`  
  Absolute coverage difference (ACD)
* `smis`  
  Scaled Mean Interval Score
* `mase`  
  Mean Absolute Scaled Error (MASE)
* `mape`  
  Mean Absolute Percentage Error (MAPE)
* `smape`  
  symmetric Mean Absolute Percentage Error (sMAPE)
* `plot_baselines`  
  Plot Forecasts by baseline methods
* `plot_baselines_samples`  
  Plot cross validated samples of forecasts by baseline methods
* `plot_prediction`  
  Plot timeseries and forecast for single split and company
* `plot_prediction_samples`  
  Plot multiple splits from list with forecast results
* `predict_arima`  
  Forecast the next n steps by ARIMA for each split
* `predict_baselines`  
  Prediction and evaluation for baseline models including cross validation
* `predict_keras_rnn`  
  Prediction and evaluation for rnn models
* `py_dropout_model`  
  Change dropout rate in recurrent layer or dropout layer of trained model (python wrapper)
* `ts_nn_preparation`  
  Timeseries data preparation for neural network Keras models
* `ts_normalization`  
  Normalize univariate timeseries
* `tune_keras_rnn`  
  Tune recurrent neural network with Keras functional API and Bayes-Optimization and select best performing model
