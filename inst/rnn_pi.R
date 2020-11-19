object <- arima
h <- n_test
level <- 95
nint <- length(level)
npaths <- 200
# origxreg <- NULL
# lambda <- NULL

fc_mean <- apply(fc_x10, 1, mean)
fc_lower <- apply(fc_x10, 1, quantile, 0.5 - level / 200, type = 8)
fc_upper <- apply(fc_x10, 1, quantile, 0.5 + level / 200, type = 8)

if (bootstrap) # Compute prediction intervals using simulations
{
  sim <- matrix(NA, nrow = npaths, ncol = h)
  for (i in 1:npaths) {
    sim[i, ] <- simulate(object, nsim = h, bootstrap = TRUE, xreg = NULL, lambda = NULL)
  }
  lower <- apply(sim, 2, quantile, 0.5 - level / 200, type = 8)
  upper <- apply(sim, 2, quantile, 0.5 + level / 200, type = 8)
  if (nint > 1L) {
    lower <- t(lower)
    upper <- t(upper)
  } else {
    lower <- matrix(lower, ncol = 1)
    upper <- matrix(upper, ncol = 1)
  }
}

