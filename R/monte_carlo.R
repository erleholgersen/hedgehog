# Helper functions for Monte Carlo methods.

pathplot <- function(S, T2 = 1) {
  # function for plotting Monte Carlo simulated price paths
  # S matrix of paths, with each row containing one path
  # T2 last time, largest value on x-axes.

  x = T2*(1:ncol(S))/ncol(S)
  plot(x, S[1,], type="l", ylim=c(min(S), max(S)), ylab="Price", xlab="Time")

  for(i in 2:nrow(S)) {
    lines(x, S[i,])
  }
}

confidence_interval <- function(x, alpha=0.05) {
  n = length(x)
  sd = sqrt(var(x)/n)
  z = qnorm(1 - alpha/2)
  return(c(mean(x) - z*sd, mean(x) + z*sd))
}

geometric_mean <- function(numbers) {
  n = length(numbers)
  return(prod(numbers)^(1/n))
}


rnorm_correlated <- function(n, rho) {
  # generates samples from two correlated standard normal variables
  # n number of samples to be generated from each variable.
  # rho correlation between variables

  n_assets = 2

  dW = rnorm(n_assets*n)
  dim(dW) = c(n, n_assets)

  correlation_matrix = array(rho, dim = c(n_assets, n_assets))
  diag(correlation_matrix) = 1

  cholesky_factor = chol(correlation_matrix)

  dW = dW %*% cholesky_factor

  return(dW)
}


lhs <- function(s, n) {
  # Latin Hypercube Sampling
  # s number of dimensions
  # n number of points in each dimension

  minimums = 0:(n - 1)/n
  maximums = 1:n/n

  X = runif(s*n, minimums, maximums)
  dim(X) = c(n, s)

  # randomly reorder columns independently
  for(i in 1:s) {
    row_order = sample(1:n, n)
    X[,i] = X[row_order, i]
  }
  return(X)
}

lhs_rnorm_correlated <- function(n, rho) {
  # generates samples from two correlated standard normal variables with Latin Hypercube Sampling
  # n number of samples to be generated from each variable.
  # rho correlation between variables

  n_assets = 2

  uniforms = lhs(n_assets, n)

  dW = qnorm(uniforms)
  correlation_matrix = array(rho, dim = c(n_assets, n_assets))
  diag(correlation_matrix) = 1

  cholesky_factor = chol(correlation_matrix)

  dW = dW %*% cholesky_factor

  return(dW)

}
