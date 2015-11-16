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

