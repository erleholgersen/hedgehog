# Helper functions for Monte Carlo methods.


#' confidence.interval
#' 
#' @description 
#'  Get the confidence interval of the mean
#'  
#'  @param x vector of numeric values
#'  @param alpha 1 minus the confidence level
#'  
#'  @return vector of length 2, containing lower and upper confidence bound
#'
confidence.interval <- function(x, alpha = 0.05) {
  n <- length(x);
  sd <- sqrt( stats::var(x)/n );
  z <- stats::qnorm(1 - alpha/2);
  
  return( c(mean(x) - z*sd, mean(x) + z*sd) );
}

#' geometric.mean
#' 
#' @description 
#'  Calculate geometric mean
#' 
#' @param x numeric vector
#' 
#' @return geometric mean
#' 
#' @author Erle Holgersen <erle.holgersen@gmail.com>
#' 
#' @export geometric.mean 
geometric.mean <- function(x) {
    
    ### INPUT TESTS ###########################################################
    
    if( !is.numeric(x) ) stop('x must be a numeric vector');
    
    ### MAIN ##################################################################
    
    n <- length(x);
    
    return( prod(x)^(1/n));
}


#' rnorm.bivariate
#' 
#' @description 
#'  Generate samples from a bivariate normal distribution based on correlation
#'
#' @param n Number of samples to be generated
#' @param rho Correlation between variables
#' 
#' @return \code{n} by 2 dimensional matrix of generated numbers
#' 
#' @author Erle Holgersen <Erle.Holgersen@gmail.com>
#' 
#' @export rnorm.bivariate
rnorm.bivariate <- function(n, rho) {
    
    n_assets <- 2;
    
    dW <- rnorm(n_assets*n);
    dim(dW) <- c(n, n_assets);
    
    correlation_matrix <- array(rho, dim = c(n_assets, n_assets));
    diag(correlation_matrix) <- 1;
    
    cholesky_factor <- chol(correlation_matrix);
    
    dW <- dW %*% cholesky_factor;
    
    return(dW);
}

#' Latin hypercube sampling
#'
#' @param s number of dimensions
#' @param n number of points in each dimension
#' 
#' @return \code{s} by \code{n} dimension matrix of generated points
#' 
#' @author Erle Holgersen <Erle.Holgersen@gmail.com>
#'
#' @export lhs
lhs <- function(s, n) {

  minimums <- 0:(n - 1)/n;
  maximums <- 1:n/n;

  X <- runif(s*n, minimums, maximums);
  dim(X) <- c(n, s);

  # randomly reorder columns independently
  for(i in 1:s) {
    row_order <- sample(1:n, n);
    X[,i] <- X[row_order, i];
  }
  return(X);
}

lhs_rnorm_correlated <- function(n, rho) {
  # generates samples from two correlated standard normal variables with Latin Hypercube Sampling
  # n number of samples to be generated from each variable.
  # rho correlation between variables

  n_assets <- 2

  uniforms <- lhs(n_assets, n)

  dW <- qnorm(uniforms);
  correlation_matrix <- array(rho, dim = c(n_assets, n_assets));
  diag(correlation_matrix) <- 1;

  cholesky_factor <- chol(correlation_matrix);

  dW <- dW %*% cholesky_factor;

  return(dW);

}



pathplot <- function(S, T2 = 1) {
    # function for plotting Monte Carlo simulated price paths
    # S matrix of paths, with each row containing one path
    # T2 last time, largest value on x-axes.
    
    x <- T2*(1:ncol(S))/ncol(S)
    graphics::plot(
        x, 
        S[1,], 
        type = 'l', 
        ylim = c( min(S), max(S) ),
        ylab = 'Price', 
        xlab = 'Time'
        );
    
    for(i in 2:nrow(S)) {
        graphics::lines(x, S[i,])
    }
}
