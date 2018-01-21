#' Generate obervations from extreme value distribution
#'
#' @param n number of observations to generate
#' @param location location parameter
#' @param scale scale parameter
#'
#' @return vector of simulated observations
#' 
#' @export
revd <- function(n, location = 0, scale = 1) {
    
    invF <- function(w) {
        return(-log(-log(w)))
    }
    
    W <- invF( runif(n) );
    Y <- location + scale*W;
    
    return(Y);
}
