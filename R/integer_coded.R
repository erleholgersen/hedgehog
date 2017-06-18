#' Integer code a vector
#' 
#' @description Convert a vector to a vector of integers, where each integer corresponds to a unique value in the original vector. Shorthand for as.numeric(factor(x, levels = unique(x)))
#' 
#' @param x vector to be converted
#' 
#' @return recoded.vector integer coded vector
#'
#' @export integer.coded
integer.coded <- function(x) {
    
    recoded.vector <- as.numeric(
        factor(
            x,
            # specify levels to make sure coding is done in order of first appearance.
            levels = unique(x)
            )
        );
    
    return(recoded.vector);
}