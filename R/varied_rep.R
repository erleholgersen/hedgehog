#' rep function extended to support variable number of repeats for each element.
#' 
#' @param x elements to be repeated.
#' @param each  vector giving number of times each element should be repeated.
#' 
#' @return repeat.vector vector created by repeating each element.
#' 
#' @export varied.rep
varied.rep <- function(x, each) {
    
    ### INPUT TESTS ###########################################################
    
    if (!is.numeric(each) ) {
        stop('each must be numeric');
    }
    
    ### MAIN LOGIC ############################################################
    
    # use multivariate repeat function to repeat each item the specified number of times    
    repeat.list <- mapply(
        rep, 
        x, 
        each,
        simplify = FALSE
        );
    
    # coerce to vector
    repeat.vector <- unlist(repeat.list);
    names(repeat.vector) <- NULL;
    
    return(repeat.vector);
    
}