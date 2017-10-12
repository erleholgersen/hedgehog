#' rep function extended to support variable number of repeats for each element.
#' 
#' @param x elements to be repeated.
#' @param each  vector giving number of times each element should be repeated.
#' 
#' @return Vector of repeated elements.
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
    
    # for reasons that are above me, mapply will occasionally return a matrix
    # in that case, convert to list manually
    if ('matrix' == class(repeat.list)) {
        repeat.list <- split(
            repeat.list, 
            rep(
                1:ncol(repeat.list), 
                each = nrow(repeat.list)
                )
            );
    }

    # coerce to vector
    repeat.vector <- unlist(as.list(repeat.list));
    names(repeat.vector) <- NULL;
    
    return(repeat.vector);
    
}