#' scientific.notation
#'
#' @description 
#'  Convert a number to a scientific notation language object
#'
#' @param x Number to be converted
#' 
#' @return a language object
#' 
#' @author Erle Holgersen <Erle.Holgersen@gmail.com>
#' 
#' @export scientific.notation
scientific.notation <- function(x) {
    
    ### INPUT TESTS ###########################################################
    
    if( !is.numeric(x) ) stop('x must be a number');
    
    if( length(x) > 1) stop('x must have length at most 1');
    
    ### MAIN ##################################################################
    
    exponent <- floor( log10(x) );
    coefficient <- round(x/(10^exponent), 2);
    
    bquote.object <- bquote( .(coefficient) %*% 10^.(exponent));
    
    return(bquote.object);
}

test <- scientific.notation(4324234323);