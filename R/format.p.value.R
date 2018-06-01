#' format.p.value
#' 
#' @description 
#' Format a p-value for a plot
#'
#' @param p.value p-value to be formatted
#' @param prefix prefix for p-value, defaults to "P"
#' 
#' @return language object with p-value expression
#'
#' @export format.p.value
format.p.value <- function(p.value, prefix = 'P') {
    
    if( p.value < .Machine$double.eps ) {
        p.expression <- bquote( .(prefix) < .( scientific.notation(.Machine$double.eps)) );
    } else if( p.value < 0.001) {
        p.expression <- bquote( .(prefix) == .( scientific.notation( p.value) ) );
    } else {
        p.expression <- bquote( .(prefix) == .( round(p.value, 3)) );
    }
    
    return(p.expression);
}