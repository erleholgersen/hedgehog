#' Colour code CNAs
#' 
#' @description 
#'  Convert vector of -1, 0, 1 CNA calls to vector of colours
#' 
#' @param cna.calls vector of -1, 0, 1 coded CNAs
#' @param loss.col colour of losses, defaults to 'blue'
#' @param gain.col colour of gains, defaults to 'red'
#' @param neutral.col colour of neutral regions, defaults to 'white'
#' 
#' @return vector of colours
#' 
#' @export colour.code.cnas
colour.code.cnas <- function(cna.calls, loss.col = 'blue', gain.col = 'red', neutral.col = 'white') {
   
    colour.dictionary <- c(
        '-1' = loss.col,
        '0' = neutral.col,
        '1' = gain.col
        );
    
    colours <- colour.dictionary[as.character(cna.calls)];
    
    return(colours);
}
