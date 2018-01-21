#' read.header
#' 
#' @description 
#'  Read the header of a file in table format.
#'
#' @param file Path to file
#' @param sep Field separator character
#' 
#' @return Character vector of header components
#' 
#' @author Erle Holgersen <Erle.Holgersen@gmail.com>
#' 
#' @export read.header
read.header <- function(file, sep = '\t') {
    
    ### INPUT TESTS ###########################################################
    
    if( !file.exists(file) ) {
        stop( paste('File', file, 'does not exist') );
    }
    
    ### MAIN ##################################################################
    
    header.line <- readLines(file, n = 1);
    
    header <- strsplit(header.line, split = sep)[[ 1 ]];
    
    return(header);
}