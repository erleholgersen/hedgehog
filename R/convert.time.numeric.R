#' convert.time.numeric
#' 
#' @description 
#' Convert a time in XX:YY format to a numeric value. For example, 12:30 gets converted to 12.5.
#'
#' @param x Times to be converted
#'
#' @return vector of length \code{x} containing corresponding numeric values
#' 
#' @export convert.time.numeric
convert.time.numeric <- function(x) {
    
    ### INPUT TESTS ###########################################################
    
    if( !all( grepl('^(\\s+)?(\\d{1,2}):(\\d{1,2})(\\s+)?$', x) ) ) {
        stop('All times must be in XX:YY format');
    }
    
    ### MAIN ##################################################################
    
    # extract components
    hours <- gsub('(\\d{1,2}):(\\d{1,2})', '\\1', x);
    minutes <-  gsub('(\\d{1,2}):(\\d{1,2})', '\\2', x);

    # convert to number
    numeric.times <- as.numeric(hours) + as.numeric(minutes)/60;
    
    return(numeric.times);
}