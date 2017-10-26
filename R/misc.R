# Helper functions with no clear category

#' Source directory
#' 
#' @description 
#'  Source all R scripts in a directory.
#'
#' @param directory Path to directory to be sourced
#' 
#' @return None
#' 
#' @author Erle Holgersen <Erle.Holgersen@gmail.com>
#' 
#' @export source.directory
source.directory <- function(directory) {
    r.script.paths <- list.files(
        pattern = '[.]R$', 
        path = directory, 
        full.names = TRUE
        );
    
    invisible( sapply(r.script.paths, FUN = source) );
}



#' Convert a factor to the numeric values in factor levels
#' 
#' @param x Factor to be converted
#' 
#' @return numeric.values Vector of numeric values corresponding to x
#' 
#' @author Erle Holgersen <Erle.Holgersen@gmail.com>
#' 
#' @export factor.to.numeric
factor.to.numeric <- function(x) {
    numeric.values <- as.numeric(as.character(x));
    
    return(numeric.values);
}


#' Interleave two vectors. Any overhang due to mismatching lengths is added onto the end.
#'
#' @param x First vector
#' @param y Second vector
#'
#' @return Vector of interleaved values. 
#' 
#' @author Erle Holgersen <Erle.Holgersen@gmail.com>
#'
#' @export interleave
interleave <- function(x, y) {
  min_length <- min(length(x), length(y));

  interleaved <- as.numeric(rbind(x[1:min_length], y[1:min_length]));

  if(length(x) > min_length) {
    interleaved <- c(interleaved, x);
  } else if(length(y) > min_length) {
    interleaved <- c(interleaved, y);
  }

  return(interleaved);
}
