
#' Make confidence interval string.
#' 
#' @param confidence.interval vector of length two giving confidence interval.
#' @param n.decimals number of decimal places to include in confidence interval.
#' 
#' @return formatted.conf.int confidence interval formatted as a string.
#' 
#' @export
conf.int.string <- function(confidence.interval, n.decimals = 3) {
  
  ### INPUT TESTS #############################################################
  if( !is.numeric(confidence.interval) ) {
    stop('confidence.interval must be a numeric vector.');
  }
  
  if( 2 != length(confidence.interval) ) {
    stop('confidence.interval must be of length 2.');
  }
  
  ### FORMAT CONFIDENCE INTERVAL ##############################################
  
  formatted.conf.int <- paste0(
    '[', 
    paste(
      round(confidence.interval, digits = n.decimals), 
      collapse = ', '
      ),
    ']'
    );
  
  return(formatted.conf.int);
  
}