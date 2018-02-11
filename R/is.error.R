#' is.error
#'
#' @description
#' Check if an object is an error. Stolen from \url{http://adv-r.had.co.nz/Exceptions-Debugging.html}.
#'
#' @param x object
#'
#' @return logical indicating if object is an error
#' 
#' @export is.error
is.error <- function(x) inherits(x, "try-error");
