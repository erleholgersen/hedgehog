#' Display structure of an arbitrary number of objects.
#' 
#' @param ... objects to display structure of
#' 
#' @return None
#' 
#' @export str.cat
str.cat <- function(...) {
  
  # get all arguments
  arguments <- list(...);
  
  print(str(arguments));
  
  # get names of aforementioned variables
  variable.names <- as.list(match.call())[-1]; # first element in match.call is name of function
  names(arguments) <- unlist(variable.names);
  
  # loop over variables and print head. 
  for (variable.name in names(arguments)) {
    
    # show which variable is being printed
    cat('Variable:', variable.name, '\n');
    
    # print head of variable
    print( str(arguments[variable.name]) );
    
    # extra line space
    # cat('\n');
  }
}