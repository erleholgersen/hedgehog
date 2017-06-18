#' Print head of an arbitrary number of obejcts.
#' 
#' 
head.cat <- function(...) {
  
  arguments <- list(...);
 
  test <- lapply(
    ..., 
    substitute
  );
  
  print(test);
  # 
  # for(i in 1:len)
  # variable.names <- deparse(substitute(...));
  
  print(variable.names);
  
  names(arguments) <- variable.names;
  
  for(variable.name in names(arguments)) {
    cat('Variable: ', variable.name, '\n');
    
    print( head(arguments[[variable.name]]) );
    
=======
#' Print head of an arbitrary number of objects.
#' 
#' @param ... objects to print head of
#' 
#' @return None
#' 
#' @examples
#' a <- 1:100;
#' b <- 50:100;
#' head.cat(a, b, letters);
#' 
#' @export head.cat
head.cat <- function(...) {
  
  # get all arguments
  arguments <- list(...);
  
  # get names of aforementioned variables
  variable.names <- as.list(match.call())[-1]; # first element in match.call is name of function
  names(arguments) <- unlist(variable.names);
  
  # loop over variables and print head. 
  for (variable.name in names(arguments)) {
    
    # show which variable is being printed
    cat('Variable:', variable.name, '\n');
    
    # print head of variable
    print( head(arguments[[variable.name]]) );
    
    # extra line space
    cat('\n');

  }
}