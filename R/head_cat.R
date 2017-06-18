

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
    
  }
}