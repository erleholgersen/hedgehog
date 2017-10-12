#' Extract wrapped term from formula
#'
#'
#'
get.wrapped.term <- function(formula, wrapping) {
  
  ### INPUT CHECKS ############################################################
  
  if( !('formula' == class(formula)) ) {
    stop('Input formula must be of class formula.');
  }
  
  if(!('character' == class(wrapping)) ) {
    stop('Wrapping term must be a character string.');
  }
  
  ### MAIN LOGIC ##############################################################
  
  formula.terms <- terms(
    formula, 
    specials = wrapping
    );
  
  variables <- as.list(
    attr(
      terms(test.formula), 
      'variables'
      )
    );
  
  wrapped.variables <- variables[grepl()];
  
  return(wrapped.variables);

}