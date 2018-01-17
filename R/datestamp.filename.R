#' Generate date-stamped filename
#'
#' @param file.name core part of file name to be date stamped
#' @param extension extension
#'
#' @return generated file name
#'
#' @export datestamp.filename
datestamp.filename <- function(file.name, extension, timestamp = FALSE) {
    
    # get stamp 
    if (timestamp) {
        
        # if timestamping requested, include both date and time
        stamp <- format(
            Sys.time(),
            '%Y-%m-%d-%H:%M:%S'
            );
        
    } else {
        # by default, only include date
        stamp <- Sys.Date();
    }

    # merge components
    stamped.file.name <- paste0(
      stamp, 
      '_',
      file.name,
      '.',
      extension
    );
    
    return(stamped.file.name);
}