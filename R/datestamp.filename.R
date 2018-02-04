#' Generate date-stamped filename
#'
#' @param file.name core part of file name to be date stamped
#' @param extension optional extension
#' @param timestamp logical indicating whether to also add a timestamp
#'
#' @return generated file name
#' 
#' @author 
#' Erle Holgersen <Erle.Holgersen@gmail.com>
#'
#' @examples 
#' datestamp.filename('plot.png');
#' datestamp.filename('a_table', extension = 'txt');
#' datestamp.filename('plot.png', timestamp = TRUE);
#' @export datestamp.filename
datestamp.filename <- function(file.name, extension = '', timestamp = FALSE) {
    
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
      file.name
    );
    
    if( !is.null(extension) && '' != extension ) {
        stamped.file.name <- paste0(stamped.file.name, '.', extension);
    }
    
    return(stamped.file.name);
}


#' @rdname datestamp.filename
#'
#' @export date.stamp.file.name
date.stamp.file.name <- datestamp.filename;