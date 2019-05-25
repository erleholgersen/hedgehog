#' Generate date-stamped filename
#'
#' @param ... core part of file name to be date stamped. If more than one part, ill be combined with \code{sep}
#' @param extension optional extension
#' @param timestamp logical indicating whether to also add a timestamp
#' @param sep separator between components of file name
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
datestamp.filename <- function(..., extension = '', timestamp = FALSE, sep = '_') {
    
    file_name = paste(c(...), collapse = sep)

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
    stamped_file_name <- paste0(stamp, '_', file_name);
    
    if( !is.null(extension) && '' != extension ) {
        stamped_file_name <- paste0(stamped_file_name, '.', extension);
    }
    
    return(stamped_file_name);
}


#' @rdname datestamp.filename
#'
#' @export date.stamp.file.name
date.stamp.file.name <- datestamp.filename;