#'
#' get.colour.palette
#'
#' @description Get a set of colours for plotting
#'
#' @param n Number of colours to retrieve
#' @param palette Name of palette
#'
#' @return Vector of colour HEX codes
#' 
#' @author Erle Holgersen <Erle.Holgersen@gmail.com>
#' 
#' @export get.colour.palette
get.colour.palette <- function(n, palette = c('default', 'pastel') ) {
    
    # get palette name
    palette <- match.arg(palette);
    
    colour.palettes <- list(
        'default' = c('#e41a1c', '#377eb8', '#4daf4a', '#984ea3', '#ff7f00', 
                      '#ffff33', '#a65628', '#f781bf'),
        'pastel' = c('#8dd3c7', '#ffffb3', '#bebada', '#fb8072', '#80b1d3', '#fdb462',
                     '#b3de69', '#fccde5', '#d9d9d9', '#bc80bd', '#ccebc5', '#ffed6f')
        );
    
    
    # make sure user doesn't request more colours than availbale in palette
    if( n > length(colour.palettes[[ palette ]]) ) {
        error.message <- paste(
            'Palette', palette, 
            'only contains', length(colour.palettes[[ palette ]]), 'colours'
            );
        
        stop(error.message);
    }
    
    return( colour.palettes[[ palette ]][1:n] );
    
}