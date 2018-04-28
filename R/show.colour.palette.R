#' show.colour.palette
#'
#' @description 
#' Show colour palette in a grid
#' 
#' @param colours vector of colours
#' 
#' @author Erle Holgersen <Erle.Holgersen@gmail.com>
#' 
#' @examples 
#' show.colour.palette( get.colour.palette(7) );
#' show.colour.palette( colours() );
#'
#' @export show.colour.palette
show.colour.palette <- function(colours) {
    
    # get dimensions
    n <- length(colours);
    
    n.rows <- ceiling( sqrt(n) );
    n.cols <- ceiling( sqrt(n) );

    # make coordinate grid
    x.coordinates <- seq(
        0.5/n.rows,
        1 - 0.5/n.rows,
        length.out = n.rows
        );
    
    y.coordinates <- seq(
        0.5/n.cols,
        1 - 0.5/n.cols,
        length.out = n.cols
        );
    
    coordinate.grid <- expand.grid(x.coordinates, y.coordinates);
    
    
    # draw rectangles
    grid::grid.newpage();
    
    for( i in seq_along(colours) ) {
        grid::grid.rect(
            x = coordinate.grid[i, 1], 
            y = coordinate.grid[i, 2], 
            width = 1/n.rows, 
            height = 1/n.cols,
            gp = grid::gpar( fill = colours[i] )
            );
    }
}

#' @rdname show.colour.palette
show.color.palette <- show.colour.palette;