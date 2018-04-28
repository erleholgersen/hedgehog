#' Plot Kaplan-Meier curves.
#' 
#' @param km.formula Formula to be plotted
#' @param dataset Dataframe
#' @param show.risk.table Boolean indicating whether to show at risk data
#' @param col Line colour
#' @param ... Other parameters to be passed to \code{plot.survfit}
#' 
#' @return None
#' 
#' @author Erle Holgersen <Erle.Holgersen@icr.ac.uk>
#' 
#' @export
km.plot <- function(
    km.formula, 
    dataset, 
    show.risk.table = FALSE, 
    col = NULL,
    ...
    ) {
  
  # TO DO: check input
  
  # fit KM curves
  km.fit <- survival::survfit(
    km.formula,
    dataset
    );
  
  # format x-axis
  x.max <- max( dataset[, all.vars(km.formula)[1]]);
  
  ### BUILD PLOT ##############################################################
  
  strata.labels <- unique(summary(km.fit)$strata);
  
  strata.widths <- vapply(
     as.character(strata.labels),
     FUN = graphics::strwidth, 
     units = 'inches',
     FUN.VALUE = 0
     );
  
  # set margins
  mai <- c(1, 0.5, 0.82, 0.42);
  if(show.risk.table) {
      mai <- c(
          0.8 + 0.2*length(strata.labels), 
          0.4 + 1.1*max(strata.widths), 
          0.82, 
          0.42
          );
  }
  
  graphics::par(
      mai = mai
      );
  
  if( is.null(col) ) {
      col <- get.colour.palette( length(strata.labels) );
  }
  
  graphics::plot(
      km.fit, 
      col = col,
      mark.time = TRUE,
      ...
      );
  
  if(show.risk.table) {
    # get times to show risk set for
    risk.times <- seq(0, x.max, length.out = 5);
    
    # get risk set data
    risk.data <- summary(
      km.fit, 
      times = risk.times
      );
    
    unique.strata <- unique(risk.data$strata);
    
    n.risk <- split(risk.data$n.risk, risk.data$strata);
    
    for(i in seq_along(n.risk) ) {
        strata <- names(n.risk)[i];

        # pad with zeroes at the end
        strata.risk <- c(
            n.risk[[ i ]],
            rep(0, length(risk.times) - length(n.risk[[ i ]] ))
            );
    
        graphics::mtext(
            strata,
            side = 1,
            at = -0.2*risk.times[2],
            adj = 1,
            line = 2 + i, 
            font = 2,
            );
        
        graphics::mtext(
            strata.risk,
            side = 1,
            at = risk.times,
            line = 2 + i
            );
        
        } # end strata loop
    
  } # end if statement
  
  if( length(strata.labels) > 1 ) {
      
      graphics::legend(
          'bottomleft',
          legend = strata.labels,
          col = col,
          lty = 1,
          bty = 'n'
          );
  }
  
}