#' Plot Kaplan-Meier curves.
#' 
#' @param km.formula Formula to be plotted.
#' @param dataset Data.
#' @param show.risk.table Boolean.
#' 
#' @export
km.plot <- function(km.formula, dataset, show.risk.table = FALSE) {
  
  # TO DO: check input
  
  # fit KM curves
  km.fit <- survfit(
    km.formula,
    dataset
    );
  
  # format x-axis
  x.max <- max( dataset[, all.vars(km.formula)[1]]);
  
  ### BUILD PLOT ##############################################################

  plot(km.fit);
  
  if(show.risk.table) {
    # get times to show risk set for
    risk.times <- seq(0, x.max, length.out = 5);
    
    # get risk set data
    risk.data <- summary(
      km.fit, 
      times = risk.times
    );
    
    # show risk table
    mtext(
      risk.data$n.risk,
      side = 1,
      at = risk.times,
      line = 3
    );
    
  }
  

  
}