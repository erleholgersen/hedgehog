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
  
  strata.labels <- unique(summary(km.fit)$strata);
  
  strata.widths <- sapply(
     as.character(strata.labels),
     strwidth, 
     units = 'inches'
     );
  
  # set margins
  par(
      mai = c(0.8 + 0.2*length(strata.labels), 0.4 + max(strata.widths), 0.82, 0.42)
      );
  
  plot(km.fit);
  
  if(show.risk.table) {
    # get times to show risk set for
    risk.times <- seq(0, x.max, length.out = 5);
    
    # get risk set data
    risk.data <- summary(
      km.fit, 
      times = risk.times
      );
    
    print(risk.data);
    
    unique.strata <- unique(risk.data$strata);
    
    n.risk <- split(risk.data$n.risk, risk.data$strata);
    
    for(i in 1:length(n.risk) ) {
        strata <- names(n.risk)[i];
        print(strata);
        
        # pad with zeroes at the end
        strata.risk <- c(
            n.risk[[ i ]],
            rep(0, length(risk.times) - length(n.risk[[ i ]] ))
            );
        
        print(strata.risk);
        
        mtext(
            strata,
            side = 1,
            at = -0.3*risk.times[2],
            adj = 1,
            line = 2 + i, 
            font = 2,
            );
        
        mtext(
            strata.risk,
            side = 1,
            at = risk.times,
            line = 2 + i
        );
        
    }
    # show risk table

    
  }
  

  
}