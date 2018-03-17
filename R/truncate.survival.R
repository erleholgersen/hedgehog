#' @title Truncate survival
#' 
#' @description Truncate survival times to time t.
#' 
#' @param t truncation time.
#' @inheritParams fg.model.ipcw
#' 
#' @return truncated.survival.object  survival object of same length input, containing survival information as observed at time t.
#' 
#' @export truncate.survival
truncate.survival <- function(t, survival.object, censoring.status = 0) {
  
  # TO DO:
  #   - check input
  #   - deal with left/interval censoring
  
  # coerce to data frame so we can extract time and status separately
  survival.data <- as.matrix(survival.object);
  
  # get new time, minimum of event time and truncation time
  truncated.survival.time <- pmin(
    t,
    survival.data[, 'time']
    );
  
  # get new survival status, initiate to censored.
  truncated.survival.status <- rep(
    censoring.status, 
    nrow(survival.data)
    );
  
  # update to original survival status for subjects with survival time shorter than t.
  truncated.survival.status[survival.data[, 'time'] <= t] <- survival.data[ survival.data[, 'time'] <= t, 'status'];
    
  # make new survival object
  truncated.survival.object <- survival::Surv(
    truncated.survival.time, 
    truncated.survival.status
    );
  
  return(truncated.survival.object);
}


