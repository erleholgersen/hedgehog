#' Generate obervations from extreme value distribution
#'
#' @param n number of observations to generate
#' @param location location parameter
#' @param scale scale parameter
#'
#' @return vector of simulated observations
#' 
#' @export
revd <- function(n, location = 0, scale = 1) {

  invF <- function(w) {
    return(-log(-log(w)))
  }

  W <- invF( runif(n) );
  Y <- location + scale*W;

  return(Y);
}

#' Get Kaplan-Meier estimate of censoring distribution at time t.
#' 
#' @param t time
#' @param survival.object survival object containing times and censoring statuses.
#' @param censoring.status status corresponding to censored observations, defaults to zero.
#' 
#' @return km.estimate 
#' 
#' @export
censoring.km.estimate <- function(t, survival.object, censoring.status = 0) {

  # TO DO: 
  #   - check input.
  #   - consider whether Surv objects can be passed in. 
  #   - implement covariates ? 

  ### INPUT CHECKS ############################################################
  
  if( !( is.numeric(t) && t > 0 ) ) {
    stop('t must be a positive number.');
  }
    
  ### MAIN LOGIC ##############################################################
  
  survival.data <- as.matrix(survival.object);
  
  # create surv object with censoring as event
  censoring.surv.object <- Surv(
    survival.data[, 'time'], 
    censoring.status == survival.data[, 'status']
    );
  
  # fit non-parametric estimate
  # if adding covariates, this needs to change!
  censoring.km.fit <- survfit(
    censoring.surv.object ~ 1
    );
  
  # Use summary of survfit object to extract KM estimate at time t.
  censoring.km.summary <- summary(
    censoring.km.fit,
    times = t
    );
  
  # extract point estimates of survival
  km.estimate <- censoring.km.summary$surv;
    
  return(km.estimate);
}


#' @title IPCW for Fine-Gray model
#' 
#' @description Calculate inverse probability of censoring weighting for Fine and Gray model.
#'
#' @param t time weights should be calculated at.
#' @param survival.object   survival object containing survival information for each patient.
#' @param censoring.status status level corresponding to censoring, defaults to 0.
#' 
#' @references Jason P. Fine and Robert J. Gray (1999). A Proportional Hazards Model for the Subdistribution of a Competing Risk. Journal of the American Statistical Association.
#' 
#' @export
fg.model.ipcw <- function(t, survival.object, censoring.status = 0) {
  
  survival.data <- as.matrix(survival.object);
  
  ### INPUT CHECKS ############################################################
  
  if( !( is.numeric(t) && t > 0 ) ) {
    stop('t must be a positive number.');
  }
  
  
  ### CALCULATE WEIGHTS #######################################################
  
  # Get r(t), indicator of knowledge of vital status of patient at time t
  r <- vital.status.known(t, survival.object);
  
  # Get KM estimate of censoring at t
  censoring.km <- censoring.km.estimate(
    t, 
    survival.object, 
    censoring.status = censoring.status
    );
  
  # Get KM estimate of censoring at min(t, X_i)
  patient.specific.censoring.km <- censoring.km.estimate(
    pmin(t, survival.data[, 'time']),
    survival.object, 
    censoring.status = censoring.status
    );
    
  weights <- r*censoring.km/patient.specific.censoring.km;
  
  return(weights);

}

#' @title Known vital status indicator
#' 
#' @description Helper function to get indicator of whether the vital status of the patient is known at time t.
#' 
#' @param t time in question.
#' @inheritParams fg.model.ipcw
#' 
#' @return known.vitals vector of same length as survival.object, indicating whether survival status of each patient is known at time t.
vital.status.known <- function(t, survival.object, censoring.status = 0) {

  # TO DO:
  #   - add check for left/ interval censoring
  
  # coerce survival object to matrix
  survival.data <- as.matrix(survival.object);
  
  # We have r(t) = I(C >= min(T, t))
  # However, since C is not observed if the patient experiences an event, we can't implement this directly.
  # 
  # Solution: 
  #   Let X denote min(T, C) (observed time)
  #   Have r(t) = I(X > t) OR (I(X < t) & status = event)

  # Build I(X < t)
  still.alive <- survival.data[, 'time'] > t;
  
  # Build (I(X < t) & status = event)
  experienced.event <- !still.alive & !( censoring.status == survival.data[, 'status'] );
    
  
  # Combine to r(t)
  known.vital.status <- still.alive | experienced.event; 
  
  return(known.vital.status);
}

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
  truncated.survival.object <- Surv(
    truncated.survival.time, 
    truncated.survival.status
    );
  
  return(truncated.survival.object);
}


