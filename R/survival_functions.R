#' Generate obervations from extreme value distribution
#'
#' @param n number of observations to generate
#' @param location location parameter
#' @param scale scale parameter
#'
#' @return vector of simulated observations
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
#' @param time vector of event times
#' @param status vector of event status
#' @param censoring.status status corresponding to censored observations, defaults to zero.
#' 
#' @return km.estimate 
#' 
#' @export
censoring.km.estimate <- function(t, time, status, censoring.status = 0) {

  # TO DO: 
  #   - check input.
  #   - consider whether Surv objects can be passed in. 
  #   - implement covariates ? 

  ### INPUT CHECKS ############################################################
  
  if( !( is.numeric(t) && t > 0 ) ) {
    stop('t must be a positive number.');
  }
    
  ### MAIN LOGIC ##############################################################
  
  # create surv object with censoring as event
  censoring.surv.object <- Surv(
    time, 
    censoring.status == status
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




