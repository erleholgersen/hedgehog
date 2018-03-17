
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
    censoring.surv.object <- survival::Surv(
        survival.data[, 'time'], 
        censoring.status == survival.data[, 'status']
    );
    
    # fit non-parametric estimate
    # if adding covariates, this needs to change!
    censoring.km.fit <- survival::survfit(
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
