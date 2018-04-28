
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
    
    ### INPUT CHECKS ##########################################################
    
    if( !( is.numeric(t) && t > 0 ) ) {
        stop('t must be a positive number.');
    }
    
    
    ### CALCULATE WEIGHTS #####################################################
    
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