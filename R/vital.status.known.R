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
