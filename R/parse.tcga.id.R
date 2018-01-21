#' parse.tcga.id
#'
#' @description
#'  Extract a specific component from a TCGA ID. See \url{https://wiki.nci.nih.gov/display/TCGA/TCGA+barcode} for details.
#'
#' @param id TCGA ID (must start with TCGA)
#' @param field Name of field to extract
#'
#' @return Extracted component of ID
#'
#' @author Erle Holgesen <Erle.Holgersen@gmail.com>
#'
#' @export parse.tcga.id
parse.tcga.id <- function(
    id, 
    field = c('project', 'tss', 'participant', 'sample', 'vial', 'portion', 'analyte', 'plate', 'center')
    ) {
    
    field <- match.arg(field);
    
    components <- strsplit(id,  split = '-');
    
    if( 'project' == field ) {
        id.field <- sapply(components, function(x) x[1]);
    } else if( 'tss' == field ) {
        id.field <- sapply(components, function(x) x[2]);
    } else if( 'participant' == field ) {
        id.field <- sapply(components, function(x) x[3]);
    } else if( 'sample' == field ) {
        id.field <- sapply(components, function(x) substr(x[4], 1, 2));
    } else if( 'vial' == field ) {
        id.field <- sapply(components, function(x) substr(x[4], 3, 3));
    } else if( 'portion' == field ) {
        id.field <- sapply(components, function(x) substr(x[5], 1, 2));
    } else if( 'analyte' == field ) {
        id.field <- sapply(components, function(x) substr(x[5], 3, 3));
    } else if( 'plate' == field ) {
        id.field <- sapply(components, function(x) x[6]);
    } else if( 'center' == field ) {
        id.field <- sapply(components, function(x) x[7]);
    }
    
    return(id.field);
    
}
