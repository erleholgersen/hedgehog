#' Calculate cosine similarity between two vectors
#'
#' @param u Numeric vector
#' @param v Numeric vector
#' 
#' @return calculated cosine similarity between u and v
#' 
#' @author 
#' Erle Holgersen <Erle.Holgersen@gmail.com>
#'
#' @examples 
#' cosine.similarity(1:3, 4:6);
#'
#' @export cosine.similarity
cosine.similarity <- function(u, v) {
    return( sum(u*v)/( sqrt( sum(u^2) )*sqrt( sum(v^2) ) ) );
}