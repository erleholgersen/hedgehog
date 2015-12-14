# Helper functions with no clear category

factor.to.numeric <- function(x) {
  return(as.numeric(as.character(x)))
}

interleave <- function(x, y) {
  min_length = min(length(x), length(y))

  interleaved = as.numeric(rbind(x[1:min_length], y[1:min_length]))

  if(length(x) > min_length) {
    interleaved = c(interleaved, x)
  } else if(length(y) > min_length) {
    interleaved = c(interleaved, y)
  }

  return(interleaved)
}
