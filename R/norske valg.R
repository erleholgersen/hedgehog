distriktsmandat <- function(n, votes, divisors = c(1.4, seq(3, 201, by = 2))) {
  # issue: what if more mandater than sequence length?
  mandater = rep(0, length(votes))

  div_votes = votes/divisors[1]

  for(i in 1:n) {
    max_index = which.max(div_votes)
    mandater[max_index] = mandater[max_index] + 1

    div_votes[max_index] = votes[max_index]/divisors[mandater[max_index] + 1]
  }

  names(mandater) <- names(votes)
  return(mandater)
}

