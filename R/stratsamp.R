#' Stratified sampling for spatial variables
#'
#' @param mu mean of normal distribution to be sampled
#' @param sigma sd of normal distribution to be sampled
#' @param n sample size per stratum
#' @param p vector of probs
#'
#' @return Sample of spatial variable. Matrix with n rows and length(p)-1 columns.
#' 
stratsamp <- function(n, distribution, parameters, p) {
  
  # find limeses of the strata
  lims <- find_strata(p, distribution, parameters)
  
  # initiate output matrix
  outmat <- matrix(data = NA, nrow = n, ncol = length(p)-1)
  
  counts <- rep(0, length(lims)-1)
  while (any(counts < n)) {
    r <- distribution_sampling(1, distribution, parameters)
    intvl <- findInterval(r, lims)
    if (counts[intvl] < n) {
      counts[intvl] <- counts[intvl] + 1
      outmat[counts[intvl], intvl] <- r
    }
  }
  outmat
}
