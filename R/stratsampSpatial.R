#' Stratified sampling for spatial variables
#'
#' @param mu mean of normal distribution to be sampled
#' @param sigma sd of normal distribution to be sampled
#' @param n sample size per stratum
#' @param p vector of probs
#'
#' @return Sample of spatial variable. Matrix with n rows and length(p)-1 columns.
stratsampSpatial <- function(mu, sigma, n, p) {
  if (is.na(mu) || (is.na(sigma))) {
    outmat <- matrix(NA, nrow = n, ncol = length(p)-1)
  } else {
    lims <- qnorm(p, mu, sigma)
    outmat <- matrix(data = NA, nrow = n, ncol = length(p)-1)
    counts <- rep(0, length(lims)-1)
    while (any(counts < n)) {
      r <- rnorm(1, mu, sigma)
      intvl <- findInterval(r,lims)
      if (counts[intvl] < n) {
        counts[intvl] <- counts[intvl] + 1
        outmat[counts[intvl], intvl] <- r
      }
    }
  }
  return(outmat)
}