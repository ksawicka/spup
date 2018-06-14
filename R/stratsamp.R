#' Stratified sampling for spatial variables
#'
#' @param distribution a string, distribution type to sample from.
#' @param parameters given distribution parameters.
#' @param n sample size per stratum.
#' @param p a vector of quantiles.
#'
#' @return Sample of spatial variable. Matrix with n rows and length(p)-1 columns.
#' 
#' @author Stefan van Dam, Kasia Sawicka
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
