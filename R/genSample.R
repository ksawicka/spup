#' Generic function for sampling.
#'
#' @param UMobject uncertain object
#' @param n number of Monte Carlo simulations
#' @param samplemethod sampling method
#' @param p number of strata
#' @param asList logical. If asList = TRUE returns list of all samples as a list. 
#' If asList = FALSE returns samples in a format of distribution parameters in UMobject.
#' @param ... other parameters
#'
#' @return Monte Carlo sample
#'
#' @examples
#' # examples here
#' 1+1
#' 
#' @export
genSample <- function(UMobject, n, samplemethod, p = 0, asList = TRUE, ...) {
  UseMethod("genSample")
} 
