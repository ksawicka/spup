#' Generic function for sampling.
#'
#' @param UMobject uncertain object
#' @param n number of Monte Carlo simulations
#' @param samplemethod sampling method
#' @param p number of strata
#' @param ... other parameters
#'
#' @return Monte Carlo sample
#'
#' @examples
#' # examples here
#' 
#' @export
genSample <- function(UMobject, n, samplemethod, p = 0, asList = TRUE, ...) {
  UseMethod("genSample")
} 
