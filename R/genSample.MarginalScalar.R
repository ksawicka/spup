#' Generating Monte Carlo sample from an uncertain object of a class 
#' 'MarginalScalar'
#'
#' Function that runs Monte Carlo simulations for MarginalScalar class objects.
#'
#' \strong{"stratifiedSampling"} Number of samples (n) must be dividable by the
#' number of quantiles to assure each quantile is evenly represented.
#'
#' @usage genSample(UMobject, n, samplemethod, p = 0, ...)
#'
#' @param UMobject uncertain object defined using defineUM().
#' @param n Integer. Number of Monte Carlo realizations.
#' @param samplemethod "randomSampling" or "stratifiedSampling".
#' @param p A vector of quantiles. Optional. Only required if sample method is
#' "stratifiedSampling".
#' @param ...  Additional parameters. 
#' 
#' @return A Monte Carlo sample of uncertain input of a class of distribution
#' parameters.
#' 
#' @author Kasia Sawicka
#' 
#' @examples
#'
#' # Example 1
#' scalarUM <- defineUM(uncertain = TRUE, distribution = "norm", distr_param = c(10, 1))
#' scalar_sample <- genSample(scalarUM, n = 10, samplemethod = "randomSampling")
#' 
#' # Example 2
#' scalarUM <- defineUM(uncertain = TRUE, distribution = "beta", distr_param = c(10, 1, 2))
#' scalar_sample <- genSample(scalarUM, n = 10, samplemethod = "stratifiedSampling", p = 0:5/5)
#'
#' @export
genSample.MarginalScalar <- function(UMobject, n, samplemethod, p = 0, asList = TRUE, ...) {
  
  stopifnot(UMobject$uncertain == TRUE)
  
  # extract information from UMobject
  distribution <- UMobject[[2]]
  distr_param <- UMobject[[3]]
  
  ### RANDOM SAMP ---------------------------------------------------------------------
  if (samplemethod == "randomSampling") {
    
    # use distribution_sampling.R to sample
    X_sample <- distribution_sampling(n, distribution, distr_param)
    
    # sort out names
    if (!is.null(UMobject$id)) {
      names(X_sample) <- paste(UMobject$id, ".sim", c(1:n), sep = "")
    } else {
      names(X_sample) <- paste("sim", c(1:n), sep = "")}
  }
  
  ### STRATIFIED SAMP ------------------------------------------------------------------
  if (samplemethod == "stratifiedSampling") {
    if (n %% (length(p)-1) != 0)
      stop("n should be divisable by the number of strata")
      
    # function to call stratsamp.R
    stsS <- function(x, ...) {
        parameters <- x
        as.numeric(stratsamp(n = n/(length(p)-1), distribution, parameters, p, ...))
    }
    X_sample <- stsS(distr_param)
    
    # sort out names
    if (!is.null(UMobject$id)) {
      names(X_sample) <- paste(UMobject$id, ".sim", c(1:n), sep = "")
    } else {
      names(X_sample) <- paste("sim", c(1:n), sep = "")}
 
  }
  X_sample
} 

