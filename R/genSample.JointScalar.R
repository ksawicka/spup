#' Generating sample from cross-correlated variables described by skalar.
#'
#' @usage genSample(UMobject, n, samplemethod, p = 0, asList = TRUE, ...)
#'
#' @param UMobject object of a class JointScalar created using defineMUM.R
#' @param n Integer; number of Monte Carlo runs
#' @param samplemethod "randomSampling" or "lhs".
#' @param p A vector of quantiles. Optional. Only required if sample method is "lhs".
#' @param asList Logical. If asList = TRUE returns list of all samples as a list. 
#' If asList = FALSE returns samples in a format of 
#' @param ... Additional parameters.
#'
#' @return Monte Carlo sample of cross-correlated scalar variables
#'
#' @examples
#' 
#' scalarUM <- defineUM(uncertain = TRUE, distribution = "norm", distr_param = c(1, 2))                
#' scalarUM2 <- defineUM(uncertain = TRUE, distribution = "norm", distr_param = c(3, 2))
#' scalarUM3 <- defineUM(uncertain = TRUE, distribution = "norm", distr_param = c(10, 2.5))                
#' myMUM <- defineMUM(UMlist = list(scalarUM, scalarUM2, scalarUM3), 
#'                matrix(c(1,0.7,0.2,0.7,1,0.5,0.2,0.5,1), nrow = 3, ncol = 3))
#' my_sample <- genSample(myMUM, n = 10, samplemethod = "randomSampling", asList=F)
#' str(my_sample)  
#' 
#' @export
genSample.JointScalar <- function(UMobject, n, samplemethod, p = 0, asList = TRUE, ...) {
  
  # Calculate variance-covariance matrix VarCov
  vector_of_means <- c()
  vector_of_sd <- c()
  for (i in 1:length(UMobject[[1]])) {
    vector_of_means[i] <- UMobject[[1]][[i]]$distr_param[1]
    vector_of_sd[i] <- UMobject[[1]][[i]]$distr_param[2]
  }
  cormatrix <- UMobject[[2]]
  VarCov <- varcov(vector_of_sd, cormatrix)
  
  if (samplemethod == "randomSampling") {
    Cross_sample <- mvtnorm::rmvnorm(n, vector_of_means, VarCov)
    if (asList) {
      l <- list()
      for (i in 1:ncol(Cross_sample)) {
        l_i <- Cross_sample[, i]
        l[[i]] <- map(1:n, function(x){l_i[x]})
      }
      Cross_sample <- l
    }
  }
    
  if (samplemethod == "lhs") {
    print("Here code for LHS sampling")
  }
 
  Cross_sample
}


