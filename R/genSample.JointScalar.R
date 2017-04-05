#' Generating sample from cross-correlated variables described by a scalar.
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
#' @return Monte Carlo sample of cross-correlated scalar variables.
#'
#' @examples
#' 
#' scalarUM <- defineUM(uncertain = TRUE, distribution = "norm",
#'                      distr_param = c(1, 2), id="Var1")                
#' scalarUM2 <- defineUM(uncertain = TRUE, distribution = "norm",
#'                       distr_param = c(3, 2), id="Var2")
#' scalarUM3 <- defineUM(uncertain = TRUE, distribution = "norm",
#'                       distr_param = c(10, 2.5), id="Var3")                
#' myMUM <- defineMUM(UMlist = list(scalarUM, scalarUM2, scalarUM3), 
#'                matrix(c(1,0.7,0.2,0.7,1,0.5,0.2,0.5,1), nrow = 3, ncol = 3))
#' my_sample <- genSample(myMUM, n = 10, samplemethod = "randomSampling", asList=F)
#' my_sample  
#' 
#' @importFrom mvtnorm rmvnorm
#' @importFrom purrr map
#'  
#' @export
genSample.JointScalar <- function(UMobject, n, samplemethod, p = 0, asList = TRUE, ...) {
  
  # extract parameters of normal distribution for all objects
  means <- c()
  sds <- c()
  ids <- c()
  for (i in 1:length(UMobject[[1]])) {
    means[i] <- UMobject[[1]][[i]]$distr_param[1]
    sds[i] <- UMobject[[1]][[i]]$distr_param[2]
    ids[i] <- UMobject[[1]][[i]]$id
  }
  
  # extract correlation matrix from UMobject
  cormatrix <- UMobject[[2]]
  
  # calculate variance-covariance matrix VarCov
  VarCov <- varcov(sds, cormatrix)
  
  
  # ------------- method "randomSampling" --------------
  if (samplemethod == "randomSampling") {
    Cross_sample <- mvtnorm::rmvnorm(n, means, VarCov)
    colnames(Cross_sample) <- ids
    rownames(Cross_sample) <- paste("sim", 1:n, sep = "")
    
  if (asList) {
    l <- list()
    for (i in 1:ncol(Cross_sample)) {
      l_i <- Cross_sample[, i]
      l[[i]] <- purrr::map(1:n, function(x){l_i[x]})
    }
    if (!is.null(ids)) names(l) <- ids
    Cross_sample <- l
    }
  } # end sampling with "randomSampling"
  
  
  # -------------------- method "lhs" ------------------  
  if (samplemethod == "lhs") {
    print("Here code for LHS sampling")
  } # end sampling with "lhs"
  
  Cross_sample
}
