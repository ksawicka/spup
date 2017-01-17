#' Generating Monte Carlo sample from a list of uncertain objects that are cross-correlated
#' and described by joint PDF or a list from independent objects using LHS method.
#'
#' \strong{"ugs"} Unconditional gaussian simulation of spatially
#' cross-correlated errors.
#'
#' \strong{"mvt"} Sampling multivariate distribution using eigenvalue decomposition
#' (based on 'mvtnorm' package).
#'
#' \strong{"lhs"} Sampling method for at least two uncertain inputs. The
#' uncertain.object is then a list of two or more. It uses startified sampling
#' method to generate the inputs for the latin hypercude algorithm, hence number of samples (n)
#' must be dividable by the number of quantiles to assure each quantile is evenly represented.
#'
#' @usage genSample(UMobject, varcov, n, samplemethod, p = 0, ...)
#'
#' @param UMobject uncertain object defined using defineUM() or a list of scalars
#' that are means of the normal marginal distributions.
#' @param varcov variance-covariance matrix; in use only if sampling joint PDF of 
#' non-spatial variables (scalars). Default is diag(ncol(x)).
#' @param n Integer. Number of Monte Carlo realizations.
#' @param samplemethod "ugs" for spatially cross-correlated errors, "mvt" for joint PDF of 
#' non-spatial variables, "lhs" if no correlation of errors is considered (can be used
#' for spatial and non-spatial variables).
#' @param p A vector of quantiles. Optional. Only required if sample method is "lhs".
#' @param ...  Additional parameters that may be passed, e.g. in
#' the "ugs" method. See examples.
#' 
#' @return A Monte Carlo sample of the variables of interest. If asList = TRUE returns
#' list of all samples as lists. 
#' 
#' @author Kasia Sawicka, Stefan van Dam, Gerard Heuvelink
#' 
#' @examples
#' 
#' # "ugs" method example
#' # load data
#' data(Madagascar)
#'
#' OC_crm <- makecrm(acf0 = 0.6, range = 1000, model = "Sph")
#' OC_UM <- defineUM(TRUE, distribution = "norm", distr_param = c(OC, OC_sd),
#' id = "OC", cross_ids = "TN")
#' class(OC_UM)
#' 
#' TN_crm <- makecrm(acf0 = 0.4, range = 1000, model = "Sph")
#' TN_UM <- defineUM(TRUE, distribution = "norm", distr_param = c(TN, TN_sd),
#' id = "TN", cross_ids = "OC")
#' class(TN_UM)
#' 
#' OC_TN_cross_crm <- makecrm()
#' 
#' 
#' # "mtv" method example
#' a <- defineUM(TRUE, "norm", c(2, 1))
#' b <- defineUM(TRUE, "norm", c(20, 5))
#' d <- defineUM(TRUE, "norm", c(10, 3))
#' my_sample <- genSample(UMobject = list(a, b, d), 
#' varcov = matrix(c(1, 1.6, 0.2, 1.6, 4, -2.4, 0.2, -2.4, 8), nrow = 3, ncol = 3),
#' n = 5, samplemethod = "mvt", asList = FALSE)
#' my_sample
#' 
#' # "lhs" method example
#' 
#' @export
genSample.list <- function(UMobject, n, samplemethod, p = 0, asList = TRUE, ...) {
  

  # distribution <- UMobject[[2]]
  # distr_param <- UMobject[[3]]
  # crm <- UMobject[[4]]
  # dots <- list(...)
  
  ### UGS ----------------------------------------------------------------------------
  if (samplemethod == "ugs") { 
    
    
    
    # if (is.null(crm))
    #   stop("Correlogram model is required for the 'ugs' sampling method.")
    # if (distribution != "norm")
    #   stop("Only normal distribution can be assumed in the 'ugs' method.")
    # for (i in 1:2) {
    #   assign(paste0("distr_param", i), distr_param[[i]])
    # }
    # mask <- distr_param1
    # g <- gstat::gstat(formula = z~1, 
    #            dummy = TRUE, beta = 0, model = crm2vgm(crm), ...)
    # epsilon_sample <- predict(object = g, newdata = mask, nsim = n)
    # X_sample <- epsilon_sample
    # # if it is Spatial data frame
    # X_sample@data <- as.data.frame(apply(as.matrix(epsilon_sample@data), MARGIN = 2,
    #                               function(x) distr_param1@data + distr_param2@data*x))
    # names(X_sample@data) <- paste("sim", c(1:n), sep = "")
    # if (asList == TRUE) {
    #   X_sample <- map(1:n, function(x){X_sample[x]})
    # }
}
    ### MVT ----------------------------------------------------------------------------
    if (samplemethod == "mvt") {
      if (is.numeric(UMobject[[1]])) {
        means <- unlist(UMobject)
      }
      if(is(UMobject[[1]], "MarginalScalar")) {
        means <- c()
        for (i in 1:length(UMobject)) {
          UMobj_i <- UMobject[[i]]
          means[i] <- UMobj_i$distr_param[1]
        }
      }
      X_sample <- mvtnorm::rmvnorm(n, means, varcov)
      if (asList) {
       l <- list()
       for (i in 1:ncol(X_sample)) {
         l_i <- X_sample[, i]
         l[[i]] <- map(1:n, function(x){l_i[x]})
       }
       X_sample <- l
       }
    }
    
    ### LHS ----------------------------------------------------------------------------
      
  X_sample
  }


