#' Sampling from a given distribution
#'
#' @param p a vector of quantiles.
#' @param distribution a string indicating which distribution to sample from.
#' See ?defineUM() for Details.
#' @param parameters parameters to pass to the appropriate sampling funtion, e.g.
#' mean and sd for "norm" distribution.
#'
#' @return Strata of the distribution defined by given quantiles. 
#' 
#' @author Kasia Sawicka, Stefan van Dam
#' 
#' @import stats
#'
find_strata <- function(p, distribution, parameters, ...) {
  if (distribution == "beta") {
    quantiles <- do.call("qbeta", args = list(p, shape1 = parameters[[1]],
                                            shape2 = parameters[[2]], ncp = parameters[[3]], ...))
  } else if (distribution == "binom") {
    quantiles <- do.call("qbinom", args = list(p, size = parameters[[1]],
                                              prob = parameters[[2]], ...))
  } else if (distribution == "cauchy") {
    quantiles <- do.call("qcauchy", args = list(p, location = parameters[[1]],
                                              scale = parameters[[2]], ...))
  } else if (distribution == "chisq") {
    quantiles <- do.call("qchisq", args = list(p, df = parameters[[1]],
                                             ncp = parameters[[2]], ...))
  } else if (distribution == "exp") {
    quantiles <- do.call("qexp", args = list(p, rate = parameters[[1]], ...))
  } else if (distribution == "gamma") {
    quantiles <- do.call("qgamma", args = list(p, shape = parameters[[1]],
                                             rate = parameters[[2]], ...))
  } else if (distribution == "geom") {
    quantiles <- do.call("qgeom", args = list(p, prob = parameters[[1]], ...))
  } else if (distribution == "hyper") {
    quantiles <- do.call("qhyper", args = list(nn = p, m = parameters[[1]],
                                             n = parameters[[2]], k = parameters[[3]], ...))  
  } else if (distribution == "lnorm") {
    quantiles <- do.call("qlnorm", args = list(p, meanlog = parameters[[1]],
                                             sdlog = parameters[[2]], ...))
  } else if (distribution == "logis") {
    quantiles <- do.call("qlogis", args = list(p, location = parameters[[1]],
                                             scale = parameters[[2]], ...))
  } else if (distribution == "nbinom") {
    quantiles <- do.call("qnbinom", args = list(p, size = parameters[[1]],
                                              prob = parameters[[2]], mu = parameters[[3]], ...))  
  } else if (distribution == "norm") {
    quantiles <- do.call("qnorm", args = list(p, mean = parameters[[1]],
                                            sd = parameters[[2]], ...))
  } else if (distribution == "pois") {
    quantiles <- do.call("qpois", args = list(p, lambda = parameters[[1]], ...))
  } else if (distribution == "t") {
    quantiles <- do.call("qt", args = list(p, df = parameters[[1]],
                                         ncp = parameters[[2]], ...))
  } else if (distribution == "unif") {
    quantiles <- do.call("qunif", args = list(p, min = parameters[[1]],
                                            max = parameters[[2]], ...))
  } else if (distribution == "weibull") {
    quantiles <- do.call("qweibull", args = list(p, shape = parameters[[1]],
                                               scale = parameters[[2]], ...))
  }
  return(quantiles)
}