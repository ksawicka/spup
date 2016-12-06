#' Sampling from a given distribution
#'
#' @param n number of observations
#' @param distribution A string describing selected distribution. The same as a part of
#' the string following the "r" in each random variate generation function in ?distributions.
#' @param parameters parameters to pass to the random variate generation function after
#' number of observations.
#'
#' @return sample of random deviates
#'
distribution_sampling <- function(n, distribution, parameters) {
  if (distribution == "beta") {
    samples <- do.call("rbeta", args = list(n, shape1 = parameters[[1]],
                        shape2 = parameters[[2]], ncp = parameters[[3]]))
  } else if (distribution == "binom") {
    samples <- do.call("rbinom", args = list(n, size = parameters[[1]],
                        prob = parameters[[2]]))
  } else if (distribution == "cauchy") {
    samples <- do.call("rcauchy", args = list(n, location = parameters[[1]],
                        scale = parameters[[2]]))
  } else if (distribution == "chisq") {
    samples <- do.call("rchisq", args = list(n, df = parameters[[1]],
                        ncp = parameters[[2]]))
  } else if (distribution == "exp") {
    samples <- do.call("rexp", args = list(n, rate = parameters[[1]]))
  } else if (distribution == "gamma") {
    samples <- do.call("rgamma", args = list(n, shape = parameters[[1]],
                        rate = parameters[[2]]))
  } else if (distribution == "geom") {
    samples <- do.call("rgeom", args = list(n, prob = parameters[[1]]))
  } else if (distribution == "hyper") {
    samples <- do.call("rhyper", args = list(nn = n, m = parameters[[1]],
                        n = parameters[[2]], k = parameters[[3]]))  
  } else if (distribution == "lnorm") {
    samples <- do.call("rlnorm", args = list(n, meanlog = parameters[[1]],
                        sdlog = parameters[[2]]))
  } else if (distribution == "logis") {
    samples <- do.call("rlogis", args = list(n, location = parameters[[1]],
                        scale = parameters[[2]]))
  } else if (distribution == "nbinom") {
    samples <- do.call("rnbinom", args = list(n, size = parameters[[1]],
                        prob = parameters[[2]], mu = parameters[[3]]))  
  } else if (distribution == "norm") {
    samples <- do.call("rnorm", args = list(n, mean = parameters[[1]],
                        sd = parameters[[2]]))
  } else if (distribution == "pois") {
    samples <- do.call("rpois", args = list(n, lambda = parameters[[1]]))
  } else if (distribution == "t") {
    samples <- do.call("rt", args = list(n, df = parameters[[1]],
                        ncp = parameters[[2]]))
  } else if (distribution == "unif") {
    samples <- do.call("runif", args = list(n, min = parameters[[1]],
                        max = parameters[[2]]))
  } else if (distribution == "weibull") {
    samples <- do.call("rweibull", args = list(n, shape = parameters[[1]],
                        scale = parameters[[2]]))
  }
  samples 
}











