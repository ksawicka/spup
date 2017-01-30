#' Sampling from a given distribution
#' 
#' Only used in samplemethod "randomSampling" for MarginalNumericSpatial.
#'
#' @param n number of observations
#' @param distribution A string describing selected distribution. The same as a part of
#' the string following the "r" in each random variate generation function in ?distributions.
#' @param parameters parameters to pass to the random variate generation function after
#' number of observations.
#'
#' @return sample of random deviates
#'
distribution_sampling_raster <- function(distribution, parameters_stack) {
  if (distribution == "beta") {
    outstack <- raster::overlay(parameters_stack, 
                                fun = function(shape1, shape2, ncp) Vectorize(rbeta(shape1, shape2, ncp)))
  } else if (distribution == "binom") {
    outstack <- raster::overlay(parameters_stack, fun = function(size, prob) Vectorize(rbinom(size, prob)))
  } else if (distribution == "cauchy") {
    outstack <- raster::overlay(parameters_stack, fun = function(location, scale) Vectorize(rcauchy(location, scale)))
  } else if (distribution == "chisq") {
    outstack <- raster::overlay(parameters_stack, fun = function(df, ncp) Vectorize(rchisq(df, ncp)))
  } else if (distribution == "exp") {
    outstack <- raster::overlay(parameters_stack, fun = function(rate) Vectorize(rexp(rate)))
  } else if (distribution == "gamma") {
    outstack <- raster::overlay(parameters_stack, fun = function(shape, rate) Vectorize(rgamma(shape, rate)))
  } else if (distribution == "geom") {
    outstack <- raster::overlay(parameters_stack, fun = function(prob) Vectorize(rgeom(prob)))
  } else if (distribution == "hyper") {
    outstack <- raster::overlay(parameters_stack, fun = function(m, n, k) Vectorize(rhyper(m, n, k)))
  } else if (distribution == "lnorm") {
    outstack <- raster::overlay(parameters_stack, fun = function(meanlog, sdlog) Vectorize(rlnorm(meanlog, sdlog)))
  } else if (distribution == "logis") {
    outstack <- raster::overlay(parameters_stack, fun = function(location, scale) Vectorize(rlogis(location, scale)))
  } else if (distribution == "nbinom") {
    outstack <- raster::overlay(parameters_stack, fun = function(size, prob, mu) Vectorize(rnbinom(size, prob, mu)))
  } else if (distribution == "norm") {
    outstack <- raster::overlay(parameters_stack, fun = function(mean, sd) Vectorize(rnorm(mean, sd)))
  } else if (distribution == "pois") {
    outstack <- raster::overlay(parameters_stack, fun = function(lambda) Vectorize(rpois(lambda)))
  } else if (distribution == "t") {
    outstack <- raster::overlay(parameters_stack, fun = function(df, ncp) Vectorize(rt(df, ncp)))
  } else if (distribution == "unif") {
    outstack <- raster::overlay(parameters_stack, fun = function(min, max) Vectorize(runif(min, max)))
  } else if (distribution == "weibull") {
    outstack <- raster::overlay(parameters_stack, fun = function(shape, scale) Vectorize(rweibull(shape, scale)))
  }
  outstack 
}












