#' Simple check if distribution provided in defineUM() belongs
#' to a list of supported distributions.
#'
#' @param object Any R object. In defineUM() it is used to
#' examine if selected distribution is in supported list of ditributions.
#'
#' @return TRUE or FALSE.
#' 
#' @author Kasia Sawicka
#' 
#'
check_distribution <- function(object) {
  Supported_ditributions <- c("beta",        
                              "cauchy",      
                              "chisq",
                              "contuni",
                              "exp",
                              "gamma",
                              "logis",
                              "lnorm",
                              "norm",
                              "weib",
                              "bern",
                              "bin",
                              # "discrpmf",
                              "discruni",
                              "geom",
                              "poiss")
  object %in% Supported_distributions
}















