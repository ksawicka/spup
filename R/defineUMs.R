#' Creating an uncertainty model for a single input.
#'
#' Function that allows user to define marginal uncertainty distributions 
#' for model inputs and subsequent Monte Carlo analysis.
#'
#' \strong{uncertain} If "TRUE" the uncertainty model for the input has to be
#' specified. If uncertain ="FALSE" the function requires a mean value of a
#' distribution, e.g. a scalar, a vector, or a map.
#'
#' The spatial object must contain a map of mean and standard deviation. If crm
#' is provided and spatial correlation between the residuals is assumed only
#' the normal distribution of residuals is allowed.
#'
#' If no spatial correlations between residuals is assumed, allowed
#' distributions for marginal uncertainty models are listed in Table 1.
#'
#'
#'
#' Table 1 Parametric probability models allowed in defineUM.
#' \tabular{rlllll}{ \tab \strong{Numeric type} \tab \strong{Distribution} \tab
#' \strong{Syntax} \tab \strong{Parameters} \tab \strong{Description} \cr \tab
#' Continuous \tab Beta \tab "beta" \tab c(\eqn{\alpha}, \eqn{\beta}) \tab
#' Shape parameters \eqn{\alpha} > 0, \eqn{\beta} > 0 \cr \tab Continuous \tab
#' Cauchy \tab \tab \tab \cr \tab Continuous \tab Chi-squared \tab \tab \tab
#' \cr \tab Continuous \tab Continuous uniform \tab \tab \tab \cr \tab
#' Continuous \tab Exponential \tab \tab \tab \cr \tab Continuous \tab Gamma
#' \tab \tab \tab \cr \tab Continuous \tab \tab \tab \tab
#'
#' }
#'
#' @usage defnummarspatial(uncertain = TRUE, sp.obj = NULL, crm = NULL, mask =
#' NULL, ...)
#'
#' @param uncertain "TRUE" or "FALSE", determines if specification of
#' Uncertainty Model (UM) is needed.
#' @param distribution A string specified which distribution to sample from.
#' See Details for a list of supported distributions.
#' @param distr_param A vector with distribution parameters. For example, for 
#' normal distribution in spatial variable this must be a map of means and a map
#' of syandard deviations.
#' @param cat_prob A list of probabilities for categorical variables. In case of spatial inputs,
#' these must be maps. In case of non-spatial inputs it is a data.frame.
#' @param crm A correlogram model, object of a class "SpatialCorrelogramModel",
#' output of makecormodel().
#' @param ...  Additional parameters of um that may want to be saved here.
#'
#' @return A list of all necessary information for creating realizations of
#' the uncertain variable.
#' 
#' @author Kasia Sawicka, Gerard Heuvelink
#' 
#' @examples
#' 
#' @export
#' 
defineUMs <- function(uncertain = TRUE, distribution, distr_param, cat_prob, crm = NULL, ...) {
                             # we need an argument here which specifies if this objcect is
                             # cross-correlated with any other input to the model.

  if (class(uncertain) != "logical")
    stop("uncertain must be logical")
  if (is.null(distr_param & cat_prob))
    stop("One of 'dist_param' or 'cat_prob' must be provided.")
  
  
  # recognise if it is a continuous or categorical variable
  
  
  # if distribution is not null, a string, and belongs to the list of supported distributions

  # if dist_param are provided, what class they are
  
# 1. Marginal spatial continuous or discrite (numerical) correlated or no correlated
  

  
  if (!is.null(sp.obj)) { # CHECK: if sp. obj is provided
    allSpatialObjects <- c("SpatialGridDataFrame") # look up vgm() for how to access list existing elsewhere.
    if (class(sp.obj) %in% allSpatialObjects) { # CHECK: if sp.obj is ov required type
      if (is.null(sp.obj@data)) {   # CHECK: if sp.object contains data - then warning if not instead of break
        stop("SpatialObject has to contain data")
      }
    } else {
      stop("sp.obj has to be of class SpatialGridDataFrame")
    }
  } else {
    stop("sp.obj is missing")
  }
  
  um <- list(uncertain = uncertain,
             distribution = distribution,
             distr_param = distr_param,
             crm = crm,
             ...)
  
  class(um) <- "NumMarSpatial"
  um
  
# 2. Marginal spatial categorical (correlated or nor correlated?)
  
# 3. Marginal time series correlated or not correlated
  
# 4. Marginal skalar
  

} 

