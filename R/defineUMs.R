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
#' @param distr_param A vector or a list with distribution parameters. For example, for 
#' normal distribution in spatial variable this must be a map of means and a map
#' of syandard deviations.
#' @param cat_prob A list of probabilities for categorical variables. In case of spatial inputs,
#' these must be maps. In case of non-spatial inputs it is a data.frame.
#' @param crm A correlogram model, object of a class "SpatialCorrelogramModel",
#' output of makecormodel(). Can only be specified for numerical variables.
#' @param ...  Additional parameters of um that may want to be saved here.
#'
#' @return A list of all necessary information for creating realizations of
#' the uncertain variable.
#' 
#' @author Kasia Sawicka, Gerard Heuvelink
#' 
#' @examples
#' 
#' data(DEM)
#' dem_crm <- makecrm(acf0 = 0.78, range = 321, model = "Exp")
#' demUM <- defineUMs(uncertain = TRUE, distribution = "norm",
#'                    distr_param = c(dem30m, dem30m_sd), crm = dem_crm)
#' 
#' @export
#' 
defineUMs <- function(uncertain = TRUE, distribution, distr_param, cat_prob = NULL, crm = NULL, ...) {
  
  if (class(uncertain) != "logical")
    stop("uncertain must be logical")
  
  if (is.null(distr_param) & is.null(cat_prob))
    stop("One of 'dist_param' or 'cat_prob' must be provided.")
  if (!is.null(distr_param) & !is.null(cat_prob))
    stop("Only one of 'dist_param' or 'cat_prob' can be provided.")
  
  # recognise if it is a continuous or categorical variable
  # if it is continuous:
  if(!is.null(distr_param)) {
    # if dist_param are the same class
    if (class(distr_param[[1]]) != class(distr_param[[2]]))
      stop("Distribution parameters must be objects of the same class.")
    # if distribution is not null, a string, and belongs to the list of supported distributions
    if (is.null(distribution))
      stop("Distribution type is missing.")
    if (class(distribution) != "character")
      stop("Distribution type must be 'string'.")
    um <- list(uncertain = uncertain,
               distribution = distribution,
               distr_param = distr_param, 
               crm = crm,
               ...)  
  }  
  if (check_if_Spatial(distr_param[[1]])) 
    class(um) <- "NumMarSpatial" 
  else if (class(distr_param[[1]]) == "numeric") 
    class(um) <- "NumMarSkalar"
  else 
    stop("Class of distribution parameters is not supported.")
  # Add time series.
  
  # if it is categorical:
  if (!is.null(cat_prob)) {
    # here some checks on the categorical objects?
    um <- list(uncertain = uncertain,
               cat_prob <- cat_prob,
               ...)
  }
  if (check_if_Spatial(cat_prob)) # here need to decide what class we allow for sptial, e.g raster stack? what is it is polygons?
    class(um) <- "CatSpatial"  
  else
    class(um) <- "CatDf" # check if all the brackets are correact, maybe order of ifs need to change.
  # check where should be stop saying Class not allowed, like in line 90.
  # maybe we need this only in one place?
  
  um
} 

