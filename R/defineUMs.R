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
#' Table 1 Parametric probability models allowed in defineUMs().
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
#' @usage defineUMs(uncertain = TRUE, distribution = NULL, distr_param = NULL, 
#'                  categories = NULL, cat_prob = NULL, crm = NULL, ...)
#'
#' @param uncertain "TRUE" or "FALSE", determines if specification of
#' Uncertainty Model (UM) is needed.
#' @param distribution A string specified which distribution to sample from.
#' See Details for a list of supported distributions.
#' @param distr_param A vector or a list with distribution parameters. For example, for 
#' normal distribution in spatial variable this must be a map of means and a map
#' of standard deviations.
#' @param crm A correlogram model, object of a class "SpatialCorrelogramModel",
#' output of makecormodel(). Can only be specified for numerical variables.
#' @param categories a vector of categories
#' @param cat_prob data frame or spatial data frame; A list of probabilities for the vector of categories. 
#' Number of columns in the data frame cannot be smaller than number of categories.
#' @param ... additional parameters
#'
#' @return A list of all necessary information for creating realizations of
#' the uncertain variable.
#' 
#' @author Kasia Sawicka, Gerard Heuvelink
#' 
#' @examples
#' 
#' # define uncertainty model for spatial numerical variable
#' data(DEM)
#' dem_crm <- makecrm(acf0 = 0.78, range = 321, model = "Exp")
#' demUM <- defineUMs(uncertain = TRUE, distribution = "norm",
#'                    distr_param = c(dem30m, dem30m_sd), crm = dem_crm)
#'                    
#' # define uncertainty model for spatial categorical variable
#' data(house)
#' houseUM <- defineUMs(uncertain = TRUE, categories = c(0.19, 0), cat_prob = houses_DF)
#' 
#' 
#' @export
#' 
defineUMs <- function(uncertain = TRUE, distribution = NULL, distr_param = NULL, crm = NULL, 
                      categories = NULL, cat_prob = NULL, ...) {
  
  if (class(uncertain) != "logical")
    stop("uncertain must be logical")
  
  if (!is.null(distr_param) & !is.null(cat_prob))
    stop("Only one of 'dist_param' or 'cat_prob' can be provided.")
  
  # recognise if it is a continuous or categorical variable
  # if it is continuous:
  if(!is.null(distr_param)) {
    a <- class(distr_param[[1]])
    if (length(distr_param) > 1) {
      for (i in 1:(length(distr_param)-1)) {
        a <- c(a, class(distr_param[[i+1]]))
      }
    }
    # if dist_param are all the same class
    if (!isTRUE(all(a == a[1])))
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
    if (check_if_Spatial(distr_param[[1]])) 
      class(um) <- "MarginalNumericSpatial" 
    else if (class(distr_param[[1]]) == "numeric") 
      class(um) <- "MarginalScalar"
    else 
      stop("Class of distribution parameters is not supported.")
    # Add time series.
  } else if (is.null(distr_param)) {
    if (is.null(categories))
      stop("Categories argument is missing.")
    um <- list(uncertain = uncertain,
               categories = categories,
               cat_prob = cat_prob,
               ...)
    if (check_if_Spatial(cat_prob)) # here need to decide what class we allow for sptial, e.g raster stack? what is it is polygons?
      class(um) <- "MarginalCategoricalSpatial"
    else
      class(um) <- "MarginalCategoricalDataFrame" 
  }  
  um
} 

