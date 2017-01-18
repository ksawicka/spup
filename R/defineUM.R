#' Define an uncertainty model for a single input.
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
#' Add bit about that only parametric are allowed to sample from both discrite
#' and continuos...
#'
#'
#'
#' Table 1 Parametric probability models allowed in defineUM().
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
#' @usage defineUM(uncertain = TRUE, distribution = NULL, distr_param = NULL, 
#'                  categories = NULL, cat_prob = NULL, crm = NULL,
#'                  id = NULL, cross_ids = NULL, ...)
#'
#' @param uncertain "TRUE" or "FALSE", determines if specification of
#' Uncertainty Model (UM) is needed.
#' @param id identifier of the variable; only in ude if the UM defined here 
#' is going to be used in defineUM() to construct joint UM for numerical variables.
#' @param distribution a string specified which distribution to sample from.
#' See Details for a list of supported distributions.
#' @param distr_param a vector or a list with distribution parameters. For example, for 
#' normal distribution in spatial variable this must be a map of means and a map
#' of standard deviations.
#' @param crm a correlogram model, object of a class "SpatialCorrelogramModel",
#' output of makecormodel(). Can only be specified for numerical variables.
#' @param categories a vector of categories
#' @param cat_prob data frame or spatial data frame; A list of probabilities for the vector of categories. 
#' Number of columns in the data frame cannot be smaller than number of categories.
#' @param ... additional parameters
#' @param cross_ids a vector of variables names that the defined one is cross-correlated with.
#' Only in use if a sample is to be generated from joint PDF of cross-correlated variables.
#'
#' @return Object of a class "Marginal"A list of all necessary information for creating realizations of
#' the uncertain variable.
#' 
#' @author Kasia Sawicka, Gerard Heuvelink
#' 
#' @examples
#' 
#' # define uncertainty model for spatial numerical variable
#' data(DEM)
#' dem_crm <- makecrm(acf0 = 0.78, range = 321, model = "Exp")
#' demUM <- defineUM(uncertain = TRUE, distribution = "norm",
#'                    distr_param = c(dem30m, dem30m_sd), crm = dem_crm)
#' class(demUM)
#'                    
#' # define uncertainty model for spatial categorical variable
#' data(house)
#' houseUM <- defineUM(uncertain = TRUE, categories = c(0.19, 0), cat_prob = houses_DF)
#' class(houseUM)
#' 
#' # define uncertainty model for a variable desribed by a scalar
#' scalarUM <- defineUM(uncertain = TRUE, distribution = "gamma", distr_param = c(1,2))
#' class(scalarUM)
#' 
#' # define uncertainty model for two spatial cross-correlated variables
#' data(Madagascar)
#'
#' OC_crm <- makecrm(acf0 = 0.6, range = 1000, model = "Sph")
#' OC_UM <- defineUM(TRUE, distribution = "norm", distr_param = c(OC, OC_sd), crm = OC_crm,
#' id = "OC", cross_ids = "TN")
#' class(OC_UM)
#' 
#' TN_crm <- makecrm(acf0 = 0.4, range = 1000, model = "Sph")
#' TN_UM <- defineUM(TRUE, distribution = "norm", distr_param = c(TN, TN_sd), crm = TN_crm,
#' id = "TN", cross_ids = "OC")
#' class(TN_UM)
#'   
#' @export
defineUM <- function(uncertain = TRUE, distribution = NULL, distr_param = NULL, 
                      crm = NULL, categories = NULL, cat_prob = NULL,
                      id = NULL, cross_ids = NULL, ...) {
  
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
               id = id,
               cross_ids = cross_ids,
               ...)  
    if (check_if_Spatial(distr_param[[1]])) 
      class(um) <- "MarginalNumericSpatial" 
    else if (class(distr_param[[1]]) == "numeric") 
      class(um) <- "MarginalScalar"
    else 
      stop("Class of distribution parameters is not supported.")
  } else if (is.null(distr_param)) {
    if (is.null(categories))
      stop("Categories argument is missing.")
    um <- list(uncertain = uncertain,
               categories = categories,
               cat_prob = cat_prob,
               id = id,
               cross_ids = cross_ids,
               ...)
    if (check_if_Spatial(cat_prob)) # here need to decide what class we allow for sptial, e.g raster stack? what is it is polygons?
      class(um) <- "MarginalCategoricalSpatial"
    else
      class(um) <- "MarginalCategoricalDataFrame" 
  }  
  um
} 

