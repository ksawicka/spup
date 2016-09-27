#' Creating an uncertainty model for single continuous input
#'
#' Function that allows user to define marginal continuous or discrete
#' uncertainty distributions for inputs to a spatial environmental model and
#' subsequent Monte Carlo analysis.
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
#' @param distr_param A vector with distribution parameters.
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
#' data(Geul)
#' 
#' Geul_pb_crm <- makecrm(0.7, 300, "sph")
#' uncertain_Geul <- defineUM(uncertain = TRUE, distribution = "normal",
#'                            distr_param = c(geul.krig[1], geul.krig[2]),
#'                            crm = Geul_crm)
#' str(uncertain_Geul)
#' 
#' @export
#' 
defineUM <- function(uncertain = TRUE, distribution, distr_param, crm = NULL, ...) {
                             # we need an argument here which specifies if this objcect is
                             # cross-correlated with any other input to the model.
                             
  um <- list(uncertain = uncertain,
             distribution = distribution,
             distr_param = distr_param,
             crm = crm,
             ...)
  um
} 

