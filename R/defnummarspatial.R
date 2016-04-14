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
#' Table 1 Parametric probability models allowed in defnummar.
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
#' @param crm A correlogram model, object of a class "SpatialCorrelogramModel",
#' output of makecormodel().
#' @param mask SpatialGridDataFrame indicating areas for further Monte Carlo
#' simulations via unconditional gaussian simulation.
#' @param object
#' @param ...  Additional parameters of um that may want to be saved here.
#'
#' @return An object of a class "NumMarSpatial". This is a list with the items
#' passed as arguments.
#' @author Kasia Sawicka, Gerard Heuvelink
#' @examples
#'
#' data(geul) % Must prep the data.
#' geul.cormodel <- makecrm(acf0 = 0.35, range = 20, model = "Exp")
#' geul.pb <- defnummarspatial(uncertain = TRUE, sp.obj = geul.krig, crm = geul.cormodel, mask = geul.mask)
#' str(geul.pb)
#' @export
#' 
defnummarspatial <- function(uncertain = TRUE, sp.obj, crm = NULL, mask = NULL, ...) {
                             # we need an argument here which specifies if this objcect is
                             # cross-correlated with any other input to the model.

  # CHECK: if uncertain is logical
  if (class(uncertain) != "logical") {
    stop("Uncertain must be logical")
  }

  # CHECK: 1. if sp.obj is provided, 2 if sp.obj is of required class, 3. if sp.obj contains data
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

  # CHECK: on sp.obj, e.g. if sp.obj is spatial
  # Here create list of all allowed types:


  # CHECK: if crm is of a class SpatialCorrelogram.
  if (!is.null(crm)) {
    if (class(crm)[1] != "SpatialCorrelogramModel")
      stop("crm has to be of a class SpatialCorrelogramModel")
  }

  # CHECK: if mask is a SpatialGridDataFrame.
  if (!is.null(mask)) {
    if (class(mask) != "SpatialGridDataFrame")
      stop("mask has to be of class SpatialGridDatFrame")
  }

  # CHECK: if sp.obj and mask has same coordinate reference system.
  if (!is.null(mask) & !is.null(sp.obj)) {
    if (!identical(sp.obj@proj4string, mask@proj4string))
      stop("Data item in spatial object and mask have different coordinate reference systems")
  }

  um <- list(uncertain = uncertain,
             sp.obj = sp.obj,
             crm = crm,
             mask = mask
             )
  extraargs = list(...)
  um = c(um, extraargs)

  # Set the name for the class
  class(um) <- "NumMarSpatial"
  um

} # END defnummarspatial




# Example===============================================================================================
# Case: no crm provided.
# a <- defnummarspatial(uncertain = TRUE, sp.obj = geul.krig)
# str(a)
#
# b <- defnummarspatial(uncertain = TRUE, sp.obj = geul.krig, crm = "???")
# str(b)
#
# c <- defnummarspatial(uncertain = TRUE, sp.obj = geul.krig, crm = "???", mask = mask)
# str(c)
# myGeul = defnummarspatial(uncertain = TRUE, sp.obj = geul.krig, crm = mycormodel, mask = mask)
# myGeulextra = defnummarspatial(uncertain = TRUE, sp.obj = geul.krig, crm = mycormodel, mask = mask, extraarg = "extra argument here")
# myGeulextra = defnummarspatial(uncertain = TRUE, sp.obj = geul.krig, crm = mycormodel, mask = mask, extraarg = "extra argument here",
#                                some.number = 45678)
