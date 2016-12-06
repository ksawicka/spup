#' Defining a spatial correlogram model
#'
#' Function that generates a spatial correlogram model.
#'
#' For the spatial variables allowed autocorrelation functions are listed in Table 4.1 of the
#' gstat manual (\url{http://www.gstat.org/gstat.pdf}). Spatial correlation
#' assumes stationarity, i.e. correlation depends only on points separation
#' distance. Anisotropy is allowed (http://www.gstat.org/gstat.pdf). No nested
#' models are allowed in the current version.
#'
#' @usage makecrm(acf0 = 1, range, model, ...)
#' @param acf0 Aurocorrelation function value at range = 0. Default is 1. Must
#' fall into interval <0,1>.
#' @param range Range parameter of the correlogram model component.
#' @param model Model type, e.g. "Exp", "Sph", "Gau", "Mat" that vgm() accepts.
#' Calling vgm() without a model argument returns a data.frame with available
#' models.
#' @param ...  Arguments that will be passed to crm2vgm() that vgm() accepts.
#' @return An object of a class "SpatialCorrelogramModel". This is a list
#' collating provided arguments.
#' @author Kasia Sawicka, Gerard Heuvelink
#' @examples
#'
#' mycormodel <- makecrm(acf0 = 0.8, range = 300, model = "Exp")
#' mycormodel
#'
#' @export
makecrm <- function(acf0, range, model, ...) {

  stopifnot(class(acf0) == "numeric")
  stopifnot(class(range) == "numeric")
  # if acf0 is between 0 and 1
  if (acf0 < 0 | acf0 > 1)
    warning("For standardized residuals acf0 argument should be between 0 and 1.")
  # if model is a string from allowed list (see vgm code for example), etc.
  models <- gstat::vgm()$short
  if (model %in% models == FALSE)
    stop("Only models accepted by gstat::vgm are allowed.")
  
  crm <- c(acf0 = acf0,
           range = range,
           model = model,
           ...)
  class(crm) <- c("crm")
  crm

}

