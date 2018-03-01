#' Defining a spatial correlogram model
#'
#' Function that generates a spatial correlogram model, an object of class "SpatialCorrelogramModel".
#'
#' For the spatial variables allowed autocorrelation functions are listed in Table 4.1 of the
#' gstat manual (\url{http://www.gstat.org/gstat.pdf}). Spatial correlation
#' assumes stationarity, i.e. correlation depends only on the separation
#' distance between points in space. Anisotropy is allowed (http://www.gstat.org/gstat.pdf). No nested
#' models are allowed in the current version.
#'
#' 
#' @aliases makecrm
#' @param acf0 Aurocorrelation function value at distance near 0. Default is 1. Must
#' fall in interval [0,1].
#' @param range Range parameter of the correlogram model component.
#' @param model Model type, e.g. "Exp", "Sph", "Gau", "Mat" that vgm() accepts. See ?gstat::vgm() for more #' details.
#' @param anis Anisotropy parameters. See ?gstat::vgm() for more details.
#' @param kappa Smoothness parameter for the Matern class of variogram models. See ?gstat::vgm() for more #' details.
#' @param add.to See ?gstat::vgm() (currently not in use)
#' @param covtable See ?gstat::vgm() (currently not in use)
#' @param Err Numeric. See ?gstat::vgm() for more details.
#'
#' 
#' @return An object of a class "SpatialCorrelogramModel". This is a list collating provided arguments.
#' 
#' @author Kasia Sawicka, Gerard Heuvelink
#' 
#' @examples
#'
#' mycormodel <- makeCRM(acf0 = 0.8, range = 300, model = "Exp")
#' str(mycormodel)
#'
#' @importFrom gstat vgm
#' 
#' @export
makeCRM <- makeCRM <- function(acf0 = 1, range = NA, model, anis, kappa = 0.5, add.to, covtable, Err = 0) {

  # acf0 checks
  stopifnot(class(acf0) == "numeric")
  stopifnot(class(range) == "numeric" | is.na(range))
  if (acf0 < 0 | acf0 > 1)
    warning("For standardized residuals acf0 argument should be between 0 and 1.")

  # anis checks
  if (missing(anis)) 
    anis <- c(0, 0, 0, 1, 1)
  if (length(anis) == 2) 
    anis <- c(anis[1], 0, 0, anis[2], 1)
  else if (length(anis) != 5) 
    stop("anis vector should have length 2 (2D) or 5 (3D)")
    
  # model checks
  models <- gstat::vgm()$short
  if (model %in% models == FALSE)
    stop("Only models accepted by gstat::vgm are allowed.")
    
  # range checks
    if (!is.na(range)) {
      if (model != "Nug") {
          if (model != "Lin" && model != "Err" && model != 
              "Int") 
              if (range <= 0) 
                stop("range should be positive")
              else if (range < 0) 
                stop("range should be non-negative")
      }
      else {
          if (range != 0) 
              stop("Nugget should have zero range")
          if (anis[4] != 1 || anis[5] != 1) 
              stop("Nugget anisotropy is not meaningful")
      }
    }

  crm <- list(acf0 = acf0,
              range = range,
              model = model,
              anis = anis,
              kappa = kappa,
              # add.to = add.to,
              # covtable = covtable,
              Err = Err)
  class(crm) <- c("SpatialCorrelogramModel")
  crm
}


# deprecation of makecrm
makecrm <- function(acf0 = 1, range = NA, model, anis, kappa = 0.5, add.to, covtable, Err = 0) {
  .Deprecated(new = "makeCRM")
  makeCRM(acf0, range, model, anis, kappa, add.to, covtable, Err)
}










