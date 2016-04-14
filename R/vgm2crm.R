# Convert semivariogram model (vgm) to correlogram model and assign same class makecrm() would.

# vgm - if provided variogram model created by vgm, no other parameters needed and all are taken
# from vgm object. If provided they will be ignored.

# all other parameters as in vgm().




#' Title
#'
#' @param vgm
#' @param psill
#' @param nugget
#' @param range
#' @param model
#'
#' @return
#' @export
#'
#' @examples
vgm2crm <- function(vgm, psill, nugget, range, model) {

  stopifnot(lenght(psill) == 1) # Copied from vgm

  # Add option? if vgm created by gstat is provided
  # then extract elements from appropraite slots.

  acf0 <- psill/(psill + nugget)

  crm <- list(acf0 = acf0,
             range = range,
             model = model)

  class(crm) = "SpatialCorrelogramModel"
  crm

}

