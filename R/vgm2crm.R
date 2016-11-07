#' Convert vgm to crm
#'
#' @param vgm See ?vgm
#' @param psill See ?vgm
#' @param nugget See ?vgm
#' @param range See ?vgm
#' @param model See ?vgm
#'
#' @return Standardised parameters of spatial correlogram model
#'
#' @examples
#' 
#' # Examples here
#' 
#' @export
vgm2crm <- function(vgm, psill, nugget, range, model) {

  stopifnot(lenght(psill) == 1) # Copied from vgm

  # Add option? if vgm created by gstat is provided
  # then extract elements from appropraite slots.

  acf0 <- psill/(psill + nugget)
  crm <- list(acf0 = acf0,
             range = range,
             model = model)
  crm
}

