#' Convert vgm to crm
#'
#' @param vgm See ?vgm
#' @param psill See ?vgm
#' @param nugget See ?vgm
#' @param range See ?vgm
#' @param model See ?vgm
#'
#' @return Standardised parameters of spatial variogram model.
#' 
#' @author Kasia Sawicka
#' 
#' 
vgm2crm <- function(vgm, psill, nugget, range, model) {

  stopifnot(length(psill) == 1) 

  acf0 <- psill/(psill + nugget)
  crm <- list(acf0 = acf0,
             range = range,
             model = model)
  
  class(crm) <- "SpatialCorrelogramModel"
  crm
}

