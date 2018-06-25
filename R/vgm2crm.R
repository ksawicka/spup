#' Convert vgm to crm
#'
#' @param vgm See ?vgm
#' @param psill See ?vgm
#' @param nugget See ?vgm
#' @param range See ?vgm
#' @param model See ?vgm
#' @param kappa See ?vgm
#' @param anis See ?vgm
#' @param Err See ?vgm
#' 
#'
#' @return Spatial correlogram model - standardised parameters of spatial variogram model
#' 
#' @author Kasia Sawicka
#' 
#' 
vgm2crm <- function(vgm, psill, nugget, range, model, kappa = 0.5, anis, Err = 0) {

  if (!missing(vgm)) {
    if (length(vgm$psill) == 1) {
      psill <- vgm$psill[1]
      range <- vgm$range[1]
	  model <- vgm$model[1]
	  nugget <- 0
    } else {
	  psill <- vgm$psill[2]
      range <- vgm$range[2]
	  model <- vgm$model[2]
	  nugget <- vgm$psill[1]
	}
    anis <- vgm$anis
    kappa <- vgm$kappa
    Err <- vgm$Err
  }
  
  stopifnot(length(psill) == 1) 
  stopifnot(length(range) == 1)
  stopifnot(length(model) == 1)
  
  if (missing(nugget)) nugget <- 0
  if (missing(anis)) anis <- NULL
  
  
  acf0 <- psill/(psill + nugget)
  crm <- list(acf0 = acf0,
             range = range,
             model = as.character(model),
             anis = anis,
             kappa = kappa,
             Err = Err)
  
  class(crm) <- "SpatialCorrelogramModel"
  crm
}

a=vgm(10, "Exp", 300, nugget = 2)
b=vgm2crm(vgm=a)
b=vgm2crm(psill = 8, range = 100, model = "Exp")
