#' Converting a spatial correlogram model to a variogram model
#'
#' Used internally in genSample() in case of sampling by unconditional gaussian
#' simulation.
#'
#' To assure equalfinality the sill parameter for spatially correlated random
#' residuals is fixed and standardized to 1.
#'
#' @param crm object of a class "SpatialCorrelogramModel", output of makecrm(). 
#'
#' @return An object of a class "variogramModel" extending data.frame.
#' 
#' @author Kasia Sawicka, Gerard Heuvelink
#' 
#' @importFrom gstat vgm
#'
crm2vgm <- function(crm) {
  
  nugget <- 1 - as.numeric(crm$acf0)
  psill <- crm$acf0
  range <- crm$range
  model <- crm$model
  anis  <- crm$anis
  kappa <- crm$kappa
  Err   <- crm$Err
  vgm <- gstat::vgm(nugget = as.numeric(nugget),
                    psill = as.numeric(psill),
                    range = as.numeric(range),
                    model = model,
                    anis = anis,
                    kappa = kappa,
                    Err = Err
                    )
  vgm
  
}
