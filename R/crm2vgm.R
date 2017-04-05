#' Converting a spatial correlogram model to a variogram model
#'
#' Used internally in genSample() in case of sampling by unconditional gaussian
#' simulation.
#'
#' To assure equalfinality the sill parameter for spatially correlated random
#' residuals is fixed and standardized to 1.
#'
#' @usage crm2vgm(crm, ...)
#'
#' @param crm Spatial correlogram model, output of makecrm(). 
#' @param ...  Parameters that can be passed to vgm().
#'
#' @return An object of a class "variogramModel" extending data.frame.
#' 
#' @author Kasia Sawicka, Gerard Heuvelink
#' 
#' @importFrom gstat vgm
#'
crm2vgm <- function(crm, ...) {
  nugget <- 1 - as.numeric(crm[[1]])
  # psill <- (nugget * crm[[1]])/(1 - crm[[1]]) # This is correct, but just returns value of acf0!
  psill <- crm[[1]]
  range <- crm[[2]]
  model <- crm[[3]]
  vgm <- gstat::vgm(nugget = as.numeric(nugget),
                    psill = as.numeric(psill),
                    range = as.numeric(range),
                    model = model,
                    ...)
  vgm
}
