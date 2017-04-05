#' sd() function for MC sample saved in a SpatialGridDataFrame
#' 
#' Calculates sd from MC realizations for each location in a map.
#'
#' @param realizations MC sample saved in SpatialGridDataFrame.
#' @param ... additional parameters.
#'
#' @return SpatialGridDataFrame; a sd of a MC sample.
#'
#' @author Kasia Sawicka
#' 
#' @examples
#' 
#' data(dem30m, dem30m_sd)
#' dem_crm <- makecrm(acf0 = 0.78, range = 321, model = "Exp")
#' demUM <- defineUM(uncertain = TRUE, distribution = "norm", 
#'                    distr_param = c(dem30m, dem30m_sd), crm = dem_crm)
#' dem_sample <- genSample(UMobject = demUM, n = 50, samplemethod = "ugs", nmax = 20)
#' slope_sample <- propagate(realizations = dem_sample, model = Slope, n = 50)
#' slope_sd <- sd(slope_sample, na.rm = TRUE)
#' 
#' @export
sd.SpatialGridDataFrame <- function(realizations, ...) {
  
  X <- as.matrix(realizations@data)
  sd_realizations <- apply(X, MARGIN = 1, sd, ...)
  s <- realizations
  s@data <- as.data.frame(sd_realizations)
  s
  
  # here implement that the names of "realizations" correspond
  # with names of argument 'realizations'
  # e.g. if it is slope, have mean_slope, sd_slope, etc.
  
}


