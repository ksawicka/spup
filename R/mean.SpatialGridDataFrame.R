
#' Calculate mean of the MC sample that is saved in a SpatialGridDataFrame
#'
#' @param realizations MC sample
#' @param ... additional parameters
#'
#' @return mean and sd of MC sample
#'
#' @examples
#' 
#' data(DEM)
#' dem_crm <- makecrm(acf0 = 0.78, range = 321, model = "Exp")
#' demUM <- defineUM(uncertain = TRUE, distribution = "norm", 
#'                    distr_param = c(dem30m, dem30m_sd), crm = dem_crm)
#' dem_sample <- genSample(UMobject = demUM, n = 50, samplemethod = "ugs", nmax = 20)
#' slope_sample <- propagate1(realizations = dem_sample, model = Slope, n = 50)
#' slope_mean <- mean(slope_sample, na.rm = TRUE)
#' 
#' @export
mean.SpatialGridDataFrame <- function(realizations, ...) {

  X <- as.matrix(realizations@data)
  mean_realizations <- apply(X, MARGIN = 1, mean, ...)
  mu <- realizations
  mu@data <- as.data.frame(mean_realizations)
  mu

  # here implement that the names of "realizations" correspond
  # with names of argument 'realizations'
  # e.g. if it is slope, have mean_slope, sd_slope, etc.
   
}
  