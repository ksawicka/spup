#' mean() function for MC sample saved in a SpatialGridDataFrame
#' 
#' Calculates mean from MC realizations for each location in a map.
#'
#' @param x MC sample saved in SpatialGridDataFrame.
#' @param ... additional parameters.
#'
#' @return SpatialGridDataFrame; a mean of a MC sample.
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
#' slope_mean <- mean(slope_sample, na.rm = TRUE)
#' 
#' @export
mean.SpatialGridDataFrame <- function(x, ...) {

  X <- as.matrix(x@data)
  mean_realizations <- apply(X, MARGIN = 1, mean, ...)
  mu <- x
  mu@data <- as.data.frame(mean_realizations)
  mu

  # here implement that the names of "realizations" correspond
  # with names of argument 'realizations'
  # e.g. if it is slope, have mean_slope, sd_slope, etc.
   
}
  