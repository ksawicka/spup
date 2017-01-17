
#' Calculate variance of the MC sample that is saved in a SpatialGridDataFrame
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
#' slope_var <- var(slope_sample, na.rm = TRUE)
#' 
#' @export
var.SpatialGridDataFrame <- function(realizations, na.rm = FALSE, ...) {

  X <- as.matrix(realizations@data)
  var_realizations <- apply(X, MARGIN = 1, var, ...)
  v <- realizations
  v@data <- as.data.frame(var_realizations)
  v

  # here implement that the names of "realizations" correspond
  # with names of argument 'realizations'
  # e.g. if it is slope, have mean_slope, sd_slope, etc.
   
}
  

