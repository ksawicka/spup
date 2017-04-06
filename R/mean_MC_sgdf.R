#' mean() function for MC sample saved in a SpatialGridDataFrame
#' 
#' Calculates mean from MC realizations for each location in a map.
#'
#' @param realizations MC sample saved in SpatialGridDataFrame.
#' @param ... additional parameters.
#'
#' @return SpatialGridDataFrame; a mean of a MC sample.
#'
#' @author Kasia Sawicka
#' 
#' @examples
#' 
#' # load data
#' data(dem30m, dem30m_sd)
#' dem_crm <- makecrm(acf0 = 0.78, range = 321, model = "Exp")
#' demUM <- defineUM(uncertain = TRUE, distribution = "norm", 
#'                    distr_param = c(dem30m, dem30m_sd), crm = dem_crm)
#' dem_sample <- genSample(UMobject = demUM, n = 50, samplemethod = "ugs", 
#'                         nmax = 20, asList = FALSE)
#' dem_mean <- mean_MC_sgdf(dem_sample)
#' 
#' @export
mean_MC_sgdf <- function(realizations, ...) {

  X <- as.matrix(realizations@data)
  mean_realizations <- apply(X, MARGIN = 1, mean, ...)
  mu <- realizations
  mu@data <- as.data.frame(mean_realizations)
  mu

  # here implement that the names of "realizations" correspond
  # with names of argument 'realizations'
  # e.g. if it is slope, have mean_slope, sd_slope, etc.
   
}
  