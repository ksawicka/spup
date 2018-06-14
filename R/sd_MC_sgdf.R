#' sd() function for MC sample saved in a SpatialGridDataFrame
#' 
#' Calculates sd from MC realizations for each location in a map.
#'
#' @param realizations MC sample saved in a SpatialGridDataFrame.
#' @param ... additional parameters.
#'
#' @return SpatialGridDataFrame; a sd of a MC sample.
#'
#' @author Kasia Sawicka
#' 
#' @examples
#' 
#' set.seed(12345)
#' data(dem30m, dem30m_sd)
#' dem_crm <- makeCRM(acf0 = 0.78, range = 321, model = "Exp")
#' demUM <- defineUM(uncertain = TRUE, distribution = "norm", 
#'                    distr_param = c(dem30m, dem30m_sd), crm = dem_crm)
#' \dontrun{
#' dem_sample <- genSample(UMobject = demUM, n = 50, samplemethod = "ugs",
#'                         nmax = 20, asList = FALSE)
#' dem_sample_sd <- sd_MC_sgdf(dem_sample)
#' }
#' 
#' @export
sd_MC_sgdf <- function(realizations, ...) {
  
  X <- as.matrix(realizations@data)
  sd_realizations <- apply(X, MARGIN = 1, sd, ...)
  s <- realizations
  s@data <- as.data.frame(sd_realizations)
  s
  
  # here implement that the names of "realizations" correspond
  # with names of argument 'realizations'
  # e.g. if it is slope, have mean_slope, sd_slope, etc.
  
}


