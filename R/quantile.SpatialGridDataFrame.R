#' quantile function for MC sample saved in SpatialGridDataFrame
#'
#' @param realizations 
#' @param ... 
#'
#' @return quantile of MC sample
#' 
#' @examples
#' 
#' data(DEM)
#' dem_crm <- makecrm(acf0 = 0.78, range = 321, model = "Exp")
#' demUM <- defineUMs(uncertain = TRUE, distribution = "norm", 
#'                    distr_param = c(dem30m, dem30m_sd), crm = dem_crm)
#' dem_sample <- genSample(UMobject = demUM, n = 50, samplemethod = "ugs", nmax = 20)
#' slope_sample <- propagate1(realizations = dem_sample, model = Slope, n = 50)
#' slope_q <- quantile(slope_sample, probs = c(0.1, 0.9), na.rm = TRUE)
#' 
#' 
#' @export
#'
quantile.SpatialGridDataFrame <- function(realizations, ...) {

  X <- as.matrix(realizations@data)
  quant_samples <- apply(X, MARGIN = 1, FUN = quantile, ...)
  quant_samples <- t(quant_samples)
  quant <- realizations
  quant@data <- as.data.frame(quant_samples)
  nq <- names(quant)
  nq <- paste0("prob", nq)
  nq <- gsub("%", "perc", nq)
  names(quant) <- nq
  quant

}

