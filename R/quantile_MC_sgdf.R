#' quantile() function for MC sample saved in a SpatialGridDataFrame
#'
#' Calculates mean from MC realizations for each location in a map.
#' 
#' @param realizations MC sample saved in SpatialGridDataFrame.
#' @param ... additional parameters.
#'
#' @return SpatialGridDataFrame; quantiles of a MC sample
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
#' dem_quantile <- quantile_MC_sgdf(dem_sample, probs = c(0.1, 0.9))
#' 
#' @importFrom stats quantile
#' 
#' @export
quantile_MC_sgdf <- function(realizations, ...) {

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
