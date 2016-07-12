#' ugs
#'
#' @param n Number of draws
#' @param mean Map of mean values 
#' @param sd Map of sd values
#' @param mask Empty map for creating the dummy map of residuals
#' @param crm Correlogram model. Output of makecrm()
#'
#' @return
#' @export
#'
#' @examples
#' 
ugs <- function(n, mean, sd, mask, crm) {
  
  g_dummy <- gstat(formula = z~1,
                   dummy = TRUE,
                   beta = 0,
                   model = crm2vgm(crm),
                   nmax = 24)
  resid_samples <- predict(object = g_dummy,
                           newdata = mask,
                           nsim = n)
  
  mean <- mean[[1]]
  sd <- sqrt(sd[[1]])
  samples <- mask
  samples@data <- as.data.frame(apply(as.matrix(resid_samples@data),
                                           MARGIN = 2, function(x) mean + sd*x))
  
  mean_of_samples <- apply(as.matrix(samples@data), MARGIN = 1, mean)
  map_of_mean_samples <- mask
  map_of_mean_samples@data <- as.data.frame(mean_of_samples)
  
  sd_of_samples <- apply(as.matrix(samples@data), MARGIN = 1, sd)
  map_of_sd_samples <- mask
  map_of_sd_samples <- as.data.frame(sd_of_samples)
  
  samples <- list(samples = samples,
                  map_of_mean_samples = map_of_mean_samples,
                  map_of_sd_samples = map_of_sd_samples,
                  crm = crm) # Probably not needed but Gerard wants to have it.
  samples
  # Here maybe we want it to be an object of class something like "SpatialMCSample"
}