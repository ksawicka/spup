
genSample.NumMarSpatial <- function(UMobject, n, samplemethod, p = 0, ...) {
  
  stopifnot(UMobject$uncertain == TRUE)
  distribution <- UMobject[[2]]
  distr_param1 <- UMobject[[3]][[1]]
  distr_param2 <- UMobject[[3]][[2]]
  crm <- UMobject[[4]]
  dots <- list(...)
  mask <- distr_param1
  
  # samplemethod = 'ugs'
  if (samplemethod == "ugs") { 

    # CHECK: if crm provided
    if (is.null(crm))
      stop("Correlogram model is required for the 'ugs' sampling method.")
    if (distribution != "norm")
      stop("Only normal distribution can be assumed in the 'ugs' method.")

    # Convert the UMobject to gstat object so it works with predict.
    g <- gstat(formula = z~1, # locations = ~x+y, # KS: is it always x and y?
               dummy = TRUE, beta = 0, model = crm2vgm(crm), ...)
    epsilon_sample <- predict(object = g, newdata = mask, nsim = n)
    X_sample <- epsilon_sample
    # if it is Spatial data frame
    X_sample@data <- as.data.frame(apply(as.matrix(epsilon_sample@data),
                                         MARGIN = 2, function(x) distr_param1 + distr_param2*x))
    # What if it is a raster? How to generate epsilon sample if it is a raster?
    # Convert to spatial grid data frame and back is only thing I can think of.
  } 
  
  
  # samplemethod = 'randomSampling'
  if (samplemethod == "randomSampling") {
    if (class(distr_param1) != "RasterLayer") {
      distr_param1df <- as.data.frame(distr_param1)
      distr_param2df <- as.data.frame(distr_param2)
      # For each grid cell, n realizations will be generated using randomSamp.R
      temp_samples <- foreach(a = distr_param1df, b = distr_param2df, .combine = rbind) %do% {
        randomSamp(a, b, n)
      }
    }  
    
    temp_samples <- temp_samples[ , sample(ncol(temp_samples))]
    X_sample <- UMobject[[4]]
    X_sample@data <- as.data.frame(temp_samples)
   

  } 


  # ===================STRATIFIED SAMPLING (stratifiedSampling) - START=========

  # if (samplemethod == "stratifiedSampling") {
  #   if (n %% (length(p)-1) != 0) {
  #     stop("n should be divisable by the number of strata")
  #   } else {
  #     temp_samples <- foreach(a = UMobject[[2]][[1]], b = sqrt(UMobject[[2]][[2]]), .combine = rbind) %do% {
  #       as.numeric(stratsampSpatial(a, b, n/(length(p)-1), p))
  #     }
  #     temp_samples <- temp_samples[,sample(ncol(temp_samples))]
  #     X_sample <- UMobject[[4]]
  #     X_sample@data <- as.data.frame(temp_samples)
  #     map.of.sd <- UMobject[[4]]
  #     sd.samples <- apply(temp_samples, MARGIN = 1, sd)
  #     map.of.sd@data <- as.data.frame(sd.samples)
  #     samples <- list(X = X_sample, map.of.sd = map.of.sd, sampling.method = samplemethod)
  #     class(samples) <- "SpatialMCSample"
  #   }
  # } # END

  X_sample

} 





# ===================Examples===================
# data(geul)
# geul.cormodel <- makecrm(acf0 = 0.35, range = 20, model = "Exp")
# geul.pb <- defnummarspatial(uncertain = TRUE, sp.obj = geul.krig, crm = geul.cormodel, mask = geul.mask)
# geul.sim <- genSample(n = 5, UMobject = geul.pb, samplemethod = "ugs", nmax = 20)
