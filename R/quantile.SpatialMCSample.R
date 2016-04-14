# S3 method for "SpatialMCSample" class object.

# x - output of genSample() "SpatialMCSample" object.

quantile.SpatialMCSample <- function(x, ...) {

  quant <- x
  names(quant)[2] <- "quantile"
  quant.samples <- apply(as.matrix(quant[[1]]@data), MARGIN = 1, FUN = quantile, ...)
  quant.samples <- t(quant.samples)
  quant[[2]]@data <- as.data.frame(quant.samples)
  quant

}

# Example:
# data(geul)
# geul.cormodel <- makecrm(acf0 = 0.35, range = 20, model = "Exp")
# geul.pb <- defnummarspatial(uncertain = TRUE, sp.obj = geul.krig, crm = geul.cormodel, mask = geul.mask)
# geul.sim <- genSample(n = 5, uncert.object = geul.pb, samplemethod = "ugs", nmax = 20)
# a <- quantile(x = geul.sim, probs = c(0.5, 0.75, 0.95, 0.99), na.rm = TRUE)
# str(a)
