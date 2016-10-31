
stats <- function(sample, ...) {
  
  # For example SptialGridDataFrame
  map.of.sd <- sample
  sd.samples <- apply(as.matrix(X_sample@data), MARGIN = 1, sd)
  map.of.sd@data <- as.data.frame(sd.samples)
   
}
  