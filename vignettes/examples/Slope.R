Slope <- function(DEM, ...) {
  require(raster)
  demraster <- 
    DEM %>%
    raster()
  
  demraster %>%
    terrain(opt = 'slope', unit = 'degrees', ...) %>%
    as("SpatialGridDataFrame")
}