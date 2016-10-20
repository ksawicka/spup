#' Simple check if class of provided object is Spatial
#'
#' @param object Any R object. In defineUM() it is used to
#' examine what type of data are delt with.
#'
#' @return TRUE or FALSE.
#'
check_if_Spatial <- function(object) {
  
  Spatial_classes <- c("SpatialPoints",
                       "SpatialPointsDataFrame",
                       "SpatialMultiPoints",
                       "SpatialMultiPointsDataFrame",
                       "SpatialPixels",
                       "SpatialPixelsDataFrame",
                       "SpatialGrid",
                       "SpatialGridDataFrame",
                       "Line",
                       "Lines",
                       "SpatialLines",
                       "SpatialLinesDataFrame",
                       "Polygon",
                       "Polygons",
                       "SpatialPolygons",
                       "SpatialPolygonsDataFrame",
                       "RasterLayer")
  
  class(object) %in% Spatial_classes
  
}















