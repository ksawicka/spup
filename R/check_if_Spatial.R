#' Simple check if class of provided object is Spatial
#'
#' @param object Any R object. In defineUM() it is used to
#' examine what type of data are delt with.
#'
#' @return TRUE or FALSE.
#'
check_if_Spatial <- function(object) {
  Spatial_classes <- c("SpatialPointsDataFrame",
                       "SpatialMultiPointsDataFrame",
                       "SpatialPixelsDataFrame",
                       "SpatialGridDataFrame",
                       "SpatialLinesDataFrame",
                       "SpatialPolygonsDataFrame",
                       "RasterLayer")
  class(object) %in% Spatial_classes
}















