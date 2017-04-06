#' Standard deviation of Digital Elevation Model of Zlatibor region in Serbia.
#'
#' A dataset containing the sd of an example Digital Elevation Model. 
#' It was calculated from dem30m using terrain funtion from raster package (opt = 'roughness').
#'
#' @docType data
#' 
#' @format a SpatialGridDataFrame with 15000 rows and 1 variable:
#' \describe{
#' \item{Elevation_sd}{Standard deviation of Digital Elevation Model, in meters}
#' }
#' 
#' @usage data(dem30m_sd)
#' 
#' @source The Zlatibor dataset was kindly provided by Prof. Branislav Bajat
#'  from the University of Belgrade, Serbia.
#'  
"dem30m_sd"