#' Function to run geul model
#'
#' @param pb map of Pb concentrations (SpatialGridDataFrame) [mg Pb/kg soil]
#' @param sc soil consumption [in grams?]
#'
#' @return soil ingestion
#' @author Kasia Sawicka
#'
geulModel <- function(pb, sc) {
  ingestion <- pb 
  ingestion@data <- pb@data * sc
  ingestion
}



