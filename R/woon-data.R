#' Neighbourhood in Rotterdam.
#'
#' The 'woon' object is a SpatialPolygonDataFrame where each building is represented by one polygon.
#'
#' @docType data
#' 
#' @format a SpatialPolygonDataFrame with 723 polygons and 7 variables:
#' \describe{
#' \item{vbos}{number of addresses present in the building}
#' \item{woonareash}{residential area, in percent}
#' \item{Function}{assigned category depending on vbos and woonareash -
#'  for residential is 1, for offcie is 2, for other is 3}
#' \item{residential}{probability that the building is residential}
#' \item{office}{probability that the building is an office}
#' \item{other}{probability that the building has other function}
#' \item{check}{check if probabilities sum to 1}
#' }
#' 
#' @usage data(dem30m_sd)
#' 
#' @source Kadaster, NL.
#'  
"woon"