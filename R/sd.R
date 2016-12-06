#' sd as generic
#'
#' @param x 
#' @param ... 
#'
#' @return sd
#'
#' @examples
#' 
#' @export
sd <- function(realizations, ...) {
  UseMethod("sd")
}