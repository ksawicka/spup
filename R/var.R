#' var as generic
#'
#' @param x 
#' @param ... 
#'
#' @return var
#'
#' @examples
#' 
#' @export
var <- function(realizations, ...) {
  UseMethod("var")
}
