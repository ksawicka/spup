#' Variance
#'
#' @param x as for stats::var
#' @param ... additional parameters.
#'
#' @return Variance
#' 
#' @export
var <- function(realizations, ...) {
  UseMethod("var")
}
