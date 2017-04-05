#' Variance as in stats::var
#'
#' @param realizations a numeric vector, matrix or data frame.
#' @param ... additional parameters.
#'
#' @return Variance
#' 
#' @importFrom stats var
#' 
#' @export
var.default <- function(realizations, ...) {
  stats::var(x = realizations, ...)
}
