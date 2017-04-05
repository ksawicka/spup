#' Standard Deviation
#'
#' @param x as for stats::sd
#' @param ... additional parameters.
#'
#' @return Standard Deviation
#' 
#' @export
sd <- function(realizations, ...) {
  UseMethod("sd")
}