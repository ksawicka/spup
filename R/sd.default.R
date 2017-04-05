#' Standard Deviation as in stats::sd
#'
#' @param realizations a numeric vector or an R object which is coercible to one by as.double(x).
#' @param ... additional parameters.
#'
#' @return Standard Deviation
#' 
#' @importFrom stats sd
#' 
#' @export
sd.default <- function(realizations, ...) {
  stats::sd(x = realizations, ...)
}