#' sd default
#'
#' @param realizations 
#' @param ... 
#'
#' @return stats::sd
#'
#' @examples
#' 
#' @export
sd.default <- function(realizations, ...) {
  stats::sd(x = realizations, ...)
}