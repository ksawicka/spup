#' var default
#'
#' @param realizations 
#' @param ... 
#'
#' @return stats::var
#'
#' @examples
#' 
#' @export
var.default <- function(realizations, ...) {
  stats::var(x = realizations, ...)
}
