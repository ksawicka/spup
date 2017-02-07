#' Print method for class "template."
#'
#' @param x 
#' @param ... 
#'
#' @return what template the the template file contains.
#'
#' @examples
#' 
#' @export
print.template <- function(x, ...) {
  cat("container containing the following template(s):\n")
  print(as.character(x))
}