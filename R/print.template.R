#' Print method for class "template."
#'
#' @param x Object of class "template".
#' @param ... additional parameters.
#'
#' @return Template file content.
#'
#' @author Dennis Walvoort
#' 
#' @export
print.template <- function(x, ...) {
  cat("container containing the following template(s):\n")
  print(as.character(x))
}