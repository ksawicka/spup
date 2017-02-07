#' Generaic function render.
#'
#' Rendering is the process of replacing the tags in moustaches by text.
#' For this, we provide a set of render-methods. See the `whisker`-package
#' (or https://mustache.github.io) for more information.
#'
#' @param x 
#' @param ... 
#'
#' @return cos
#'
#' @examples
#' 
#' @export
render <- function(x, ...) {
  UseMethod("render")
}