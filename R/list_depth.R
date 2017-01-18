#' Title depth
#'
#' @param List an object of class 'list'
#'
#' @return an integer; level of list nesting
#'
#' @examples
#' 
#' a <- list(1,2)
#' depth(a)
#' 
#' a <- list(list(1, 2), 3)
#' depth(a)
#' 
list_depth <- function(List) {
  ifelse(is.list(List), 1L + max(sapply(List, depth)), 0L)
}