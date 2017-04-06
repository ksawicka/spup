#' Function to find the level of list nesting
#'
#' @param List an object of class 'list'.
#'
#' @return an integer; level of list nesting
#'
#' @author Kasia Sawicka
#' 
#' @examples
#' 
#' a <- list(1,2)
#' list_depth(a)
#' 
#' a <- list(list(1, 2), 3)
#' list_depth(a)
#' 
#' @export
list_depth <- function(List) {
  ifelse(is.list(List), 1L + max(sapply(List, list_depth)), 0L)
}