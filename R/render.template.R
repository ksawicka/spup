#' Render method for "template" class.
#'
#' @param x 
#' @param ... 
#'
#' @return cos
#'
#' @examples
#' 
#' @export
render.template <- function(x, ...) {
  sub(pattern = "\\.template$", replacement = "", x = x) %T>% 
    walk(function(x) {
      readLines(paste(x, "template", sep = "."), warn = FALSE) %>% 
        render(...) %>%
        writeLines(x)
    }) %>%
    as.character
}