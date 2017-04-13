#' Render method for "template" class.
#'
#' Rendering is the process of replacing the tags in moustaches by text.
#' 
#' @param x an object of class "template", a model input file with additional
#' extension ".template".
#' @param ... additional parameters.
#'
#' @return Rendered template file.
#'
#' @author Dennis Walvoort
#' 
#' 
#' @importFrom purrr walk
#' @importFrom magrittr %>% %T>%
#' 
#' @export
render.template <- function(x, ...) {
  sub(pattern = "\\.template$", replacement = "", x = x) %T>% 
    purrr::walk(function(x) {
      readLines(paste(x, "template", sep = "."), warn = FALSE) %>% 
        render(...) %>%
        writeLines(x)
    }) %>%
    as.character
}