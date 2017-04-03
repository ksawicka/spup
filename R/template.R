#' Construct class "template".
#' 
#' Class that stores all templates with model inputs. The aim of this class is to: 1. organise model input files;
#' 2. perform some checks. 
#' 
#' A template is simply a model input file with: 1. the additional extension `.template`. 
#' 2. input that needs to be modified is replaced by mustache-style tags.
#'
#' @param filenames Name on the model input file.
#'
#' @return An obejct of a class "template".
#'
#' @examples
#' 
#' my_template <- template("vignettes/examples/input.txt.template")
#' readLines(my_template, warn = FALSE)
#' 
#' @export
template <- function(filenames) {
  filenames %>% 
    purrr::walk(
      function(x) {
        if (!grepl(pattern = "\\.template$", x = x)) {
          stop(
            "File %s does not have extension 'template'" %>% 
              sprintf(x %>% sQuote), 
            call. = FALSE
          )
        }
      }
    )
  filenames %>% 
    purrr::walk(
      function(x) {
        if (!file.exists(x)) {
          stop(
            "File %s not found" %>% 
              sprintf(x %>% sQuote), 
            call. = FALSE
          )
        }
      }
    )
  class(filenames) <- "template"
  filenames
}