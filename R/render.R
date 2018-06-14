#' Rendering template
#'
#' Rendering is the process of replacing the tags in moustaches by text.
#' For this, we provide a set of render-methods. See the `whisker` package
#' (or https://mustache.github.io) for more information.
#'
#' @param x an object of class "character" or "template".
#' @param ... additional parameters.
#'
#' @return Rendered character template or a file on disk.
#'
#' @author Dennis Walvoort
#' 
#' @examples
#' 
#' require(magrittr)
#' require(whisker)
#' # render character string
#' my_template <- "Hello {{name}}. How are you doing?"
#' my_template %>% 
#'   render(name = "Winnie the Pooh")
#' 
#' # render table      
#' my_template <- c(
#'      "| x | y |",
#'      "|---|---|",
#'      "{{#MY_TABLE}}",
#'      "| {{X}} | {{Y}} |",
#'      "{{/MY_TABLE}}")
#' my_table <- data.frame(X = 1:5, Y = letters[1:5])  
#' my_table
#' my_template %>% 
#' render(MY_TABLE = unname(rowSplit(my_table))) %>%
#' cat
#' 
#' @export
render <- function(x, ...) {
  UseMethod("render")
}