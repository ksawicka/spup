#' Render method for "character" class.
#'
#' Rendering is the process of replacing the tags in moustaches by text.
#' 
#' @param x an object of class "character".
#' @param ... additional parameters.
#'
#' @return Rendered character template.
#'
#' @author Dennis Walvoort
#' 
#' @examples
#' 
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
#' @importFrom whisker whisker.render
#' @importFrom magrittr %>%
#' 
#' @export
render.character <- function(x, ...) {
  dots <- list(...)
  if (length(dots) == 0L) {
    return(x)
  }
  x %>% 
    whisker::whisker.render(dots)
}