#' Render method for "character class".
#'
#' @param x 
#' @param ... 
#'
#' @return cos
#' @export
#'
#' @examples
#' 
#' # render character string
#' my_template <- "Hello {{name}}. How are you doing?"
#' my_template %>% 
#'   render(name = "Kasia")
#' 
#' # render table      
#' my_template <- c(
#'      "| x | y |",
#'      "|---|---|",
#'      "{{#MY_TABLE}}",
#'      "| {{X}} | {{Y}} |",
#'      "{{/MY_TABLE}}"
#' my_table <- data.frame(X = 1:5, Y = letters[1:5])  
#' my_table
#' my_template %>% 
#' render(MY_TABLE = unname(rowSplit(my_table))) %>%
#' cat
#'   
#' 
render.character <- function(x, ...) {
  dots <- list(...)
  if (length(dots) == 0L) {
    return(x)
  }
  x %>% 
    whisker::whisker.render(dots)
}