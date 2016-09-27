#' Executing selected model runs with uncertain inputs
#'
#' Function that allows user to run selected model number of times using MC
#' sample.
#'
#' \strong{model} has to be a user specific function that uses input sample as
#' an argument.
#'
#' @usage propagate(model, n, ...)
#'
#' @param model 
#' @param realizations A list of samples. If contains more than one item, 
#' they have to be of the same number of realizations.
#' @param n number of model runs. Relates to number of first realizations to take.
#' @param ... Additional parameters, e.g. model inputs/parameters. Must be named 
#' like in model definition.
#' 
#' @return Multiple model runs as a list.
#' 
#' @author Kasia Sawicka, Dennis Walvoort, Gerard Heuvelink
#' @examples
#'
#' 
#'
#' @export
#' 
propagate1 <- function(realizations, model, n, ...) {
  
  out1 <-
    map(1:n, function(x){realizations[x]}) %>%
    list() %>%
    pmap(model, ...)

  out2 <- out1[[1]]
  col_name_base <- names(out2)
  names(out2) <- paste(col_name_base, "1", sep = "")
  for (i in 2:n) {
    names(out1[[i]]) <- paste(col_name_base, i, sep = "")
    out2 <- cbind(out2, out1[[i]])
  }
  out2
  
}

