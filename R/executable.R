#' Wrapper function for calling executables in R.
#'
#' @param filename a path with a name to the .exe file to wrapped here.
#'
#' @return Executable output.
#' 
#' @author Dennis Walvoort
#' 
#' @importFrom magrittr %>%
#' 
#' @export
executable <- function(filename) {
  if (!file.exists(filename)) {
    stop(
      "%s not found" %>% sprintf(sQuote(filename)), 
      call. = FALSE
    )
  }
  function() {
    root <- dirname(filename)
    owd <- setwd(root)
    on.exit(setwd(owd))
    system2(basename(filename), wait = TRUE)
  }
}