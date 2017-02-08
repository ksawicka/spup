#' Wrapper function for calling executables in R.
#'
#' @param filename 
#'
#' @return Executable output.
#' 
#' @examples
#' # save executable as a function in R
#' # note: dummy_model.exe needs to be created first by compiling dummy_model.c
#' dummy_model <- executable("./vignettes/examples/dummy_model.exe")
#' 
#' # create template
#' my_template <- template("./vignettes/examples/input.txt.template")
#' 
#' # render the template
#' render(my_template, b0 = 3.1, b1 = 4.2)
#' 
#' # run external model
#' dummy_model()
#' 
#' # read output (output file of dummy_model is "output.txt")
#' scan(file = "./vignettes/examples/output.txt", quiet = TRUE)
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