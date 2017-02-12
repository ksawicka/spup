## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(
    comment = NA,
    quiet = TRUE,
    progress = FALSE,
    tidy = FALSE,
    cache = FALSE,
    message = FALSE,
    error = FALSE, # FALSE: always stop execution.
    warning = TRUE,
    dpi = 100
)

## ---- echo = FALSE-------------------------------------------------------
knitr::opts_knit$set(global.par = TRUE)

## ---- echo = FALSE-------------------------------------------------------
par(mar = c(3, 3, 2, 2), mgp = c(1.7, 0.5, 0), las = 1, cex.main = 1, tcl = -0.2, cex.axis = 0.8,
    cex.lab = 0.8)

## ------------------------------------------------------------------------
# library(magrittr) # piping
# library(whisker)  # templating
# library(readr)    # fast I/O (huge files)
# library(purrr)    # functional programming tools
# library(dplyr)    # a grammar of data manipulation

## ------------------------------------------------------------------------
template <- function(filenames) {
    filenames %>% 
        walk(
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
        walk(
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

print.template <- function(x, ...) {
    cat("container containing the following template(s):\n")
    print(as.character(x))
}

