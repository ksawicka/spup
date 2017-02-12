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
library(magrittr) # piping
library(whisker)  # templating
library(readr)    # fast I/O (huge files)
library(purrr)    # functional programming tools
library(dplyr)    # a grammar of data manipulation

setwd("D:/GD/Wageningen/QUICS/RPackage/spup/vignettes")

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

## ------------------------------------------------------------------------
read_lines("examples/input.txt")

## ------------------------------------------------------------------------
read_lines("examples/input.txt.template")

## ------------------------------------------------------------------------

render <- function(x, ...) {
    UseMethod("render")
}

render.character <- function(x, ...) {
    dots <- list(...)
    if (length(dots) == 0L) {
        return(x)
    }
    x %>% 
        whisker.render(dots)
}

render.template <- function(x, ...) {
    sub(pattern = "\\.template$", replacement = "", x = x) %T>% 
        walk(function(x) {
            readLines(paste(x, "template", sep = "."), warn = FALSE) %>% 
            render(...) %>%
            writeLines(x)
        }) %>%
        as.character
}


## ------------------------------------------------------------------------
my_template <- "Hello {{name}}. How are you doing?"

my_template %>% 
    render(name = "Kasia")


## ------------------------------------------------------------------------
my_template <- c(
    "| x | y |",
    "|---|---|",
    "{{#MY_TABLE}}",
    "| {{X}} | {{Y}} |",
    "{{/MY_TABLE}}"
)

my_table <- data.frame(X = 1:5, Y = letters[1:5])
my_table

my_template %>% 
    render(MY_TABLE = unname(rowSplit(my_table))) %>%
    cat

## ------------------------------------------------------------------------
my_template <- template("examples/input.txt.template")

## ------------------------------------------------------------------------
my_template %>% 
    read_lines

## ------------------------------------------------------------------------
my_template %>% 
    render(b0 = 3, b1 = 4)

## ------------------------------------------------------------------------
my_template %>% 
    render(b0 = 3, b1 = 4) %>%
    read_lines

## ------------------------------------------------------------------------
executable <-
function(filename) {
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

## ------------------------------------------------------------------------
dummy_model <- executable("examples/dummy_model.exe")

## ------------------------------------------------------------------------
# create template
my_template <- template("examples/input.txt.template")

# render the template
render(my_template, b0 = 3.1, b1 = 4.2)

# run external model
dummy_model()

# read output (output file of kasia-model is "output.txt")
scan(file = "examples/output.txt", quiet = TRUE)

## ------------------------------------------------------------------------

n_realizations <- 100

n_realizations %>%
    rerun({
        # render template
        render(my_template, b0 = rnorm(n = 1), b1 = runif(n = 1))
        
        # run model
        dummy_model()

        # read output
        scan("examples/output.txt", quiet = TRUE)
    }) %>%
    set_names(paste0("r", 1:n_realizations)) %>% 
    as_data_frame %>%
    apply(MARGIN = 1, FUN = quantile)    

