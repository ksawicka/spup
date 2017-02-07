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

## ---- fig.width = 5, fig.height = 5--------------------------------------
# load packages
library(sp)
library(spup)

# load and view the data
data(Rotterdam)
plot(woon, main = "Neighbourhood", cex.main = 1)
head(woon@data)

## ------------------------------------------------------------------------

# define uncertainty model for the bulding function
woonUM <- defineUM(TRUE, categories = c(1,2,3), cat_prob = woon[, c(4:6)])
class(woonUM)


## ---- fig.width = 7, fig.height = 7--------------------------------------
# create possible realizations of the building function
woon_sample <- genSample(woonUM, 10, asList = FALSE)
class(woon_sample)

# view several realizations
spplot(woon_sample[c(3,4,1,2)], main = list(label = "Examples of the building function realizations", cex = 1))

## ------------------------------------------------------------------------
# view the model
tax

## ------------------------------------------------------------------------
# coerce  SpatialPolygonDataFrame to a list of individual SpatialPolygonDataFrames
woon_sample <- map(1:ncol(woon_sample), function(x){woon_sample[x]})

# or sample from uncertain input and save it in a list
woon_sample <- genSample(UMobject = woonUM, n = 10, asList = TRUE)
class(woon_sample)

## ------------------------------------------------------------------------
for (i in 1:10) names(woon_sample[[i]]) <- "Function"

## ------------------------------------------------------------------------
# run uncertainty propagation
tax_sample <- propagate(woon_sample, model = tax, n = 10)
tax_sample
summary(unlist(tax_sample))

