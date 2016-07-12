# Try "pipes" for spatial case:

library(magrittr)
library(purrr)
library(ggplot2)

### Code base

# CLASS
setClass(
  Class = "Distribution", 
  slots =  c(
    distributions = "list"
  )
)

# constructor
Distribution <- function(...) {
  new(
    Class = "Distribution", 
    distributions = list(...)
  )
}

# METHODS

setGeneric(
  name = "draw", 
  def = function(x, n, ...) {
    standardGeneric("draw")
  }
)

setMethod(
  f = "draw", 
  signature = signature(x = "Distribution", n = "numeric"), 
  definition = function(x, n, ...) {
    x@distributions %>%
      invoke_map(n = n) %>%
      as.data.frame
  }
)

setMethod(
  f = "draw", 
  signature = signature(x = "Distribution", n = "missing"), 
  definition = function(x, ...) {
    draw(x, n = 1)
  }
)


setGeneric(
  name = "propagate", 
  def = function(x, f) {
    standardGeneric("propagate")
  })

setMethod(
  f = "propagate", 
  signature = signature(x = "list", f = "function"), 
  definition = function(x, f) {
    invoke(f, x)
  }
)


  
  ### A spatial MISO-model with uncorrelated inputs
  
  
# define MISO-model for Geul
geulModel <- function(pb, sc) {
  pb * sc
}

data(geul)
# Old version:
geul.cormodel <- makecrm(acf0 = 0.35, range = 20, model = "Exp")
geul.pb <- defnummarspatial(uncertain = TRUE, sp.obj = geul.krig, crm = geul.cormodel, mask = geul.mask)
# geul.sim.pb <- genSample(n = 5, uncert.object = geul.pb, samplemethod = "ugs", nmax = 20)
 
ugs <- function(n, mean, sd, mask, crm) {
  
  g_dummy <- gstat(formula = z~1,
                   dummy = TRUE,
                   beta = 0,
                   model = crm2vgm(crm),
                   nmax = 24)
  resid_samples <- predict(object = g_dummy,
                           newdata = mask,
                           nsim = n)
  
  mean <- mean[[1]]
  sd <- sqrt(sd[[1]])
  mean_samples <- resid_samples
  mean_samples@data <- as.data.frame(apply(as.matrix(resid_samples@data),
                                           MARGIN = 2, function(x) mean + sd*x))
  map_of_sd <- mask
  sd_samples <- apply(as.matrix(mean_samples@data), MARGIN = 1, sd)
  map_of_sd@data <- as.data.frame(sd_samples)
  # samples <- list(X = mean_samples,
  #                 map_of_sd = map_of_sd,
  #                 crm = crm)
  # samples
  mean_samples
}

a <- ugs(n = 10, mean = geul.krig[1], sd = geul.krig[2], mask = geul.mask, crm = geul.cormodel)  

# define bivariate distribution
# note: the argument names in bar and distr are identical
distr <- Distribution(
  pb = function(n) {
    ugs(n = n, mean = geul.krig[1], sd = geul.krig[2], mask = geul.mask, crm = geul.cormodel)
    # genSample(n = n, uncert.object = geul.pb, samplemethod = "ugs", nmax = 20)
    # Error in as.data.frame.default(x[[i]], optional = TRUE, stringsAsFactors = stringsAsFactors) : 
    #   cannot coerce class ""SpatialMCSample"" to a data.frame 
    # Same happens with "SpatialCorrelogramModel" if above S3 class assigning switched off
  }, 
  sc = function(n) {
    rbeta(n = n, shape = 2, shape2 = 5)
  }
)

# draw single realization
draw(distr) 
a <- draw(distr)
str(a)

# draw multiple realizations
draw(distr, n = 10)

# error propagation
par(mfrow = c(1, 1))
distr %>% 
  draw(1000) %>% 
  propagate(geulModel) # %>% 
  # hist(main = "MISO-output")


