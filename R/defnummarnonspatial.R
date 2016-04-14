# Kasia Sawicka & Stefan van Dam
# January 2016

# This functions takes a distribution wth its parameters as input and makes an S4 class of it

defnummarnonspatial <- function(uncertain = FALSE, dist, par, cor) {
  um <- list(uncertain = uncertain,
             dist = dist,
             par = par)

  # Set the name for the class
  class(um) <- "NumMarNonSpatial"
  um

}

# Examples
mynorm <- defnummarnonspatial(uncertain = TRUE, dist = "norm", par = c(70,3))
mynorm <- defnummarnonspatial(uncertain = TRUE, dist = "normal", par = c(70,3))

# logparams <- transformLog(0.120,0.250)
# mylog <- defnummarnonspatial(uncertain = TRUE, dist = "log", par = c(logparams[2], logparams[1]))

#mybeta <- defnummarnonspatial(uncertain = TRUE, dist = "beta", par = c(50, 5))
