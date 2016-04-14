# Function for propagation run - run the model
# with simulations from genSample().



# geulModel <- function(pb, sc) {
#   pb * sc
# }



# dots <- list(geul.sim.pb, geul.sim.sc)

spPropagate <- function(..., model, n) {



  # model_args <- as.list(args(model))
  # model_args <- formals(model)



  dots <- list(...)

  n_rows <- length(dots[[1]][[1]])

  propagate.output <- matrix(data = NA_real_, nrow = n_rows, ncol = n)
  for (i in 1:n) {
    # model(...) # here is the problem, how to pass one by one simulations?
    # For Geul:
    propagate.output[, i] <- model(dots[[1]][[1]][[i]], dots[[2]][[1]][[i]])
  }

  propagate.output
} # END propagate()



## Example:
# data(geul)
#
# # Simulate pb:
# geul.cormodel <- makecrm(acf0 = 0.35, range = 20, model = "Exp")
# geul.pb <- defnummarspatial(uncertain = TRUE, sp.obj = geul.krig, crm = geul.cormodel, mask = geul.mask)
# geul.sim.pb <- genSample(n = 5, uncert.object = geul.pb, samplemethod = "ugs", nmax = 20)
#
# # Simulate sc:
# geul.sc <- defnummarnonspatial(uncertain = TRUE, dist = "log", par = c(-2.958, 1.294))
# geul.sim.sc <- genSample(n = 5, uncert.object = geul.sc, samplemethod = "randomSampling")
#
# # Run propagation:
# geul.propagate <- spPropagate(pb = geul.sim.pb, sc = geul.sim.sc, model = geulModel, n = 5)
# head(geul.propagate)
# str(geul.propagate)
# summary(geul.propagate)





