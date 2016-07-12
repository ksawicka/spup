#' Executing selected model runs with uncertain inputs
#'
#' Function that allows user to run selected model number of times using MC
#' sample.
#'
#' \strong{model} has to be a user specific function that uses inputsample as
#' an argument.
#'
#' @usage spPropagate(model, n, ...)
#'
#' @param n Number of model runs.
#' @param ... Additional parameters.
#' @param a model, written as a user's specific function.
#'
#' @return
#' @author Kasia Sawicka, Gerard Heuvelink
#' @examples
#'
#' data(geul)
#' # Simulate pb:
#' geul.cormodel <- makecrm(acf0 = 0.35, range = 20, model = "Exp")
#' geul.pb <- defnummarspatial(uncertain = TRUE, sp.obj = geul.krig, crm = geul.cormodel, mask = geul.mask)
#' geul.sim.pb <- genSample(n = 5, uncert.object = geul.pb, samplemethod = "ugs", nmax = 20)
#' # Simulate sc:
#' geul.sc <- defnummarnonspatial(uncertain = TRUE, dist = "log", par = c(-2.958, 1.294))
#' geul.sim.sc <- genSample(n = 5, uncert.object = geul.sc, samplemethod = "randomSampling")
#' # Run propagation:
#' geul.propagate <- spPropagate(pb = geul.sim.pb, sc = geul.sim.sc, model = geulModel, n = 5)
#' head(geul.propagate)
#' str(geul.propagate)
#' summary(geul.propagate)
#'
#' @export
#' 
spPropagate <- function(model, n, parallel = FALSE, clusters, ...) {

  require(foreach)

  # if (isTRUE(parallel)) {
  #   if (clusters == 0) {
  #     stop("At least one Cluster must be set if sampling must be done in parallel.")
  #   }
  # }

  # CHECK: n cannot be larger than the shortest list of simulations.

  # run model once to determine the type of output to impose type of data in propagate.output object below
  # one_run <- model(...)
  
  
  # dots <- list(...)

  # for geul
  dots <- list(geul.sim.pb, geul.sim.sc)
  n_rows <- length(dots[[1]][[1]])

  if (!isTRUE(parallel)) {
    # dedicate space for the model output
    propagate_output <- matrix(data = NA_real_, nrow = n_rows, ncol = n)
    foreach(i = 1:n) %do% {
      propagate_output[, i] <- model(...)
    }
  }

    # }
    # else {
    #
    #   cl <- makeCluster(clusters)
    #   registerDoParallel(cl)
    #
    #   propagate_output <- numeric()
    #   foreach(i = 1:n, .export = Model) %dopar% {
    #     # propagate_output <- model(...)
    #   }
  propagate_output
}

# ## Examples:
# data(geul)
# # Simulate pb:
# geul.cormodel <- makecrm(acf0 = 0.35, range = 20, model = "Exp")
# geul.pb <- defnummarspatial(uncertain = TRUE, sp.obj = geul.krig, crm = geul.cormodel, mask = geul.mask)
# geul.sim.pb <- genSample(n = 5, uncert.object = geul.pb, samplemethod = "ugs", nmax = 20)
# # Simulate sc:
# geul.sc <- defnummarnonspatial(uncertain = TRUE, dist = "log", par = c(-2.958, 1.294))
# geul.sim.sc <- genSample(n = 5, uncert.object = geul.sc, samplemethod = "randomSampling")
# # Run propagation:
# geul.propagate <- spPropagate(pb = geul.sim.pb[[1]], sc = geul.sim.sc[1], model = geulModel, n = 5)
# head(geul.propagate)
# str(geul.propagate)
# summary(geul.propagate)