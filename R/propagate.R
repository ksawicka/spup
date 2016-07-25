#' Executing selected model runs with uncertain inputs
#'
#' Function that allows user to run selected model number of times using MC
#' sample.
#'
#' \strong{model} has to be a user specific function that uses inputsample as
#' an argument.
#'
#' @usage propagate(model, n, ...)
#'
#' @param n Number of model runs.
#' @param ... Additional parameters, e.g. model inputs/parameters.
#' @param a model, written as a user's specific function.
#'
#' @return
#' @author Kasia Sawicka, Dennis Walvoort, Gerard Heuvelink
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
propagate <- function(model, n, ...) {

 
}

