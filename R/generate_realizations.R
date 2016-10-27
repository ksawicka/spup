# source("R/randomSamp.R")
# source("R/stratsamp.R")
# source("R/crm2vgm.R")
# source("R/matrixReshuffle.R")
# source("R/lhs.R")

#' Generating Monte Carlo sample from an uncertain object
#'
#' Function that runs Monte Carlo simulations depending on the type of
#' uncertain object. Facilitates unconditional gausian simularion of errors for
#' spatially auto-correlated residulas, and random sampling, stratified
#' sampling and latin hypercube sampling if no spatial auto-correlation is
#' included.
#'
#' The function simulate realizations of residuals and calculates the
#' simulations of an uncertan object according to X = m + sd * \eqn{\epsilon}.
#'
#' Returned map of sd is based on sd calculated per grid cell for all
#' simulations of X.
#'
#' \strong{"ugs"} Unconditional gaussian simulation of spatially
#' auto-correlated errors.
#'
#' \strong{"stratifiedSampling"} Number of samples (n) must be dividable by the
#' number of quantiles to assure each quantile is evenly represented.
#'
#' \strong{"lhs"} Sampling method for at least two uncertain inputs. The
#' uncertain.object is then a list of two or more. It uses startified sampling
#' method to generate the inputs for the latin hypercude algorithm, hence the p
#' is restricted as above.
#'
#' @usage genSample(n, uncert_object, samplemethod, p = 0, ...)
#'
#' @param n Integer. Number of Monte Carlo realizations.
#' @param uncert_object Uncertain object of one of the classes:
#' "NumMarSpatial", "NumMarNonSpatial", "categorical", "joint?".
#' @param samplemethod "ugs" for spatially correlated errors, "randomSampling",
#' "stratifiedSampling", "lhs" if no spatial correlation of erros is
#' considered.
#' @param p A vector of quantiles. Optional. Only required if sample method is
#' "stratifiedSampling" or "lhs".
#' @param ...  Additional parameters that may be passed, e.g. to the predict in
#' the "ugs" method.
#'
#' @return An object of a class "SpatialMCSample" or "NonSpatialMCSample"
#' depending on the class of uncert_object. This is a list with the following
#' items: \item{X}{ Simulated realizations of the spatial uncertain object. }
#' \item{map.of.sd}{ A map of sd is based on sd calculated per grid cell for
#' all simulations of X. } \item{sampling.method}{ A string with a selected
#' sampling method. } \item{crm}{ Spatial correlogram model. The slot only
#' exists if spatial auto-correlation of errors is taken into account. }
#'
#' @author Kasia Sawicka, Stefan van Dam, Gerard Heuvelink
#'
#' @examples
#'
#'   ## Spatial objects examples
#'   # "ugs" example
#'   data(geul)
#'   geul.cormodel <- makecrm(acf0 = 0.35, range = 20, model = "Exp")
#'   geul.pb <- defnummarspatial(uncertain = TRUE, sp.obj = geul.krig, crm = geul.cormodel, mask = geul.mask)
#'   geul.sim <- genSample(n = 5, uncert_object = geul.pb, samplemethod = "ugs", nmax = 20)
#'
#'   # "randomSampling" example
#'   data(geul)
#'   geul.pb <- defnummarspatial(uncertain = TRUE, sp.obj = geul.krig, mask = geul.mask)
#'   geul.sim <- genSample(n = 5, uncert_object = geul.pb, samplemethod = "randomSampling")
#'
#'   # "startifiedSampling" example
#'   data(geul)
#'   geul.pb <- defnummarspatial(uncertain = TRUE, sp.obj = geul.krig, mask = geul.mask)
#'   geul.sim <- genSample(n = 5, uncert_object = geul.pb, samplemethod = "stratifiedSampling", p = 0:5/5)
#'
#'   # "lhs" example
#'   data(geul)
#'   geul.pb <- defnummarspatial(uncertain = TRUE, sp.obj = geul.krig, mask = geul.mask)
#'   geul.sc <- defnummarnonspatial(uncertain = TRUE, dist = "log", par = c(-2.958, 1.294))
#'   list.of.geul <- list(geul.pb = geul.pb, geul.sc = geul.sc)
#'   geul.sim <- genSample(n = 5, uncert_object = list.of.geul, samplemethod = "lhs", p = 0:5/5)
#'
#'   ## Skalar example
#'   geul.sc <- defnummarnonspatial(uncertain = TRUE, dist = "log", par = c(-2.958, 1.294))
#'   geul.sim <- genSample(n = 5, uncert_object = geul.sc, samplemethod = "randomSampling")
#'
#' @export

# library(foreach)
# library(gstat)
# library(magrittr)


generate_realizations <- function(uncert_object, n, samplemethod, p = 0, ...) {

  if (!class(uncert_object) == "list") {
    if (uncert_object[1] == FALSE)
      stop("object to sample from must be uncertain")
  }

  # ---------------------------------------------------------------------------
  # Case I - sample from spatial objects.
  # Case I.1: User only provides cor model and wants a map of dummy resid - method ugs

  if (samplemethod == "ugs") { 

    if (is.null(uncert_object$crm))
      stop("correlogram model is required for unconditional gaussian simulation")

    # Convert the uncert_object to gstat object so it works with predict.
    g_dummy <- gstat(formula = z~1,
                    dummy = TRUE,
                    beta = 0,
                    model = crm2vgm(uncert_object[[3]]),
                    ...)
    resid.samples <- predict(object = g.dummy,
                            newdata = uncert_object[[4]],
                            nsim = n)

    mean <- uncert_object[[2]][[1]]
    sd <- sqrt(uncert_object[[2]][[2]])
    mean.samples <- resid.samples
    mean.samples@data <- as.data.frame(apply(as.matrix(resid.samples@data),      # Change calling by name @data to call by number of slot
                                            MARGIN = 2, function(x) mean + sd*x))
    map.of.sd <- uncert_object[[4]]
    sd.samples <- apply(as.matrix(mean.samples@data), MARGIN = 1, sd)
    map.of.sd@data <- as.data.frame(sd.samples)
    samples <- list(X = mean.samples,
                   map.of.sd = map.of.sd,
                   sampling.method = samplemethod,
                   crm = uncert_object[[3]])
    class(samples) <- "SpatialMCSample"
  }


  # ===================UNCONDITIONAL GAUSSIAN SIMULATION (UGS) - END===================

  # Case 2: User provides raster/grid and no cor model.
  # Sampling done as below, using SRS or StratSamp.
  # KS: Can we have some statement per each piece of code what is it doing?
  # See for example defnummarspatialKasia.R


  # ===================RANDOM SAMPLING (randomSampling) - START===================
  if (samplemethod == "randomSampling") {
    # CHECK: what the class of the sample uncert_object is
    if (class(uncert_object) == "NumMarSpatial") {
      # For each grid cell, n realizations will be generated using randomSamp.R
      temp.samples <- foreach(a = uncert_object[[2]][[1]], b = sqrt(uncert_object[[2]][[2]]), .combine = rbind) %do% {
        randomSamp(a, b, n)
      }
      temp.samples <- temp.samples[,sample(ncol(temp.samples))]
      mean.samples <- uncert_object[[4]]
      mean.samples@data <- as.data.frame(temp.samples)
      map.of.sd <- uncert_object[[4]]
      sd.samples <- apply(temp.samples, MARGIN = 1, sd)
      map.of.sd@data <- as.data.frame(sd.samples)
      samples <- list(X = mean.samples, map.of.sd = map.of.sd, sampling.method = samplemethod)
      class(samples) <- "SpatialMCSample"
    }

    # CHECK: what the class of the uncert_object is
    if (class(uncert_object) == "NumMarNonSpatial") {
      # CHECK: what the distribution of the uncert_object is
      if (uncert_object$dist == "norm") {
        samples <- rnorm(n, uncert_object$par[1], uncert_object$par[2])
        samples <- sample(samples)
        # add sd?
      }

      # CHECK: what the distribution of the uncert_object is
      if (uncert_object$dist == "log") {
        samples <- rlnorm(n, uncert_object$par[1], uncert_object$par[2])
        samples <- sample(samples)
        # add sd?
      }

      # CHECK: what the distribution of the uncert_object is
      if (uncert_object$dist == "beta") {
        samples <- rbeta(n, uncert_object$par[1], uncert_object$par[2])
        samples <- sample(samples)
        # add sd?
      }
      samples <- list(X = samples, sampling.method = samplemethod) # add sd?
      class(samples) <- "NonSpatialMCSample"
    }
  } # END RANDOM SAMPLING (randomSampling)


  # ===================STRATIFIED SAMPLING (stratifiedSampling) - START=========
  # CHECK: which sample method the user chose
  if (samplemethod == "stratifiedSampling") {
    # CHECK: what the class of the sample uncert_object is
    if (class(uncert_object) == "NumMarSpatial") {
      if (n %% (length(p)-1) != 0) {
        stop("n should be divisable by the number of strata")
      } else {
        temp.samples <- foreach(a = uncert_object[[2]][[1]], b = sqrt(uncert_object[[2]][[2]]), .combine = rbind) %do% {
          as.numeric(stratsampSpatial(a, b, n/(length(p)-1), p))
        }

        temp.samples = temp.samples[,sample(ncol(temp.samples))]
        mean.samples = uncert_object[[4]]
        mean.samples@data = as.data.frame(temp.samples)
        map.of.sd = uncert_object[[4]]
        sd.samples <- apply(temp.samples, MARGIN = 1, sd)
        map.of.sd@data <- as.data.frame(sd.samples)
        samples = list(X = mean.samples, map.of.sd = map.of.sd, sampling.method = samplemethod)
        class(samples) <- "SpatialMCSample"
      }
    }

    if (class(uncert_object) == "NumMarNonSpatial") {
      if (n %% (length(p)-1) != 0) {
        stop("n should be divisable by the number of strata")
      } else {
        samples <- stratsamp(uncert_object, n/(length(p)-1), p)
        samples <- matrixReshuffle(samples)
        samples <- list(X = samples, sampling.method = samplemethod) # add sd?
        class(samples) <- "NonSpatialMCSample"
      }
    }
   } # END STRATIFIED SAMPLING (stratifiedSampling)



  # ===================LATIN HYPERCUBE SAMPLING (lhs) - START=================
  # CHECK: which sample method the user chose
  # Prepare uncertain objects as a list inputs for lhs().
  # Serves two types of objects for now: "NumMarSpatial" and "NumMarNonSpatial"
  if (samplemethod == "lhs") {
    if (class(uncert_object) == "list") { # add if length list > 1
      uncert.obj.names <- names(uncert_object)
      samples.to.lhs <- list()
      for (i in 1:length(uncert_object)) {
        if (class(uncert_object[[i]]) == "NumMarSpatial") {
          single.obj.samples <- foreach(a = uncert_object[[i]][[2]][[1]], b = sqrt(uncert_object[[i]][[2]][[2]]), .combine = rbind) %do% {
            as.numeric(stratsampSpatial(a, b, n/(length(p)-1), p))
          }

          rownames(single.obj.samples) <- NULL
          colnames(single.obj.samples) <- colnames(single.obj.samples, do.NULL = FALSE, prefix = "sim")
          single.obj.samples <- list(samples = single.obj.samples, n.sample = n, n.strata = (length(p)-1))
          class(single.obj.samples) <- c(class(uncert_object[[i]]), class(single.obj.samples))
          samples.to.lhs[i] <- list(single.obj.samples)
        }

        if (class(uncert_object[[i]]) == "NumMarNonSpatial") {
          if (n %% (length(p)-1) != 0) {
            stop("n should be divisable by the number of strata")
          } else {
            # .Random.seed <- tmp.seed
            single.obj.samples <- stratsamp(uncert_object[[i]], n/(length(p)-1), p)
            single.obj.samples <- list(samples = single.obj.samples, n.sample = n, n.strata = (length(p)-1))

            # Set the name for the class
            class(single.obj.samples) <- c(class(uncert_object[[i]]), class(single.obj.samples))
            samples.to.lhs[i] <- list(single.obj.samples)
          }
        }
      }

      # Run lhs algorythm using a list of uncertain objects, created above.
      lhs.samples <- lhs(samples.to.lhs)
      # Create an empty list to fill in below with output of lhs() run.
      samples <- list()

      # Fill in above list depending on type of object.
      for (i in 1:length(lhs.samples)) {
        if (class(lhs.samples[[i]])[1] == "NumMarSpatial") {
          mean.samples <- uncert_object[[i]][[2]]
          mean.samples@data <- as.data.frame(lhs.samples[[i]])
          map.of.sd = uncert_object[[i]][[4]]
          sd.samples <- apply(lhs.samples[[i]], MARGIN = 1, sd)
          map.of.sd@data <- as.data.frame(sd.samples)
          lhs.samples[[i]] <- list(X = mean.samples, map.of.sd = map.of.sd)
          class(lhs.samples[[i]]) <- "SpatialMCSample"
        } else {
          class(lhs.samples[[i]]) <- "NonSpatialMCSample"
        }
        samples[[i]] <- list(lhs.samples[[i]])
      }
      names(samples) <- uncert.obj.names
      samples = c(samples, sampling.method = samplemethod)

    } else{
      stop("For \"lhs\" uncert_object must be a list of minimum length of two")
    }
  } # END LATIN HYPERCUBE SAMPLING (lhs)

 samples

}
