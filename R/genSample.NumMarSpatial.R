# KS 05.02.16: some description needed here, will be used for .Rd files later:

# Function to generate realizations of user inputs to run any Monte Carlo analysis with

# n - number of realizations the user wants. Required

# uncert.object - uncertain uncert.object defined using defnummarspatial or defnummarnonspatial. Can be one object
# for simple random sampling or a list of minimum 2 for LHS.

# samplemethod - optional and only in use when sampling from uncorrelated grid.
#                Possible methods:
#                   "ugs" - unconditional spatial simulations of residuals,
#                   "randomSampling" -
#                   "stratifiedSampling" -
#                   "lhs" -


# p - optional. Is list of probailities used to define the quantiles.
# Only required if sample method is stratifiedSampling or LHS.


# ... passes any arguments thta may be required by other finctions inside,
# e.g. will allow to specify nmax in gstat() bit in "ugs" bit.
# Could do the same with argument beta here used in gstat, but then don't need
# argument resid.mean in defnummarspatial().

# What the function returns is based on the class of the uncert.object:
# 1. If uncert.object is spatial and it contains correlation, then the output contains Krige maps
# 2. If uncert.object is spatial, but does not contain correlation, the output is a matrix?
# Each collumn then contains one sample map.
# 3. If uncert.object is non-spatial, the output contains a list(?) of realizations

source("R/randomSamp.R")
source("R/stratsamp.R")
source("R/crm2vgm.R")
source("R/matrixReshuffle.R")
source("R/lhs.R")

library(foreach) # KS: find out how to have it automatically dependent within the function.
library(gstat)


genSample <- function(uncert.object, n, samplemethod, p = 0, ...) UseMethod("genSample")

genSample.NumMarSpatial <- function(uncert.object, n, samplemethod, p = 0, ...) {

  if (!class(uncert.object) == "list") {
    if (uncert.object[1] == FALSE)
      stop("object to sample from must be uncertain")
  }


  # ===================UNCONDITIONAL GAUSSIAN SIMULATION (UGS) - START===================
  # CHECK - what is present in our uncertain uncert.object created by defnummarspatial


  # Sampling done using predict().
  if (samplemethod == "ugs") { # ugs - unconditional spatial simulations of residuals

    # CHECK: if crm provided
    if (is.null(uncert.object$crm))
      stop("correlogram model is required for unconditional gaussian simulation")

    # Convert the uncert.object to gstat object so it works with predict.
    g.dummy <- gstat(formula = z~1,
                    # locations = ~x+y, # KS: is it always x and y?
                    dummy = TRUE,
                    beta = 0,
                    model = crm2vgm(uncert.object[[3]]),
                    ...)
    resid.samples <- predict(object = g.dummy,
                            newdata = uncert.object[[4]],
                            nsim = n)

    mean <- uncert.object[[2]][[1]]
    sd <- sqrt(uncert.object[[2]][[2]])
    mean.samples <- resid.samples
    mean.samples@data <- as.data.frame(apply(as.matrix(resid.samples@data),
                                            MARGIN = 2, function(x) mean + sd*x))
    map.of.sd <- uncert.object[[4]]
    sd.samples <- apply(as.matrix(mean.samples@data), MARGIN = 1, sd)
    map.of.sd@data <- as.data.frame(sd.samples)
    samples <- list(X = mean.samples,
                   map.of.sd = map.of.sd,
                   sampling.method = samplemethod,
                   crm = uncert.object[[3]])
    class(samples) <- "SpatialMCSample"
  }


  # ===================RANDOM SAMPLING (randomSampling) - START===================

  if (samplemethod == "randomSampling") {
      # For each grid cell, n realizations will be generated using randomSamp.R
      temp.samples <- foreach(a = uncert.object[[2]][[1]], b = sqrt(uncert.object[[2]][[2]]), .combine = rbind) %do% {
      randomSamp(a, b, n)
    }
    temp.samples <- temp.samples[,sample(ncol(temp.samples))]
    mean.samples <- uncert.object[[4]]
    mean.samples@data <- as.data.frame(temp.samples)
    map.of.sd <- uncert.object[[4]]
    sd.samples <- apply(temp.samples, MARGIN = 1, sd)
    map.of.sd@data <- as.data.frame(sd.samples)
    samples <- list(X = mean.samples, map.of.sd = map.of.sd, sampling.method = samplemethod)
    class(samples) <- "SpatialMCSample"
  }


  # ===================STRATIFIED SAMPLING (stratifiedSampling) - START=========

  if (samplemethod == "stratifiedSampling") {
    if (n %% (length(p)-1) != 0) {
      stop("n should be divisable by the number of strata")
    } else {
      temp.samples <- foreach(a = uncert.object[[2]][[1]], b = sqrt(uncert.object[[2]][[2]]), .combine = rbind) %do% {
        as.numeric(stratsampSpatial(a, b, n/(length(p)-1), p))
      }
      temp.samples <- temp.samples[,sample(ncol(temp.samples))]
      mean.samples <- uncert.object[[4]]
      mean.samples@data <- as.data.frame(temp.samples)
      map.of.sd <- uncert.object[[4]]
      sd.samples <- apply(temp.samples, MARGIN = 1, sd)
      map.of.sd@data <- as.data.frame(sd.samples)
      samples <- list(X = mean.samples, map.of.sd = map.of.sd, sampling.method = samplemethod)
      class(samples) <- "SpatialMCSample"
    }
  }


  # ===================LATIN HYPERCUBE SAMPLING (lhs) - START=================
  # # CHECK: which sample method the user chose
  # # Prepare uncertain objects as a list inputs for lhs().
  # # Serves two types of objects for now: "NumMarSpatial" and "NumMarNonSpatial"
  #
  # if (samplemethod == "lhs") {
  #   if (class(uncert.object) == "list") { # add if length list > 1
  #     uncert.obj.names <- names(uncert.object)
  #     samples.to.lhs <- list()
  #     for (i in 1:length(uncert.object)) {
  #       if (class(uncert.object[[i]]) == "NumMarSpatial") {
  #         single.obj.samples <- foreach(a = uncert.object[[i]][[2]][[1]], b = sqrt(uncert.object[[i]][[2]][[2]]), .combine = rbind) %do% {
  #           as.numeric(stratsampSpatial(a, b, n/(length(p)-1), p))
  #         }
  #
  #         rownames(single.obj.samples) <- NULL
  #         colnames(single.obj.samples) <- colnames(single.obj.samples, do.NULL = FALSE, prefix = "sim")
  #         single.obj.samples <- list(samples = single.obj.samples, n.sample = n, n.strata = (length(p)-1))
  #         class(single.obj.samples) <- c(class(uncert.object[[i]]), class(single.obj.samples))
  #         samples.to.lhs[i] <- list(single.obj.samples)
  #       }
  #
  #
  #     }
  #
  #     # Run lhs algorythm using a list of uncertain objects, created above.
  #     lhs.samples <- lhs(samples.to.lhs)
  #     # Create an empty list to fill in below with output of lhs() run.
  #     samples <- list()
  #
  #     # Fill in above list depending on type of object.
  #     for (i in 1:length(lhs.samples)) {
  #       if (class(lhs.samples[[i]])[1] == "NumMarSpatial") {
  #         mean.samples <- uncert.object[[i]][[2]]
  #         mean.samples@data <- as.data.frame(lhs.samples[[i]])
  #         map.of.sd = uncert.object[[i]][[4]]
  #         sd.samples <- apply(lhs.samples[[i]], MARGIN = 1, sd)
  #         map.of.sd@data <- as.data.frame(sd.samples)
  #         lhs.samples[[i]] <- list(X = mean.samples, map.of.sd = map.of.sd)
  #         class(lhs.samples[[i]]) <- "SpatialMCSample"
  #       }
  #       samples[[i]] <- list(lhs.samples[[i]])
  #     }
  #     names(samples) <- uncert.obj.names
  #     samples <- c(samples, sampling.method = samplemethod)
  #
  #   } else{
  #     stop("For \"lhs\" uncert.object must be a list of minimum length of two")
  #   }
  # }

  # ===================LATIN HYPERCUBE SAMPLING (lhs) - END=================

 samples

} # END genSample()





# ===================Examples===================
# data(geul)
# geul.cormodel <- makecrm(acf0 = 0.35, range = 20, model = "Exp")
# geul.pb <- defnummarspatial(uncertain = TRUE, sp.obj = geul.krig, crm = geul.cormodel, mask = geul.mask)
# geul.sim <- genSample(n = 5, uncert.object = geul.pb, samplemethod = "ugs", nmax = 20)
