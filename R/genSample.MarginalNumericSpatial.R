#' Generating Monte Carlo sample from an uncertain object of a class 
#' 'MarginalNumericSpatial'
#'
#' Function that runs Monte Carlo simulations depending on the type of
#' uncertain object. Facilitates unconditional gausian simulation of errors for
#' spatially auto-correlated residulas, and random sampling, stratified
#' sampling if no spatial auto-correlation is included.
#'
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
#' @usage genSample(UMobject, n, samplemethod, p = 0, ...)
#'
#' @param UMobject uncertain object defined using defineUM().
#' @param n Integer. Number of Monte Carlo realizations.
#' @param samplemethod "ugs" for spatially correlated errors, "randomSampling" and
#' "stratifiedSampling" if no spatial correlation of errors is
#' considered.
#' @param p A vector of quantiles. Optional. Only required if sample method is
#' "stratifiedSampling" or "lhs".
#' @param var Logical. Specifies if variance for X realizations should be also returned.
#' @param ...  Additional parameters that may be passed, e.g. in
#' the "ugs" method. See examples.
#' 
#' @return A Monte Carlo sample of uncertain input of a class of distribution
#' parameters. 
#' 
#' @author Kasia Sawicka, Stefan van Dam, Gerard Heuvelink
#' 
#' @examples
#'
#' # load data
#' data(DEM)
#' 
#' # "ugs" method example
#' dem_crm <- makecrm(acf0 = 0.78, range = 321, model = "Exp")
#' demUM <- defineUM(uncertain = TRUE, distribution = "norm", 
#'                    distr_param = c(dem30m, dem30m_sd), crm = dem_crm)
#' dem_sample <- genSample(UMobject = demUM, n = 5, samplemethod = "ugs", nmax = 20)
#' str(dem_sample)
#'
#' # "randomSampling" method example
#' demUM <- defineUM(uncertain = TRUE, distribution = "norm", distr_param = c(dem30m, dem30m_sd))
#' dem_sample <- genSample(UMobject = demUM, n = 5, samplemethod = "randomSampling")
#' str(dem_sample)
#' 
#' demUM <- defineUM(uncertain = TRUE, distribution = "beta", distr_param = c(dem30m, dem30m_sd, dem30m))
#' dem_sample <- genSample(UMobject = demUM, n = 5, samplemethod = "randomSampling")
#' str(dem_sample)
#'
#' # "startifiedSampling" method example
#' demUM <- defineUM(uncertain = TRUE, distribution = "norm", distr_param = c(dem30m, dem30m_sd))
#' dem_sample <- genSample(UMobject = demUM, n = 5, samplemethod = "stratifiedSampling", p = 0:5/5)
#' str(dem_sample)
#' 
#' demUM <- defineUM(uncertain = TRUE, distribution = "exp", distr_param = c(dem30m), lower.tail = FALSE)
#' dem_sample <- genSample(UMobject = demUM, n = 5, samplemethod = "stratifiedSampling", p = 0:5/5)
#' str(dem_sample)
#' 
#' # load data
#' data(dummyraster)
#' rastUM <- defineUM(uncertain = TRUE, distribution = "norm", distr_param = c(rast_mean, rast_sd))
#' rast_sample <- genSample(UMobject = rastUM, n = 5, samplemethod = "randomSampling")
#' str(rast_sample)
#' class(rast_sample)
#' 
#' # raster with auto-correlation
#' rast_crm <- makecrm(acf0 = 0.6, range = 6, model = "Sph")
#' rastUM <- defineUM(uncertain = TRUE, distribution = "norm", distr_param = c(rast_mean, rast_sd),
#' crm = rast_crm)
#' rast_sample <- genSample(UMobject = rastUM, n = 5, samplemethod = "ugs", asList = F)
#' str(rast_sample)
#' class(rast_sample)
#' 
#'
#' @export
genSample.MarginalNumericSpatial <- function(UMobject, n, samplemethod, p = 0, asList = TRUE, ...) {
  
  stopifnot(UMobject$uncertain == TRUE)
  distribution <- UMobject[[2]]
  distr_param <- UMobject[[3]]
  crm <- UMobject[[4]]
  dots <- list(...)
  
  ### UGS ----------------------------------------------------------------------------
  if (samplemethod == "ugs") { 
    if (is.null(crm))
      stop("Correlogram model is required for the 'ugs' sampling method.")
    if (distribution != "norm")
      stop("Only normal distribution can be assumed in the 'ugs' method.")
    if (is(distr_param[[1]], "RasterLayer")) {
      original_class <- "RasterLayer"
      distr_param[[1]] <- as(distr_param[[1]], "SpatialGridDataFrame")
      distr_param[[2]] <- as(distr_param[[2]], "SpatialGridDataFrame")
    } else {original_class <- "SpatialDF"}
    for (i in 1:2) {
      assign(paste0("distr_param", i), distr_param[[i]])
    }
    mask <- distr_param1
    g <- gstat::gstat(formula = z~1, dummy = TRUE, beta = 0, model = crm2vgm(crm), ...)
    epsilon_sample <- predict(object = g, newdata = mask, nsim = n)
    X_sample <- epsilon_sample
    # if it is Spatial data frame
    X_sample@data <- as.data.frame(apply(as.matrix(epsilon_sample@data), MARGIN = 2,
                                  function(x) distr_param1@data + distr_param2@data*x))
    names(X_sample@data) <- paste("sim", c(1:n), sep = "")

    if (original_class == "RasterLayer") {
      X_sample <- raster::stack(X_sample)
      if (asList == TRUE) {
        l <- list()
        l[[1]] <- X_sample@layers[[1]]
        for (i in 2:nlayers(X_sample)) {
          l[[i]] <- X_sample@layers[[i]]
        }
        X_sample <- l
      }
    }
    
    if (asList == TRUE) {
      X_sample <- map(1:n, function(x){X_sample[x]}) # convert SpGridDF to list
    }
  }
  
  ### RANDOM SAMP ---------------------------------------------------------------------
  if (samplemethod == "randomSampling") {
    if (class(distr_param[[1]]) != "RasterLayer") { # so all other spatial classes
      ds <- function(x) {
        parameters <- x
        distribution_sampling(n, distribution, parameters)
      }
      in1df <- do.call("cbind", distr_param)
      in1mtx <- as.matrix(in1df@data)
      temp_samples <- t(apply(in1mtx, MARGIN = 1, ds))
      X_sample <- distr_param[[1]]
      X_sample@data <- as.data.frame(temp_samples)
      names(X_sample@data) <- paste("sim", c(1:n), sep = "")
      if (asList == TRUE) {
        X_sample <- map(1:n, function(x){X_sample[x]})
      }
    } else {
      in1stack <- raster::stack(distr_param)
      outstack <- distribution_sampling_raster(distribution, parameters_stack = in1stack)
      for (i in 1:(n-1)) {
        outstack_i <- distribution_sampling_raster(distribution, parameters_stack = in1stack)
        outstack <- raster::stack(outstack, outstack_i)
      }
      X_sample <- outstack
      if (asList == TRUE) {
        l <- list()
        l[[1]] <- X_sample@layers[[1]]
        for (i in 2:nlayers(X_sample)) {
          l[[i]] <- X_sample@layers[[i]]
        }
        X_sample <- l  
      }
    }
  }
  
  ### STRATIFIED SAMP ------------------------------------------------------------------
  if (samplemethod == "stratifiedSampling") {
    if (n %% (length(p)-1) != 0)
      stop("n should be divisable by the number of strata")
    if (class(distr_param[[1]]) != "RasterLayer") {
      stsS <- function(x, ...) {
        parameters <- x
        as.numeric(stratsamp(n = n/(length(p)-1), distribution, parameters, p, ...))
      }
      in1df <- do.call("cbind", distr_param)
      in1mtx <- as.matrix(in1df@data)  
      temp_samples <- t(apply(in1mtx, MARGIN = 1, stsS))
      X_sample <- distr_param[[1]]
      X_sample@data <- as.data.frame(temp_samples)
      names(X_sample@data) <- paste("sim", c(1:n), sep = "")
      if (asList == TRUE) {
        X_sample <- map(1:n, function(x){X_sample[x]})
      }
    } else {
      print("Implement it for rasters.")
    }  
  }
  X_sample
} 

