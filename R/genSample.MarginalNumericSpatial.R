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
#' @param UMobject uncertain object defined using defineUM().
#' @param n Integer. Number of Monte Carlo realizations.
#' @param samplemethod "ugs" for spatially correlated errors, "randomSampling" and
#' "stratifiedSampling" if no spatial correlation of errors is
#' considered.
#' @param p A vector of quantiles. Optional. Only required if sample method is
#' "stratifiedSampling" or "lhs".
#' @param asList logical. If asList = TRUE returns list of all samples as a list. 
#' If asList = FALSE returns samples in a format of distribution parameters in UMobject.
#' @param debug.level integer; set gstat internal debug level, see below for useful values. 
#' If set to -1 (or any negative value), a progress counter is printed.
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
#' set.seed(12345)
#' # load data
#' data(dem30m, dem30m_sd)
#' 
#' # "ugs" method example
#' dem_crm <- makecrm(acf0 = 0.78, range = 321, model = "Exp")
#' demUM <- defineUM(uncertain = TRUE, distribution = "norm", 
#'                    distr_param = c(dem30m, dem30m_sd), crm = dem_crm)
#' \dontrun{
#' dem_sample <- genSample(UMobject = demUM, n = 5, samplemethod = "ugs", nmax = 20, asList = FALSE)
#' str(dem_sample)
#' }
#'
#' # "randomSampling" method example
#' demUM <- defineUM(uncertain = TRUE, distribution = "norm", distr_param = c(dem30m, dem30m_sd))
#' \dontrun{
#' dem_sample <- genSample(UMobject = demUM, n = 5, samplemethod = "randomSampling",asList = FALSE)
#' str(dem_sample)
#' }
#' 
#' demUM <- defineUM(uncertain = TRUE, distribution = "beta",
#'                   distr_param = c(dem30m, dem30m_sd, dem30m))
#' \dontrun{
#' dem_sample <- genSample(UMobject = demUM, n = 5, samplemethod = "randomSampling")
#' str(dem_sample)
#' }
#'
#' # "startifiedSampling" method example
#' demUM <- defineUM(uncertain = TRUE, distribution = "norm", distr_param = c(dem30m, dem30m_sd))
#' \dontrun{
#' dem_sample <- genSample(UMobject = demUM, n = 5, samplemethod = "stratifiedSampling", p = 0:5/5)
#' str(dem_sample)
#' }
#' 
#' demUM <- defineUM(uncertain = TRUE, distribution = "exp",
#'                   distr_param = c(dem30m), lower.tail = FALSE)
#' \dontrun{
#' dem_sample <- genSample(UMobject = demUM, n = 5, samplemethod = "stratifiedSampling", p = 0:5/5)
#' str(dem_sample)
#' }
#' 
#' # Examples with rasters
#' # ugs (raster with auto-correlation)
#' data(OC, OC_sd)
#' OC_crm <- makecrm(acf0 = 0.6, range = 1000, model = "Sph")
#' OC_UM <- defineUM(TRUE, distribution = "norm", distr_param = c(OC, OC_sd), crm = OC_crm, id = "OC")
#' class(OC_UM)
#' \dontrun{
#' some_sample <- genSample(OC_UM, n = 5, "ugs", nmax=20)
#' some_sample
#' }
#' 
#' @importFrom gstat gstat
#' @importFrom raster stack
#' @importFrom purrr map
#' @importFrom methods as
#'
#' @export
genSample.MarginalNumericSpatial <- function(UMobject, n, samplemethod, p = 0, asList = TRUE, debug.level = 1, ...) {
  
  # PRELIMINARIES -------------------------------------------------------------------
  # ---------------------------------------------------------------------------------
  
  stopifnot(UMobject$uncertain == TRUE)
  
  # extract information from UMobject
  distribution <- UMobject[[2]]
  distr_param <- UMobject[[3]]
  crm <- UMobject[[4]]
  
  
  ### UGS ----------------------------------------------------------------------------
  if (samplemethod == "ugs") { 
    if (is.null(crm))
      stop("Correlogram model is required for the 'ugs' sampling method.")
    if (distribution != "norm")
      stop("Only normal distribution can be assumed in the 'ugs' method.")
    
    # recognise if dealing with rester or spatial data frame objects,
    # if raster then converst it to spatial grid
    if (is(distr_param[[1]], "RasterLayer")) {
      original_class <- "RasterLayer"
      distr_param[[1]] <- as(distr_param[[1]], "SpatialGridDataFrame")
      distr_param[[2]] <- as(distr_param[[2]], "SpatialGridDataFrame")
    } else {original_class <- "SpatialDF"}
    
    # n_param <- length(distr_param)
    # # split distr_param into separate objects to ease code below
    # for (i in 1:n_param) {
    #   assign(paste0("distr_param", i), distr_param[[i]])
    # }
    
    mask <- distr_param[[1]] # assign geometry 
    
    # create gstat object
    g <- gstat::gstat(formula = z~1, dummy = TRUE, beta = 0, model = crm2vgm(crm), ...)
    
    # simulate epsilon
    epsilon_sample <- predict(object = g, newdata = mask, nsim = n, debug.level = debug.level)
    
    # calculate Z = m + sd*epsilon
    X_sample <- epsilon_sample # assign geometry
    X_sample@data <- as.data.frame(apply(as.matrix(epsilon_sample@data), MARGIN = 2,
                                  function(x) distr_param[[1]]@data + distr_param[[2]]@data*x))
    
    # sort out names
    if (!is.null(UMobject$id)) {
      names(X_sample@data) <- paste(UMobject$id, ".sim", c(1:n), sep = "")
    } else {
      names(X_sample@data) <- paste("sim", c(1:n), sep = "")}

    # sort out final product depending on if Raster or spatial data frame
    # and if object to be returned as list
    if (original_class == "RasterLayer") {
      X_sample <- raster::stack(X_sample)
      if (asList == TRUE) {
        X_sample <- purrr::map(1:n, function(x){X_sample[[x]]})
      }
    } else if (asList == TRUE) {
      X_sample <- purrr::map(1:n, function(x){X_sample[x]}) # convert SpGridDF to list
    }
  } # end sampling with "ugs"
  
  ### RANDOM SAMP ---------------------------------------------------------------------
  if (samplemethod == "randomSampling") {
    
    # sampling from class spatial data frame 
    if (class(distr_param[[1]]) != "RasterLayer") { # so all other spatial classes
      
      # function that calls distribution_sampling.R in apply()
      ds <- function(x) {
        parameters <- x
        distribution_sampling(n, distribution, parameters)
      }
      
      # convert data to matrix to be able to use apply()
      in1df <- do.call("cbind", distr_param)
      in1mtx <- as.matrix(in1df@data)
      
      # sample
      temp_samples <- t(apply(in1mtx, MARGIN = 1, ds))
      
      # save sample in X_sample object
      X_sample <- distr_param[[1]] # assign geometry
      X_sample@data <- as.data.frame(temp_samples)
      
      # sort out names
      if (!is.null(UMobject$id)) {
        names(X_sample@data) <- paste(UMobject$id, ".sim", c(1:n), sep = "")
      } else {
        names(X_sample@data) <- paste("sim", c(1:n), sep = "")}
      
      # if asList = T converst sample to a list
      if (asList == TRUE) {
        X_sample <- purrr::map(1:n, function(x){X_sample[x]})
      }
      
    # sampling from raster
    } else {
      # save distribution parameters in a raster stack
      in1stack <- raster::stack(distr_param)
      # sample using distribution_sampling_raster.R
      # first MC realization
      outstack <- distribution_sampling_raster(distribution, parameters_stack = in1stack)
      # the rest of MC realizations
      for (i in 1:(n-1)) {
        outstack_i <- distribution_sampling_raster(distribution, parameters_stack = in1stack)
        outstack <- raster::stack(outstack, outstack_i)
      }
      X_sample <- outstack
      
      # sort out names
      if (!is.null(UMobject$id)) {
        names(X_sample) <- paste(UMobject$id, ".sim", c(1:n), sep = "")
      } else {
        names(X_sample) <- paste("sim", c(1:n), sep = "")}
      if (asList == TRUE) {
        X_sample <- purrr::map(1:n, function(x){X_sample[[x]]})
      }
    }
  }
  
  ### STRATIFIED SAMP ------------------------------------------------------------------
  if (samplemethod == "stratifiedSampling") {
    if (n %% (length(p)-1) != 0)
      stop("n should be divisable by the number of strata")
    
    # recognise if dealing with rester or spatial data frame objects,
    # if raster then converst it to spatial grid
    if (is(distr_param[[1]], "RasterLayer")) {
      original_class <- "RasterLayer"
      for (i in 1:length(distr_param)) {
        distr_param[[i]] <- as(distr_param[[i]], "SpatialGridDataFrame")
      }
    } else {original_class <- "SpatialDF"}
    
    # function to call stratsamp.R in apply()
    stsS <- function(x, ...) {
      parameters <- x
      as.numeric(stratsamp(n = n/(length(p)-1), distribution, parameters, p, ...))
    }
    
    # convert data to matrix to be able to use apply()
    in1df <- do.call("cbind", distr_param)
    in1mtx <- as.matrix(in1df@data)  
    
    # sample
    temp_samples <- t(apply(in1mtx, MARGIN = 1, stsS))
    
    # save sample in X_sample object
    X_sample <- distr_param[[1]] # assign geometry
    X_sample@data <- as.data.frame(temp_samples)
    
    # sort out names
    if (!is.null(UMobject$id)) {
      names(X_sample@data) <- paste(UMobject$id, ".sim", c(1:n), sep = "")
    } else {
      names(X_sample@data) <- paste("sim", c(1:n), sep = "")}
    
    # sort out final product depending on if Raster or spatial data frame
    # and if object to be returned as list
    if (original_class == "RasterLayer") {
      X_sample <- raster::stack(X_sample)
      if (asList == TRUE) {
        X_sample <- purrr::map(1:n, function(x){X_sample[[x]]})
      }
    } else if (asList == TRUE) {
      X_sample <- purrr::map(1:n, function(x){X_sample[x]}) # convert SpGridDF to list
    }
  }
  X_sample
} 

