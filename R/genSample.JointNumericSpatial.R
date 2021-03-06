#' Generating Monte Carlo sample from a list of uncertain objects that are cross-correlated.
#' 
#' Uncertain objects are described by joint PDF or a list from independent objects. Sampling can be done
#' via three different sampling methods:
#'
#' \strong{"ugs"} Unconditional gaussian simulation of spatially
#' cross-correlated errors.
#'
#' \strong{"randomSampling"} Sampling multivariate distribution using eigenvalue decomposition
#' (based on 'mvtnorm' package).
#'
#' \strong{"lhs"} Not implemented yet. Sampling method for at least two uncertain inputs. The
#' uncertain.object is then a list of two or more. It uses startified sampling
#' method to generate the inputs for the latin hypercude algorithm, hence number of samples (n)
#' must be dividable by the number of quantiles to assure each quantile is evenly represented.
#' 
#' NOTE. Version 1.3-1 includes bug fixing related to derivation of
#' cross-correlation matrix for multivariate uncertainty propagation analysis.
#'
#' @param UMobject object of a class JointNumericSpatial. Output of defineMUM().
#' @param n Integer. Number of Monte Carlo realizations.
#' @param samplemethod "ugs" for spatially cross-correlated errors, "randomSampling" for joint PDF of 
#' non-spatial variables, "lhs" if no correlation of errors is considered.
#' @param p A vector of quantiles. Optional. Only required if sample method is "lhs".
#' @param asList Logical. If TRUE return sample in a form of a list, if FALSE returnsample in a format
#' of distribution parameters.
#' @param debug.level integer; set gstat internal debug level, see below for useful values. 
#' If set to -1 (or any negative value), a progress counter is printed.
#' @param ...  Additional parameters that may be passed, e.g. in
#' the "ugs" method. See examples.
#' 
#' @return A Monte Carlo sample of the variables of interest. If asList = TRUE returns
#' list of all samples as lists. 
#' 
#' @author Kasia Sawicka, Stefan van Dam, Gerard Heuvelink
#' 
#' @examples
#' 
#' set.seed(12345)
#' # "ugs" method example
#' # load data
#' data(OC, OC_sd, TN, TN_sd)
#' 
#' # Test for SpatialGridDataFrames
#' OC <- as(OC, 'SpatialGridDataFrame')
#' TN <- as(TN, 'SpatialGridDataFrame')
#' OC_sd <- as(OC_sd, 'SpatialGridDataFrame')
#' TN_sd <- as(TN_sd, 'SpatialGridDataFrame')
#' 
#' # define marginal UMs
#' OC_crm <- makeCRM(acf0 = 0.6, range = 5000, model = "Sph")
#' OC_UM <- defineUM(TRUE, distribution = "norm", distr_param = c(OC, OC_sd), crm = OC_crm, id = "OC")
#' TN_crm <- makeCRM(acf0 = 0.4, range = 5000, model = "Sph")
#' TN_UM <- defineUM(TRUE, distribution = "norm", distr_param = c(TN, TN_sd), crm = TN_crm, id = "TN")
#' 
#' # define joint UM
#' soil_prop <- list(OC_UM, TN_UM)
#' mySpatialMUM <- defineMUM(soil_prop, matrix(c(1,0.7,0.7,1), nrow=2, ncol=2))
#' 
#' # sample - "ugs" method
#' # toy example
#' my_cross_sample <- genSample(mySpatialMUM, n = 3, "ugs", nmax = 24, asList = TRUE)
#' class(my_cross_sample)
#' \dontrun{
#' my_cross_sample <- genSample(mySpatialMUM, n = 50, "ugs", nmax = 24, asList = TRUE)
#' class(my_cross_sample)
#' }
#' 
#' 
#' 
#' @importFrom gstat gstat
#' @importFrom purrr map
#' @importFrom raster stack
#' 
#' @export
genSample.JointNumericSpatial <- function(UMobject, n, samplemethod, p = 0, asList = TRUE, debug.level = 1,  ...) {
  
  
  # PRELIMINARIES ----------------------------------------------------------------
  # ------------------------------------------------------------------------------
  
  # how many marginals the multivariate object contains
  n_vars <- length(UMobject[[1]])
  
  # extract info about normal distribution parameters for all objects
  means <- c()
  sds <- c()
  for (i in 1:n_vars) {
    means[i]  <- UMobject[[1]][[i]]$distr_param[1]
    sds[i]    <- UMobject[[1]][[i]]$distr_param[2]
  }
  
  # extract correlation matrix from UMobject
  cormatrix <- UMobject[[2]]
  
  # recognise if dealing with raster or spatial data frame objects,
  # if raster then converst it to spatial grid
  if (is(means[[i]], "RasterLayer")) {
    original_class <- "RasterLayer"
    for (i in 1:n_vars) {
      means[[i]] <- as(means[[i]], "SpatialGridDataFrame")
      sds[[i]]   <- as(sds[[i]], "SpatialGridDataFrame")
    }
  } else {original_class <- "SpatialDF"}
  
  
  
  # SAMPLING ---------------------------------------------------------------------
  # ------------------------------------------------------------------------------
  
  # UGS --------------------------------------------------------------------------
  if (samplemethod == "ugs") {

    # split all info from UMobject into meaningful pieces
    # check if models are the same (ranges and shapes)
    ranges <- c()
    shapes <- c()
    for (i in 1:n_vars) {
      ranges[i] <- UMobject[[1]][[i]]$crm[[2]]
      shapes[i] <- UMobject[[1]][[i]]$crm[[3]]
    } 
    stopifnot(isTRUE(all(ranges == ranges[1])))
    stopifnot(isTRUE(all(shapes == shapes[1])))

    # translate correlograms to variograms
    vgms <- list()
    ids <- c()
    nuggets <- c()
    psills  <- c()
    for (i in 1:n_vars) {
      vgms[[i]]    <- crm2vgm(UMobject[[1]][[i]]$crm)
      ids[i] <- UMobject[[1]][[i]]$id
      nuggets[i]   <- vgms[[i]]$psill[1]
      psills[i]    <- vgms[[i]]$psill[2]  
    }
    
    # start building gstat object with the first variable
    g <- gstat::gstat(NULL, id = ids[1], formula = z~1, 
                      dummy = TRUE, beta = 0, model = vgms[[1]], ...)
    # add gstat objects of for remaining variables
    for (i in 2:n_vars) {
      g <- gstat::gstat(g, id = ids[i], formula = z~1,
                        dummy = TRUE, beta = 0, model = vgms[[i]], ...)
    }
    # and add gstat objects for all combinations (cross-correlations) of the variables
    for (i in 1:(n_vars - 1)) {
      for (j in (i + 1):n_vars) {
        g <- gstat::gstat(g, id = c(ids[i], ids[j]), 
                          model = gstat::vgm(psill = cormatrix[i, j] * sqrt(psills[i]*psills[j]),
                                             model = shapes[1],
                                             range = ranges[1], 
                                             nugget = cormatrix[i, j] * (1 - sqrt(psills[i]*psills[j]))), ...)
      }
    }
    
    # predict epsilon
    mask <- means[[1]] # assign geometry
    epsilon_sample <- predict(object = g, newdata = mask, nsim = n, debug.level = debug.level)
    # calculate Z = m + sd*epsilon for each variable
    Cross_sample <- epsilon_sample # assing geometry and dimentions the same as for epsilons
    # first variable:
    Cross_sample@data[1:n] <- 
      as.data.frame(apply(as.matrix(epsilon_sample@data[1:n]),
                                    MARGIN = 2,
                                    function(x) means[[1]]@data + sds[[1]]@data*x))
    # all remaining variables:
    for (i in 1:n_vars-1) {
      Cross_sample@data[(i*n+1):(i*n+n)] <- 
        as.data.frame(apply(as.matrix(epsilon_sample@data[(i*n+1):(i*n+n)]),
                                      MARGIN = 2,
                                      function(x) means[[i+1]]@data + sds[[i+1]]@data*x))
    }
    # splitted into operations on first variable first and the rest later
    # for easier loop iterations.
    
  } # end sampling with "ugs"

  
  # RANDOM SAMP ------------------------------------------------------------------
  if (samplemethod == "randomSampling") {

    # extract ids for all variables
    ids <- c()
    for (i in 1:n_vars) {
      ids[i]    <- UMobject[[1]][[i]]$id
    }

    # put the normal distribution parameters for all variables in one spatial data frame
    in1df <- means[[1]]
    for (i in 2:n_vars) in1df <- cbind(in1df, means[[i]])
    for (i in 1:n_vars) in1df <- cbind(in1df, sds[[i]])
    
    # number of geographical locations
    no_locations <- nrow(in1df@data)

    # create empty dataset to fill in later
    Cross_sample <- means[[1]] # assign geometry
    Cross_sample@data <- as.data.frame(matrix(nrow = no_locations, ncol = n*n_vars, data = NA))
    
    # do sampling cell by cell (location by location)
    means_location <- c()
    sds_location <- c()
    for (i in 1:no_locations) {
      # for each location extract means and sds
      means_location <- as.numeric(in1df@data[i, c(1:n_vars)])
      sds_location   <- as.numeric(in1df@data[i, c((n_vars+1):(n_vars*2))])
      # do sampling only for the cells (locations) where there is no NAs
      if (!any(is.na(means_location)) & !any(is.na(sds_location))) {
        VarCov <- varcov(sds_location, cormatrix)
        Cross_sample_cell <- mvtnorm::rmvnorm(n, means_location, VarCov)
        Cross_sample@data[i, 1:n] <- Cross_sample_cell[, 1]
        for (j in 1:n_vars-1) {
          Cross_sample@data[i, c((j*n+1):(j*n+n))] <- Cross_sample_cell[, j+1]
        }
      # if there is NA return NA
      } else {
        Cross_sample@data[i, 1:n] <- NA
        for (j in 1:n_vars-1) {
          Cross_sample@data[i, c((j*n+1):(j*n+n))] <- NA
        }
      }
    }
    colnames(Cross_sample@data)[1:n] <- paste(ids[1], ".sim", 1:n, sep = "")
    for (i in 1:n_vars-1) {
      names(Cross_sample@data)[(i*n+1):(i*n+n)] <- paste(ids[i+1], ".sim", 1:n, sep = "")
    }
  } # end sampling with "randomSampling"

  
  # LHS --------------------------------------------------------------------------
  if (samplemethod == "lhs") { 
    print("lhs sampling method is not implemented yet")
  } # end sampling with "lhs"   
  
  
  
  # FINAL OPERATIONS -------------------------------------------------------------
  # ------------------------------------------------------------------------------
  
  # if the original class of distributions parameters was RasterLayer
  # convert simulations to raster stack
  if (original_class == "RasterLayer") {
   Cross_sample <- raster::stack(Cross_sample)
   # if asList = T then split into separate rasters saved in a list
   # for each object a list of MC realizations is created and then
   # it's added to a list for all objects
   if (asList == TRUE) {
     l <- list()
     # split raster stack into a list of separate raster for the first object
     l[[1]] <- purrr::map(1:n, function(x){Cross_sample[[x]]})
     # repeat for the rest of the objects
     for (i in 1:n_vars-1) {
       l[[i+1]] <- purrr::map((i*n+1):(i*n+n), function(x){Cross_sample[[x]]})
     }
     Cross_sample <- l
   }
  # if the original class was spatial data frame only check argument
  # asList and if TRUE do as above. The difference is in number of square
  # brackets when purrr::mapping the object
  } else if (asList == TRUE) {
   l <- list()
   l[[1]] <- purrr::map(1:n, function(x){Cross_sample[x]})
   for (i in 1:n_vars-1) {
     l[[i+1]] <- purrr::map((i*n+1):(i*n+n), function(x){Cross_sample[x]})
   }
   Cross_sample <- l
  }
  Cross_sample
}


