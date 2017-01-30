#' Generating Monte Carlo sample from a list of uncertain objects that are cross-correlated
#' and described by joint PDF or a list from independent objects using random sampling or LHS method.
#'
#' \strong{"ugs"} Unconditional gaussian simulation of spatially
#' cross-correlated errors.
#'
#' \strong{"randomSampling"} Sampling multivariate distribution using eigenvalue decomposition
#' (based on 'mvtnorm' package).
#'
#' \strong{"lhs"} Sampling method for at least two uncertain inputs. The
#' uncertain.object is then a list of two or more. It uses startified sampling
#' method to generate the inputs for the latin hypercude algorithm, hence number of samples (n)
#' must be dividable by the number of quantiles to assure each quantile is evenly represented.
#'
#' @usage genSample(UMobject, varcov, n, samplemethod, p = 0, ...)
#'
#' @param UMobject object of a class JointNumericSpatial. Output of defineMUM().
#' @param n Integer. Number of Monte Carlo realizations.
#' @param samplemethod "ugs" for spatially cross-correlated errors, "randomSampling" for joint PDF of 
#' non-spatial variables, "lhs" if no correlation of errors is considered.
#' @param p A vector of quantiles. Optional. Only required if sample method is "lhs".
#' @param ...  Additional parameters that may be passed, e.g. in
#' the "ugs" method. See examples.
#' @param asList Logical. If TRUE return sample in a form of a list, if FALSE returnsample in a format
#' of distribution parameters.
#' 
#' @return A Monte Carlo sample of the variables of interest. If asList = TRUE returns
#' list of all samples as lists. 
#' 
#' @author Kasia Sawicka, Stefan van Dam, Gerard Heuvelink
#' 
#' @examples
#' 
#' # "ugs" method example
#' # load data
#' data(Madagascar)
#' 
#'  # Temporarily - convert to spatial grid data frames as raster not supported yet
#'  OC <- as(OC, 'SpatialGridDataFrame')
#'  TN <- as(TN, 'SpatialGridDataFrame')
#'  OC_sd <- as(OC_sd, 'SpatialGridDataFrame')
#'  TN_sd <- as(TN_sd, 'SpatialGridDataFrame')
#' 
#' # define marginal UMs
#' OC_crm <- makecrm(acf0 = 0.6, range = 1000, model = "Sph")
#' OC_UM <- defineUM(TRUE, distribution = "norm", distr_param = c(OC, OC_sd),
#'                   crm = OC_crm, id = "OC", cross_ids = "TN")
#' TN_crm <- makecrm(acf0 = 0.4, range = 1000, model = "Sph")
#' TN_UM <- defineUM(TRUE, distribution = "norm", distr_param = c(TN, TN_sd),
#'                   crm = TN_crm, id = "TN", cross_ids = "OC")
#' 
#'  # some dummy variable to test code on more than two
#'  dummy <- OC
#'  dummy@data <- OC@data*TN@data/2
#'  names(dummy) <- "dummy"
#'  dummy_sd <- dummy
#'  dummy_sd@data <- dummy@data * 0.3
#'  names(dummy_sd) <- "dummy_sd"
#'  dummy_crm <- makecrm(acf0 = 0.9, range = 1000, model = "Sph")
#'  dummy_UM <- defineUM(TRUE, distribution = "norm", distr_param = c(dummy, dummy_sd),
#'                      crm = dummy_crm, id = "dummy", cross_ids = c("OC", "TN"))
#' 
#' # define joint UM
#' #soil_prop <- list(OC_UM, TN_UM)
#' #mySpatialMUM <- defineMUM(soil_prop, matrix(c(1,0.7,0.7,1), nrow=2, ncol=2))
#'  soil_prop <- list(OC_UM, TN_UM, dummy_UM)
#'   mySpatialMUM <- defineMUM(soil_prop, matrix(c(1,0.7,0.2,0.7,1,0.5,0.2,0.5,1), nrow=3, ncol=3))
#' class(mySpatialMUM)
#' 
#' # sample
#' my_cross_sample <- genSample(mySpatialMUM, 5, "ugs", nmax = 24, asList = F)
#' class(my_cross_sample)
#' 
#'  
#'"lhs" method example
#' 
#' @export
genSample.JointNumericSpatial <- function(UMobject, n, samplemethod, p = 0, asList = TRUE, ...) {
  
  n_vars <- length(UMobject[[1]])
  
  # "method ugs" - use gstat
  if (samplemethod == "ugs") {

    # Split all info from UMobject into meaningful pieces
    # check if models are the same (ranges and shapes)
    #
    # acf0s  <- c()
    ranges <- c()
    shapes <- c()
    means <- list()
    sds <- list()
    for (i in 1:n_vars) {
      # acf0s[i]  <- UMobject[[1]][[i]]$crm[[1]]
      ranges[i] <- UMobject[[1]][[i]]$crm[[2]]
      shapes[i] <- UMobject[[1]][[i]]$crm[[3]]
      means[i]  <- UMobject[[1]][[i]]$distr_param[1]
      sds[i]    <- UMobject[[1]][[i]]$distr_param[2]
    } 
    stopifnot(isTRUE(all(ranges == ranges[1])))
    stopifnot(isTRUE(all(shapes == shapes[1])))
    
    if (is(means[[i]], "RasterLayer")) {
      original_class <- "RasterLayer"
      for (i in 1:n_vars) {
        means[[i]] <- as(means[[i]], "SpatialGridDataFrame")
        sds[[i]]   <- as(sds[[i]], "SpatialGridDataFrame")
      }
    } else {original_class <- "SpatialDF"}
    
    cormatrix <- UMobject[[2]]
    
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
    
    # start building gstat object
    g <- gstat::gstat(NULL, id = ids[1], formula = z~1, 
                      dummy = TRUE, beta = 0, model = vgms[[1]], nmax=24)#...)
    for (i in 2:n_vars) {
      g <- gstat::gstat(g, id = ids[i], formula = z~1,
                        dummy = TRUE, beta = 0, model = vgms[[i]], nmax=24)#...)
    }
    for (i in 1:(n_vars - 1)) {
      for (j in (i + 1):n_vars) {
        g <- gstat::gstat(g, id = c(ids[i], ids[j]), 
                          model = gstat::vgm(cormatrix[i, j] * sqrt(psills[i]*psills[j]),
                                      model = shapes[1],
                                      range = ranges[1], 
                                      cormatrix[i, j] * sqrt(nuggets[i]*nuggets[j])), nmax=24)#...)
      }
    }
    
    # predict epsilon
    mask <- means[[1]]
    epsilon_sample <- predict(object = g, newdata = mask, nsim = n)
    
    # calculate Z = m + sd*epsilon for each variable
    Cross_sample <- epsilon_sample
    
    # means - this is a list
    # sds - this is a list
    # epsilon_sample
    Cross_sample@data[1:n] <- 
      as.data.frame(apply(as.matrix(epsilon_sample@data[1:n]),
                                    MARGIN = 2,
                                    function(x) means[[1]]@data + sds[[1]]@data*x))
    for (i in 1:n_vars-1) {
      Cross_sample@data[(i*n+1):(i*n+n)] <- 
        as.data.frame(apply(as.matrix(epsilon_sample@data[(i*n+1):(i*n+n)]),
                                      MARGIN = 2,
                                      function(x) means[[i+1]]@data + sds[[i+1]]@data*x))
    }
    
    # sort out output depending what spatial data and if list
    if (original_class == "RasterLayer") {
      Cross_sample <- raster::stack(Cross_sample)
      if (asList == TRUE) {
        l <- list()
        l[[1]] <- map(1:n, function(x){Cross_sample[[x]]})
        for (i in 1:n_vars-1) {
          l[[i+1]] <- map((i*n+1):(i*n+n), function(x){Cross_sample[[x]]})
        }
      Cross_sample <- l
      }
    } else if (asList == TRUE) {
      l <- list()
      l[[1]] <- map(1:n, function(x){Cross_sample[x]})
      for (i in 1:n_vars-1) {
        l[[i+1]] <- map((i*n+1):(i*n+n), function(x){Cross_sample[x]})
      }
      Cross_sample <- l
    }
  }
  
  # method "random Sampling" or "lhs"
  # - do cell by cell as in .JointScalar and back save in spatial
  
  if (samplemethod == "randomSampling") {

    means <- c()
    sds <- c()
    for (i in 1:n_vars) {
      means[i]  <- UMobject[[1]][[i]]$distr_param[1]
      sds[i]    <- UMobject[[1]][[i]]$distr_param[2]
    }
    # put it all in one spatial data frame
    in1df <- means[[1]]
    for (i in 2:n_vars) in1df <- cbind(in1df, means[[i]])
    for (i in 1:n_vars) in1df <- cbind(in1df, sds[[i]])
    no_locations <- nrow(in1df@data)
    cormatrix <- UMobject[[2]]
    
    Cross_sample <- means[[1]] # assign geometry
    Cross_sample@data <- as.data.frame(matrix(nrow = no_locations, ncol = n*n_vars, data = NA))
    means_location <- c()
    sds_location <- c()
    for (i in 1:no_locations) {
      means_location <- as.numeric(in1df@data[i, c(1:n_vars)])
      sds_location   <- as.numeric(in1df@data[i, c((n_vars+1):(n_vars*2))])
      VarCov <- varcov(sds_location, cormatrix)
      Cross_sample_cell <- mvtnorm::rmvnorm(n, means_location, VarCov)
      # use the id to name the variables here
      Cross_sample@data[i, 1:n] <- Cross_sample_cell[, 1]
      for (j in 1:n_vars-1) {
        Cross_sample@data[i, c((j*n+1):(j*n+n))] <- Cross_sample_cell[, j+1]
      }
      # here fix names of vars in spatial data frame
    }
  }

  if (samplemethod == "lhs") {
    print("lhs")
  }    
    
  Cross_sample
  }


