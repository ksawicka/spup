#' Methods for generating Monte Carlo realizations from uncertain inputs.
#' 
#' #' Methods for classes: "MarginalNumericSpatial", "MarginalScalar",
#' "MarginalCategoricalSpatial", "JoinNumericSpatial", "JointScalar".
#' Function that runs Monte Carlo simulations depending on the type of
#' uncertain object. Facilitates unconditional gausian simulation of errors for
#' spatially auto-correlated residulas, and random sampling, stratified
#' sampling if no spatial auto-correlation is included.
#' 
#' 
#' Sampling methods:
#'
#' \strong{"ugs"} Unconditional gaussian simulation of spatially auto-correlated and/or
#' cross-correlated errors.
#'
#' \strong{"randomSampling"} Sampling multivariate distribution using eigenvalue decomposition
#' (based on 'mvtnorm' package).
#' 
#' \strong{"stratifiedSampling"} Number of samples (n) must be dividable by the
#' number of quantiles to assure each quantile is evenly represented.
#'
#' \strong{"lhs"} Not implemented yet. Sampling method for at least two uncertain inputs. The
#' uncertain.object is then a list of two or more. It uses startified sampling
#' method to generate the inputs for the latin hypercude algorithm, hence number of samples (n)
#' must be dividable by the number of quantiles to assure each quantile is evenly represented.
#' 
#'
#' @param UMobject an uncertain object to sample from, output of defineUM() or defineMUM().
#' @param n integer, number of Monte Carlo realizations.
#' @param samplemethod a string, "ugs", "randomSampling", "stratifiedSampling", 
#' "lhs" ("lhs" currently not in use).
#' @param p A vector of quantiles. Optional. Only required if sample method is "stratifiedSampling" or "lhs".
#' @param asList logical. If asList = TRUE returns list of all samples as a list. 
#' If asList = FALSE returns samples in a format of distribution parameters in UMobject.
#' @param ... Additional parameters that may be passed, e.g. in the "ugs" method. See examples.
#' 
#'
#' @return A Monte Carlo sample of the variables of interest. If asList = TRUE returns
#' list of all samples as lists.
#' 
#' @author Kasia Sawicka, Stefan van Dam, Gerard Heuvelink
#'
#' @examples
#' 
#' set.seed(12345)
#' 
#' ### ------------------- "MarginalNumericSpatial" -------------------
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
#' 
#' ### ----------------------- "MarginalScalar" -----------------------
#' # Example 1
#' scalarUM <- defineUM(uncertain = TRUE, distribution = "norm", distr_param = c(10, 1))
#' scalar_sample <- genSample(scalarUM, n = 10, samplemethod = "randomSampling")
#' 
#' # Example 2
#' scalarUM <- defineUM(uncertain = TRUE, distribution = "beta", distr_param = c(10, 1, 2))
#' scalar_sample <- genSample(scalarUM, n = 10, samplemethod = "stratifiedSampling", p = 0:5/5)
#' 
#' 
#' ### ----------------- "MarginalCategoricalSpatial" -----------------
#' # load data
#' data(woon)
#' woonUM <- defineUM(TRUE, categories = c(1,2,3), cat_prob = woon[, c(4:6)])
#' # woon_sample <- genSample(woonUM, 10, asList = FALSE)
#' # class(woon_sample)
#' # str(woon_sample@data)
#' # spplot(woon_sample)
#' woon_sample <- genSample(woonUM, 10)
#' class(woon_sample)
#' 
#' # analyse probability of having snow
#' # load data
#' data(dem30m, dem30m_sd)
#' 
#' # generate dummy probabilities for categories "snow" and "no snow"
#' dem30m$snow_prob <- NA
#' dem30m$snow_prob[dem30m$Elevation > 1000] <- 0.75
#' dem30m$snow_prob[dem30m$Elevation <= 1000] <- 0.25
#' dem30m$no_snow_prob <- 1 - dem30m$snow_prob
#' summary(dem30m@data)
#' snowUM <- defineUM(uncertain = TRUE, categories = c("snow", "no snow"), cat_prob = dem30m[2:3])
#' class(snowUM)
#' snow_sample <- genSample(snowUM, 10, asList = FALSE)
#' head(snow_sample@data)
#' 
#' # case with raster
#' # load data
#' data(dem30m, dem30m_sd)
#' dem30m$snow_prob <- NA
#' dem30m$snow_prob[dem30m$Elevation > 1000] <- 0.75
#' dem30m$snow_prob[dem30m$Elevation <= 1000] <- 0.25
#' dem30m$no_snow_prob <- 1 - dem30m$snow_prob
#' summary(dem30m@data)
#' dem_stack <- raster::stack(dem30m)
#' snowUM <- defineUM(uncertain = TRUE, categories = c("snow", "no snow"), cat_prob = dem_stack[[2:3]])
#' snow_sample <- genSample(snowUM, 10, asList = FALSE)
#' require(sp)
#' spplot(snow_sample)
#' 
#' 
#' ### -------------------- "JoinNumericSpatial" ----------------------
#' # "ugs" method example
#' # load data
#' data(OC, OC_sd, TN, TN_sd)
#' 
#'  # # Temporarily - convert to spatial grid data frames as raster not supported yet
#'  # OC <- as(OC, 'SpatialGridDataFrame')
#'  # TN <- as(TN, 'SpatialGridDataFrame')
#'  # OC_sd <- as(OC_sd, 'SpatialGridDataFrame')
#'  # TN_sd <- as(TN_sd, 'SpatialGridDataFrame')
#' 
#' # define marginal UMs
#' OC_crm <- makecrm(acf0 = 0.6, range = 1000, model = "Sph")
#' OC_UM <- defineUM(TRUE, distribution = "norm", distr_param = c(OC, OC_sd), crm = OC_crm, id = "OC")
#' TN_crm <- makecrm(acf0 = 0.4, range = 1000, model = "Sph")
#' TN_UM <- defineUM(TRUE, distribution = "norm", distr_param = c(TN, TN_sd), crm = TN_crm, id = "TN")
#' 
#' # # some dummy variable to test code on more than two variables
#' # dummy <- OC
#' # dummy@data <- OC@data*TN@data/2
#' # names(dummy) <- "dummy"
#' # dummy_sd <- dummy
#' # dummy_sd@data <- dummy@data * 0.3
#' # names(dummy_sd) <- "dummy_sd"
#' # dummy_crm <- makecrm(acf0 = 0.9, range = 1000, model = "Sph")
#' # dummy_UM <- defineUM(TRUE, distribution = "norm", distr_param = c(dummy, dummy_sd),
#' #                     crm = dummy_crm, id = "dummy")
#' 
#' # define joint UM
#' soil_prop <- list(OC_UM, TN_UM)
#' mySpatialMUM <- defineMUM(soil_prop, matrix(c(1,0.7,0.7,1), nrow=2, ncol=2))
#' # soil_prop <- list(OC_UM, TN_UM, dummy_UM)
#' #  mySpatialMUM <- defineMUM(soil_prop, matrix(c(1,0.7,0.2,0.7,1,0.5,0.2,0.5,1), nrow=3, ncol=3))
#' class(mySpatialMUM)
#' 
#' # sample - "ugs" method
#' \dontrun{
#' my_cross_sample <- genSample(mySpatialMUM, n = 5, "ugs", nmax = 24, asList = TRUE)
#' class(my_cross_sample)
#' }
#' 
#' # sample - "randomSampling"
#' \dontrun{
#' my_cross_sample <- genSample(mySpatialMUM, n = 5, "randomSampling")
#' }
#' 
#' 
#' ### ------------------------- "JointScalar" ------------------------
#' scalarUM <- defineUM(uncertain = TRUE, distribution = "norm",
#'                      distr_param = c(1, 2), id="Var1")                
#' scalarUM2 <- defineUM(uncertain = TRUE, distribution = "norm",
#'                       distr_param = c(3, 2), id="Var2")
#' scalarUM3 <- defineUM(uncertain = TRUE, distribution = "norm",
#'                       distr_param = c(10, 2.5), id="Var3")                
#' myMUM <- defineMUM(UMlist = list(scalarUM, scalarUM2, scalarUM3), 
#'                matrix(c(1,0.7,0.2,0.7,1,0.5,0.2,0.5,1), nrow = 3, ncol = 3))
#' my_sample <- genSample(myMUM, n = 10, samplemethod = "randomSampling", asList = FALSE)
#' my_sample  
#' 
#' 
#' 
#' @export
genSample <- function(UMobject, n, samplemethod, p = 0, asList = TRUE, ...) {
  UseMethod("genSample")
} 
