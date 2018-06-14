#' Methods for generating Monte Carlo realizations from uncertain inputs.
#' 
#' Methods for classes: "MarginalNumericSpatial", "MarginalScalar", 
#' "MarginalCategoricalSpatial", "JointNumericSpatial", "JointScalar".
#' Function that runs Monte Carlo simulations depending on the type of uncertain object.
#' Facilitates unconditional Gaussian simulation of errors for spatially
#' auto-correlated residuals, as well as random and stratified random sampling
#' if no spatial auto-correlation is included.
#' 
#' Sampling methods:
#'
#' \strong{"ugs"} Unconditional Gaussian simulation of spatially auto-correlated and/or
#' cross-correlated errors.
#'
#' \strong{"randomSampling"} Sampling multivariate distribution using eigenvalue decomposition
#' (based on 'mvtnorm' package).
#' 
#' \strong{"stratifiedSampling"} Number of samples (n) must be dividable by the
#' number of quantiles to assure that each quantile is evenly represented.
#'
#' \strong{"lhs"} Not implemented yet. Sampling method for at least two uncertain inputs. The
#' uncertain.object is then a list of two or more. It uses a stratified sampling
#' method to generate inputs for the latin hypercube algorithm.
#' 
#'
#' @param UMobject an uncertain object to sample from, output of defineUM() or defineMUM().
#' @param n integer, number of Monte Carlo realizations.
#' @param samplemethod a string, "ugs", "randomSampling", "stratifiedSampling", 
#' "lhs" ("lhs" currently not in use).
#' @param p A vector of quantiles. Optional. Only required if sample method is "stratifiedSampling" or "lhs".
#' @param asList logical. If asList = TRUE returns list of all samples as a list. 
#' If asList = FALSE returns samples in a format of distribution parameters in UMobject.
#' @param debug.level integer; set gstat internal debug level, see below for useful values. 
#' If set to -1 (or any negative value), a progress counter is printed.
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
#' dem_crm <- makeCRM(acf0 = 0.78, range = 321, model = "Exp")
#' demUM <- defineUM(uncertain = TRUE, distribution = "norm", 
#'                    distr_param = c(dem30m, dem30m_sd), crm = dem_crm)
#'                    
#' # toy example
#' dem_sample <- genSample(UMobject = demUM, n = 2, samplemethod = "ugs", nmax = 4, asList = FALSE)
#' str(dem_sample)
#' # any meaningful Monte Carlo analysis should have normally much larger number of runs               
#' \dontrun{
#' dem_sample <- genSample(UMobject = demUM, n = 100, samplemethod = "ugs", nmax = 20, asList = FALSE)
#' str(dem_sample)
#' }
#' 
#' # "startifiedSampling" method example
#' demUM <- defineUM(uncertain = TRUE, distribution = "norm", distr_param = c(dem30m, dem30m_sd))
#' # toy example
#' dem_sample <- genSample(UMobject = demUM, n = 5, samplemethod = "stratifiedSampling", p = 0:5/5)
#' # any meaningful Monte Carlo analysis should have normally much larger number of runs
#' \dontrun{
#' dem_sample <- genSample(UMobject = demUM, n = 100, samplemethod = "stratifiedSampling", p = 0:5/5)
#' str(dem_sample)
#' }
#' 
#' # Examples with rasters
#' # (raster with auto-correlation)
#' data(OC, OC_sd)
#' OC_crm <- makeCRM(acf0 = 0.6, range = 1000, model = "Sph")
#' OC_UM <- defineUM(TRUE, distribution = "norm", distr_param = c(OC, OC_sd), crm = OC_crm, id = "OC")
#' class(OC_UM)
#' # toy example
#' some_sample <- genSample(OC_UM, n = 2, "ugs", nmax = 4)
#' some_sample
#' # any meaningful Monte Carlo analysis should have normally much larger number of runs
#' \dontrun{
#' some_sample <- genSample(OC_UM, n = 50, "ugs", nmax = 24)
#' some_sample
#' }
#' 
#' 
#' ### ----------------------- "MarginalScalar" -----------------------
#' # example with normal distribution
#' scalarUM <- defineUM(uncertain = TRUE, distribution = "norm", distr_param = c(10, 1))
#' scalar_sample <- genSample(scalarUM, n = 10, samplemethod = "randomSampling")
#' 
#' 
#' ### ----------------- "MarginalCategoricalSpatial" -----------------
#' # load data
#' data(woon)
#' woonUM <- defineUM(TRUE, categories = c(1,2,3), cat_prob = woon[, c(4:6)])
#' woon_sample <- genSample(woonUM, 10, asList = FALSE)
#' class(woon_sample)
#' str(woon_sample@data)
#' 
#' 
#' ### -------------------- "JointNumericSpatial" ----------------------
#' # load data
#' data(OC, OC_sd, TN, TN_sd)
#' 
#' # define marginal UMs
#' OC_crm <- makeCRM(acf0 = 0.6, range = 1000, model = "Sph")
#' OC_UM <- defineUM(TRUE, distribution = "norm", distr_param = c(OC, OC_sd), crm = OC_crm, id = "OC")
#' TN_crm <- makeCRM(acf0 = 0.4, range = 1000, model = "Sph")
#' TN_UM <- defineUM(TRUE, distribution = "norm", distr_param = c(TN, TN_sd), crm = TN_crm, id = "TN")
#' 
#' # define joint UM
#' soil_prop <- list(OC_UM, TN_UM)
#' mySpatialMUM <- defineMUM(soil_prop, matrix(c(1,0.7,0.7,1), nrow=2, ncol=2))
#' class(mySpatialMUM)
#' 
#' # sample - "ugs" method
#' # toy example
#' my_cross_sample <- genSample(mySpatialMUM, n = 2, "ugs", nmax = 4, asList = TRUE)
#' class(my_cross_sample)
#' # any meaningful Monte Carlo analysis should have normally much larger number of runs
#' \dontrun{
#' my_cross_sample <- genSample(mySpatialMUM, n = 100, "ugs", nmax = 24, asList = TRUE)
#' class(my_cross_sample)
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
#' my_sample <- genSample(myMUM, n = 5, samplemethod = "randomSampling", asList = FALSE)
#' my_sample  
#' 
#' 
#' 
#' @export
genSample <- function(UMobject, n, samplemethod, p = 0, asList = TRUE, debug.level = 1, ...) {
  UseMethod("genSample")
} 
