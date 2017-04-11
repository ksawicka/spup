#' Propagation function
#' 
#' A function that runs a model repeatedly with Monte Carlo samples
#' of uncertain inputs.
#'
#' @param realizations a list where each element is a single Monte Carlo realizations 
#' if only one parameter/variable is considered uncertain; a list of such lists if more
#' than one parameter/variable is considered uncertain.
#' @param model model that is written as a function in R.
#' @param n number of Monte Carlo Runs.
#' @param ... any further arguments that the model takes.
#'
#' @return Model output Monte Carlo realizations.
#'
#' @author Kasia Sawicka
#' 
#' @examples
#' 
#' set.seed(12345)
#' ## continuous spatial data example with a single variable
#' # load data
#' data(dem30m, dem30m_sd)
#' 
#' # Slope model 
#' Slope <- function(DEM, ...) {
#'   require(raster)
#'   require(purrr)
#'   demraster <- 
#'     DEM %>%
#'     raster()
#'   demraster %>%
#'     terrain(opt = 'slope', ...) %>%
#'     as("SpatialGridDataFrame")
#' }
#'  
#' # uncertainty propagation
#' dem_crm <- makecrm(acf0 = 0.78, range = 321, model = "Exp")
#' demUM <- defineUM(uncertain = TRUE, distribution = "norm", 
#'                    distr_param = c(dem30m, dem30m_sd), crm = dem_crm)
#' dem_sample <- genSample(UMobject = demUM, n = 5, samplemethod = "ugs", nmax = 20)
#' slope_sample <- propagate(dem_sample, model = Slope, n = 5)
#' 
#' ## categorical spatial data example
#' # load data
#' data(woon)
#' 
#' # tax model
#' tax <- function(building_Function) { 
#'   building_Function$tax2pay <- NA
#'   building_Function$tax2pay[building_Function$Function == 1] <- 1000
#'   building_Function$tax2pay[building_Function$Function == 2] <- 10000
#'   building_Function$tax2pay[building_Function$Function == 3] <- 10
#'   total_tax <- sum(building_Function$tax2pay)
#'   total_tax
#' }
#' 
#' # uncertainty propagation
#' woonUM <- defineUM(TRUE, categories = c(1,2,3), cat_prob = woon[, c(4:6)])
#' woon_sample <- genSample(woonUM, 10)
#' class(woon_sample)
#' tax # the model takes SpatialGridDataFrame with a column called "Function"
#' for (i in 1:10) names(woon_sample[[i]]) <- "Function"
#' tax_uncert <- propagate(realizations = woon_sample, n = 10, model = tax)
#' tax_uncert <- unlist(tax_uncert)
#' summary(tax_uncert)
#' 
#' ## cross-correlated example
#' # load data
#' data(OC, OC_sd, TN, TN_sd)
#' 
#' # C/N model
#' C_N_model_raster <- function(OC, TN) {
#'   OC/TN
#' }
#' 
#' # define marginal UMs
#' OC_crm <- makecrm(acf0 = 0.6, range = 1000, model = "Sph")
#' OC_UM <- defineUM(TRUE, distribution = "norm", distr_param = c(OC, OC_sd), crm = OC_crm, id = "OC")
#' TN_crm <- makecrm(acf0 = 0.4, range = 1000, model = "Sph")
#' TN_UM <- defineUM(TRUE, distribution = "norm", distr_param = c(TN, TN_sd), crm = TN_crm, id = "TN")
#' 
#' # define joint UM
#' mySpatialMUM <- defineMUM(list(OC_UM, TN_UM), matrix(c(1,0.7,0.7,1), nrow=2, ncol=2))
#' 
#' # sample - "ugs" method
#' my_cross_sample <- genSample(mySpatialMUM, 5, "ugs", nmax = 24)
#' class(my_cross_sample)
#' 
#' # run propagation
#' CN_sample <- propagate(realizations = my_cross_sample, model = C_N_model_raster, n = 5)
#' CN_sample
#' 
#' 
#' @export
propagate <- function(realizations, model, n, ...) {
  
  d <- list_depth(realizations)
  stopifnot(d < 3)
  model_output <- list()
  
  # one uncertain model input, MC sample saved in list
  if (d == 1) {
    stopifnot(n <= length(realizations)) # can't run model for more n than MC samples
    for (i in 1:n) {
      model_input <- realizations[[i]]
      model_output[[i]] <- do.call(what = model, args = list(model_input, ...))
    }
  # more than one uncertain input to the model
  } else if (d == 2) {
    for (i in 1:n) {
      model_input <- list()
      for (j in 1:length(realizations)) {
        model_input[[j]] <- realizations[[j]][[i]]   
      }
      model_output[[i]] <- do.call(what = model, args = c(model_input, ...))
    }
  }
  model_output
}



