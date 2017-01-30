#' Title Propagation function
#'
#' @param realizations a list or a list of lists; max one nesting is allowed.
#' @param model a model that is written as a function in R.
#' @param n number of Monte Carlo Runs.
#' @param ... any arguments that the model takes on top of realizations.
#'
#' @return model output realizations
#'
#' @examples
#' 
#' # continuous spatial data example with a single variable
#' data(DEM)
#' dem_crm <- makecrm(acf0 = 0.78, range = 321, model = "Exp")
#' demUM <- defineUM(uncertain = TRUE, distribution = "norm", 
#'                    distr_param = c(dem30m, dem30m_sd), crm = dem_crm)
#' dem_sample <- genSample(UMobject = demUM, n = 5, samplemethod = "ugs", nmax = 20)
#' slope_sample <- propagate(dem_sample, model = Slope, n = 5, projection = CRS("+init=epsg:3857"))
#' 
#' # categorical spatial data example
#' data(Rotterdam)
#' woonUM <- defineUM(TRUE, categories = c(1,2,3), cat_prob = woon[, c(4:6)])
#' woon_sample <- genSample(woonUM, 10)
#' class(woon_sample)
#' tax # the model takes SpatialGridDataFrame with a column called "Function"
#' for (i in 1:10) names(woon_sample[[i]]) <- "Function"
#' tax_uncert <- propagate(realizations = woon_sample, n = 10, model = tax)
#' tax_uncert <- unlist(tax_uncert)
#' summary(tax_uncert)
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
  } else if (d == 2) {
    # print("method for many arguments")
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



