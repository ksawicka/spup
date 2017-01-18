#' Title Propagation function
#'
#' @param realizations a list or a list of lists; max one nesting is allowed.
#' @param model a model that is written as a function in R
#' @param n number of Monte Carlo Runs
#' @param ... any arguments that the model takes on top of realizations
#'
#' @return model output realizations
#'
#' @examples
#' 
#' data(DEM)
#' dem_crm <- makecrm(acf0 = 0.78, range = 321, model = "Exp")
#' demUM <- defineUM(uncertain = TRUE, distribution = "norm", 
#'                    distr_param = c(dem30m, dem30m_sd), crm = dem_crm)
#' dem_sample <- genSample(UMobject = demUM, n = 5, samplemethod = "ugs", nmax = 20)
#' slope_sample <- propagate(dem_sample, model = Slope, n = 5, projection = CRS("+init=epsg:3857"))
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


# set.seed(12345)
# data(DEM)
# dem_crm <- makecrm(acf0 = 0.78, range = 321, model = "Exp")
# demUM <- defineUM(uncertain = TRUE, distribution = "norm",
#                  distr_param = c(dem30m, dem30m_sd), crm = dem_crm)
# dem_sample <- genSample(UMobject = demUM, n = 5, samplemethod = "ugs", nmax = 20)
# 
# a <- propagate(realizations = dem_sample, model = Slope, n = 5, projection = CRS("+init=epsg:3857"))
# str(a)
# spplot(a[[1]])
# 
# dem_sample2 <- dem_sample
# k = c(2,2,2,2,2)
# 
# mymodel <- function(dem1, dem2, k) {
#   dupa <- dem1
#   dupa@data <- (dem1@data/dem2@data)*k
#   dupa
# }
# a <- mymodel(dem_sample[[1]], dem_sample2[[1]], k=2)
# spplot(a)
# 
# b <- propagate(list(dem_sample, dem_sample2), model = mymodel, n=5, k=2)
# ll <- list(dem_sample, dem_sample2)
# b <- propagate(ll, model = mymodel, n=5, k=5)

# data(dummyraster)
# rastUM <- defineUM(uncertain = TRUE, distribution = "norm", distr_param = c(rast_mean, rast_sd))
# rast_sample <- genSample(UMobject = rastUM, n = 5, samplemethod = "randomSampling")
# rast_model1 <- function(r1) {
#   r <- r1*2
#   r
# }
# b <- propagate2(realizations = rast_sample, model = rast_model1, n =5)
# str(b)
# plot(b[[1]])
#
#
# rast_model2 <- function(r1, r2) {
#   r <- r1/r2 * 100
#   r
# }
# rast_sample2 <- rast_sample
# b <- propagate2(list(rast_sample,rast_sample2), rast_model2, 5)
# str(b)
# plot(b[[1]])



