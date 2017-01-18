#' Generating Monte Carlo sample from an uncertain object of a class 
#' 'MarginalCategoricalSpatial'
#'
#' @usage genSample(UMobject, n, ...)
#'
#' @param UMobject uncertain object defined using defineUM().
#' @param n Integer. Number of Monte Carlo realizations.
#' @param ... additional parameters
#' 
#' @return A Monte Carlo sample of a categorical spatial variable.
#' 
#' @author Kasia Sawicka
#' 
#' @examples
#'
#' # load data
#' data(house)
#' houseUM <- defineUM(uncertain = TRUE, categories = c(100,200), cat_prob = houses_DF)
#' h_sample <- genSample(houseUM, n = 10)
#' str(h_sample)
#'
#' houseUM <- defineUM(uncertain = TRUE, categories = c("yes", 1), cat_prob = houses_DF)
#'
#' @export
genSample.MarginalCategoricalSpatial <- function(UMobject, n, asList = TRUE, ...) {

  categories <- UMobject[[2]]
  cat_prob <- UMobject[[3]]@data
  in1mtx <- as.matrix(cat_prob)
  temp_samples <- t(apply(in1mtx, MARGIN = 1, 
                          function(x) sample(categories, 
                                             size = n, 
                                             replace = TRUE, 
                                             prob = x)))
  X_sample <- UMobject[[3]]
  X_sample@data <- as.data.frame(temp_samples)
  names(X_sample@data) <- paste("sim", c(1:n), sep = "")
  X_sample
  
}
