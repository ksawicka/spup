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
#' data(Rotterdam)
#' woonUM <- defineUM(TRUE, categories = c(1,2,3), cat_prob = woon[, c(4:6)])
#' # woon_sample <- genSample(woonUM, 10, asList=FALSE)
#' # class(woon_sample)
#' # str(woon_sample@data)
#' # spplot(woon_sample)
#' woon_sample <- genSample(woonUM, 10)
#' class(woon_sample)
#'
#' @export
genSample.MarginalCategoricalSpatial <- function(UMobject, n, asList = TRUE, ...) {

  # extract information from UMobject
  categories <- UMobject[[2]]
  cat_prob <- UMobject[[3]]@data
  in1mtx <- as.matrix(cat_prob)
  
  # sample
  temp_samples <- t(apply(in1mtx, MARGIN = 1, 
                          function(x) sample(categories, 
                                             size = n, 
                                             replace = TRUE, 
                                             prob = x)))
  
  X_sample <- UMobject[[3]] # assign geometry
  X_sample@data <- as.data.frame(temp_samples)
  
  # sort out the names
  if (!is.null(UMobject$id)) {
    names(X_sample@data) <- paste(UMobject$id, ".sim", c(1:n), sep = "")
  } else {
    names(X_sample@data) <- paste("sim", c(1:n), sep = "")}
  
  # if asList = T convert spatial data frame to a list
  if (asList) {
    X_sample <- map(1:n, function(x){X_sample[x]}) # convert SpDF to list
  }
  
  X_sample
}



