#' Define Mulivariate Uncertainty Model
#'
#' In case of scalar a square matrix of correlations,
#' dimentionally equal to the number of objects, square, symetric 
#' (transposed must be the same as original, diagonal must all be 1
#' all values must be <-1, +1>) and all eigenvalues must be > 0.
#' 
#' For the spatial object the 
#'
#' @param UMlist a list of uncertain objects creaded in defineUM()
#' @param cormatrix matrix of cross-correlations
#' @param ... 
#'
#' @return Object of a class "JointNumericSpatial" or "JointScalar".
#'
#' @examples
#' 
#' data(Madagascar)
#' OC_crm <- makecrm(acf0 = 0.6, range = 1000, model = "Sph")
#' OC_UM <- defineUM(TRUE, distribution = "norm", distr_param = c(OC, OC_sd), crm = OC_crm, id = "OC")
#' class(OC_UM)
#' TN_crm <- makecrm(acf0 = 0.4, range = 1000, model = "Sph")
#' TN_UM <- defineUM(TRUE, distribution = "norm", distr_param = c(TN, TN_sd), crm = TN_crm, id = "TN")
#' class(TN_UM)
#' 
#' soil_prop <- list(OC_UM,TN_UM)
#' str(soil_prop)
#' mySpatialMUM <- defineMUM(soil_prop, matrix(c(1,0.7,0.7,1), nrow=2, ncol=2))
#' class(mySpatialMUM)
#' str(mySpatialMUM)
#' 
#' # scalar
#' scalarUM <- defineUM(uncertain = TRUE, distribution = "norm", distr_param = c(1, 2), id="Var1")                
#' scalarUM2 <- defineUM(uncertain = TRUE, distribution = "norm", distr_param = c(3, 2), id="Var2")
#' scalarUM3 <- defineUM(uncertain = TRUE, distribution = "norm", distr_param = c(10, 2.5), id="Var3")                
#' myMUM <- defineMUM(UMlist = list(scalarUM, scalarUM2, scalarUM3), 
#'                matrix(c(1,0.7,0.2,0.7,1,0.5,0.2,0.5,1), nrow = 3, ncol = 3))
#' class(myMUM)
#' 
#' @export
defineMUM <- function(UMlist, cormatrix, ...) {
  
  stopifnot(length(UMlist) > 1)
  stopifnot(is(UMlist[[1]], "MarginalNumericSpatial") | is(UMlist[[1]], "MarginalScalar"))
  a <- c()
  for (i in 1:length(UMlist)) {
    a[i] <- class(UMlist[[i]])
  }
  if (!isTRUE(all(a == a[1])))
    stop("Object in UMlist must all be of the same class.")
  
  # stopifnot("distribution type of all elements of UMlist is normal")
  for (i in 1:length(UMlist)) {
    a[i] <- UMlist[[i]]$distribution
  }
  if (!isTRUE(all(a == "norm")))
    stop("Only normal distributions are supported for objects saved in UMlist.") 
  
  # all variables must have ids and all must be different
  ids <- c()
  for (i in 1:length(UMlist)) ids[i] <- UMlist[[i]]$id
  stopifnot(length(ids) == length(unique(ids)))
  
  t <- 1E-7
  stopifnot(class(cormatrix) == "matrix")
  stopifnot(dim(cormatrix)[1] == length(UMlist))
  stopifnot(dim(cormatrix)[1] == dim(cormatrix)[2])
  stopifnot(min(diag(cormatrix)) > (1-t))
  stopifnot(max(diag(cormatrix)) < (1+t))
  stopifnot(min(cormatrix) > (-1-t))
  stopifnot(max(cormatrix) < (1+t))               
  stopifnot(cormatrix == t(cormatrix))
  stopifnot(min(eigen(cormatrix)$values) > 0)
  
  # output is UMlist with added to it the correlation matrix  
  mum <- list(UMlist = UMlist,
              cormatrix = cormatrix,
              ...)

  if (is(UMlist[[1]], "MarginalNumericSpatial")) {
    class(mum) <- "JointNumericSpatial"
  } else {
    class(mum) <- "JointScalar"
  }
  
  mum
}



