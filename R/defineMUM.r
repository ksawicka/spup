#' Define Mulivariate Uncertainty Model
#'
#' In case of scalar a square matrix of correlations,
#' dimentionally equal to the number of objects, square, symetric 
#' (transposed must be the same as original, diagonal must all be 1
#' all values must be <-1, +1>) and all eigenvalues must be > 0.
#'
#' @param UMlist a list of uncertain objects creaded in defineUM()
#' @param cormatrix matrix of cross-correlations
#' @param ... 
#'
#' @return Object of a class "JointNumericSpatial" or 
#'
#' @examples
#' 
#' data(Madagascar)
#' OC_crm <- makecrm(acf0 = 0.6, range = 1000, model = "Sph")
#' OC_UM <- defineUM(TRUE, distribution = "norm", 
#'                   distr_param = c(OC, OC_sd), crm = OC_crm,
#'                   id = "OC", cross_ids = "TN")
#' class(OC_UM)
#' 
#' TN_crm <- makecrm(acf0 = 0.4, range = 1000, model = "Sph")
#' TN_UM <- defineUM(TRUE, distribution = "norm", 
#'                   distr_param = c(TN, TN_sd), crm = TN_crm,
#'                   id = "TN", cross_ids = "OC")
#' class(TN_UM)
#' 
#' OC <- list(OC_UM,TN_UM)
#' class(OC)
#' length(OC)
#' str(OC)
#'  
#' test1 <- defineMUM(OC,matrix(c(1,0.7,0.7,1), nrow=2, ncol=2))
#' test2 <- defineMUM(OC,matrix(c(1,0.7,0.6,1), nrow=2, ncol=2))
#' test3 <- defineMUM(OC,matrix(c(1,-0.7,-0.7,1.1), nrow=2, ncol=2))                  
#' test3 <- defineMUM(OC,matrix(c(1,1,1,1), nrow=2, ncol=2))  
#' 
#' # scalar
#' scalarUM <- defineUM(uncertain = TRUE, distribution = "norm", distr_param = c(1,2))                
#' scalarUM2 <- defineUM(uncertain = TRUE, distribution = "norm", distr_param = c(3,2))                
#' a <- defineMUM(UMlist = list(scalarUM, scalarUM2), matrix(c(1,0.7,0.7,1), nrow=2, ncol=2))
#' class(a)
#' 
#' @export
defineMUM <- function(UMlist, cormatrix, ...) {
  
  # check - UMlist must have at least two items
  
  # stopifnot("UMlist is list of MarginalNumericalSpatial or MarginalScalar and all elements are of the same class")
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



