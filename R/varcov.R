#' Calculate variance covariance matrix
#'
#' @param sd_vector vector of standard deviations
#' @param cormat correlation matrix
#'
#' @return variance-covariance matrix
#'
#' @examples
#' 
#' varcov(c(1,2,3), matrix(c(1,0.7,0.2,0.7,1,0.5,0.2,0.5,1), nrow = 3, ncol = 3))
#' 
#' 
varcov <- function(sd_vector, cormat) {
  sd_matrix <- diag(sd_vector)
  varcov_matrix <- sd_matrix %*% cormat %*% t(sd_matrix)
  varcov_matrix
}