#' Function to run Allier model of moisture content at wilting point.
#'
#' @param field.capasity data input
#' @param soil.porosity data input
#' @param beta0 model parameter
#' @param beta1 model parameter
#' @param beta2 model parameter
#' @param epsilon residuals from the lack of model fit
#' @param delta measurment error
#'
#' @return
#' @author Kasia Sawicka
#'
allierModel <- function(field.capasity, soil.porosity, beta0, beta1, beta2, epsilon, delta) {
  beta0 + beta1 * field.capasity + beta2 * soil.porosity + epsilon + delta
}
