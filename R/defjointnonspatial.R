#' Definition of joint non-spatial uncertainty model
#'
#' Function to define joint uncertainty of cross-correlated spatial objects to do Monte Carlo.
#'
#' @param uncert.objects Objects created by defnumarspatial
#' @param ...
#'
#' @usage defjointnonspatial(uncertain, means, varcov)
#' @return Joint uncertainty model of cross-correlated spatial objects
#' @author Kasia Sawicka, Gerard Heuvelink
#' @examples
#'
#' myjointnorm <- defjointnonspatial(uncertain = TRUE,
#'                                  means = c(5, 8, 10),
#'                                  varcov = matrix(c(1, 1.6, 0.2, 1.6, 4,
#'                                                    -2.4, 0.2, -2.4, 8),
#'                                                   nrow = 3, ncol = 3))
#' myjointnorm
#'
#' @export
#'
defjointnonspatial <- function(uncertain, means, varcov) {

  jm <- list(uncertain = uncertain,
             means = means,
             varcov = varcov)

  class(jm) <- "JointNonSpatial"
  jm

}

