


defjointnonspatial <- function(uncertain, means, varcov) {

  jm <- list(uncertain = uncertain,
             means = means,
             varcov = varcov)

  class(jm) <- "JointNonSpatial"
  jm

}

## Example
# myjointnorm <- defjointnonspatial(uncertain = TRUE,
#                                   means = c(5, 8, 10),
#                                   varcov = matrix(c(1, 1.6, 0.2, 1.6, 4,
#                                                     -2.4, 0.2, -2.4, 8),
#                                                     nrow = 3, ncol = 3))
# myjointnorm
