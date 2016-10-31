# 
# 
# genSample <- function(uncert.object, n, ...) UseMethod("genSample")
# 
# genSample.JointNonSpatial <- function(uncert.object, n, ...) {
#   # check for needed packages
#   require(mvtnorm)
# 
#   means <- uncert.object[[2]]
#   varcov <- uncert.object[[3]]
#   samples <- rmvnorm(n, means, varcov)
#   samples
# }
# 
# ## Example:
# # myjointnorm <- defjointnonspatial(uncertain = TRUE,
# #                                   means = c(5, 8, 10),
# #                                   varcov = matrix(c(1, 1.6, 0.2, 1.6, 4,
# #                                                     -2.4, 0.2, -2.4, 8),
# #                                                   nrow = 3, ncol = 3))
# # myjointnonspatialsamples <- genSample(myjointnorm, 10)
# # myjointnonspatialsamples
