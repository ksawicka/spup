#' Definition of joint spatial uncertainty model
#'
#' Function to define joint uncertainty of cross-correlated spatial objects to do Monte Carlo.
#'
#' @param uncert.objects Objects created by defnumarspatial
#' @param ...
#'
#' @usage defjointspatial(uncert.objects, ...)
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
defjointspatial <- function(uncert.objects, ...) {



  jm <- list(uncertain = uncertain,
             means = means,
             varcov = varcov)

  class(jm) <- "JointSpatial"
  jm

}

## Example
# myjointnorm <- defjointnonspatial(uncertain = TRUE,
#                                   means = c(5, 8, 10),
#                                   varcov = matrix(c(1, 1.6, 0.2, 1.6, 4,
#                                                     -2.4, 0.2, -2.4, 8),
#                                                   nrow = 3, ncol = 3))
# myjointnorm






#
# # Example===============================================================================================
#
# data(allier)
#
# g <- gstat(id = c("fc"), formula = fc~1, data = Allier,
#            model = vgm(0.0027,"Sph",480,0.0013))
# g <- gstat(g, id = "por", formula = por~1, data = Allier,
#            model = vgm(0.0029,"Sph",480,0.0008))
# g <- gstat(g, id = c("fc", "por"),
#            model = vgm(0.0013,"Sph",480,-0.0008))
# g <- fit.lmc(variogram(g), g)
#
# Allier.krig <- predict(g, Allier.grd)
#
# str(Allier.krig)
# # Formal class 'SpatialGridDataFrame' [package "sp"] with 4 slots
# # ..@ data       :'data.frame':	8400 obs. of  5 variables:
# #   .. ..$ fc.pred   : num [1:8400] NA NA NA NA NA NA NA NA NA NA ...
# # .. ..$ fc.var    : num [1:8400] NA NA NA NA NA NA NA NA NA NA ...
# # .. ..$ por.pred  : num [1:8400] NA NA NA NA NA NA NA NA NA NA ...
# # .. ..$ por.var   : num [1:8400] NA NA NA NA NA NA NA NA NA NA ...
# # .. ..$ cov.fc.por: num [1:8400] NA NA NA NA NA NA NA NA NA NA ...
# # ..@ grid       :Formal class 'GridTopology' [package "sp"] with 3 slots
# # .. .. ..@ cellcentre.offset: Named num [1:2] 12.5 12.5
# # .. .. .. ..- attr(*, "names")= chr [1:2] "x" "y"
# # .. .. ..@ cellsize         : num [1:2] 25 25
# # .. .. ..@ cells.dim        : int [1:2] 100 84
# # ..@ bbox       : num [1:2, 1:2] 0 0 2500 2100
# # .. ..- attr(*, "dimnames")=List of 2
# # .. .. ..$ : chr [1:2] "x" "y"
# # .. .. ..$ : chr [1:2] "min" "max"
# # ..@ proj4string:Formal class 'CRS' [package "sp"] with 1 slot
# # .. .. ..@ projargs: chr NA
#
# field.c <- Allier.krig
# field.c@data$por.pred <- NULL
# field.c@data$por.var <- NULL
# field.c@data$cov.fc.por <- NULL
#
# soil.p <- Allier.krig
# soil.p@data$fc.pred <- NULL
# soil.p@data$fc.var <- NULL
# soil.p@data$cov.fc.por <- NULL
#
# fc.crm <- makecrm(acf = 0.5, range = 480, model = "Sph")
# fc.uncert <- defnummarspatial(uncertain = TRUE, sp.obj = field.c, crm = fc.crm, mask = Allier.grd)
# sp.crm <- makecrm(acf = 0.3, range = 480, model = "Sph")
# sp.uncert <- defnummarspatial(uncertain = TRUE, sp.obj = soil.p, crm = sp.crm, mask = Allier.grd)
#
# # 1. In case fc and sp weren't cross-correlate we need to provide
# #    variance-covariance matrix (symetrical and semi-positive).
#
# # 2. First call od defnummarspatial() should create 1x1 matrix like that
# #    - what's the value? NA?
#
# # 3. Second call should add row and column to this matrix if in this and
# #    first call it was specified that these objects are cross-correlated.
#
# # 4. In case these two are spacially cross-corelated co-criging already produces
# #    matrix of variance-covariance, but not for erros. So is the procedure like above
# #    and we ignore this crreated by co-kriging matrix?
#
#
#
#
#
#
#
#
#
#
#
#
#
#
