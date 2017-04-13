#' Plots correlogram model
#'
#' @param x Object of class "SpatialCorrelogramModel" as created by makecrm().
#' @param distance minimum distance between locations (unit should correspond
#' with the unit of the range parameter in makecrm()).
#' @param ylim the y limits of the plot.
#' @param ... additional parameters.
#'
#' @return plot of correlogram model
#'
#' @author Kasia Sawicka, Gerard Heuvelink
#' 
#' @examples
#' 
#' mycormodel <- makecrm(acf0 = 0.8, range = 300, model = "Exp")
#' plot(mycormodel, distance = 1)
#' 
#' @importFrom gstat variogramLine
#' @importFrom graphics plot abline
#' 
#' @export
plot.SpatialCorrelogramModel <- function(x, distance = 1, ylim = c(0,1), ...) {
  
  stopifnot(distance >= 0)
  acf0 <- as.numeric(x[[1]])
  a <- as.numeric(x[[2]])  # range
  crm_shape <- x[[3]]
  if (crm_shape=="Nug") a <- 0  # default, only used in case of "Nug"
  n <- a/distance
  
  if (crm_shape == "Nug") xlim_factor <- 1
  if (crm_shape == "Sph") xlim_factor <- 1.2
  if (crm_shape == "Exp") xlim_factor <- 3.5
  if (crm_shape == "Lin") {
    stopifnot (a >= 0)
    xlim_factor <- 1.2
  }
  if (crm_shape == "Cir") xlim_factor <- 1.2
  if (crm_shape == "Pen") xlim_factor <- 1.2
  if (crm_shape == "Gau") xlim_factor <- 2.5
  if (crm_shape == "Bes") xlim_factor <- 5
  if (crm_shape == "Log") xlim_factor <- 1.2
  if (crm_shape == "Pow") {
    stopifnot(a > 0 & a <= 2)
    xlim_factor <- 1.2
  }
  if (crm_shape == "Per") xlim_factor <- 2
  
  crm_line <- gstat::variogramLine(gstat::vgm(psill = acf0,
                                              model = crm_shape,
                                              range = a,
                                              nugget = 1 - acf0),
                                              maxdist = xlim_factor * a,
                                              n = n,
                                              covariance = TRUE)

  plot(crm_line, type="l", ylim = ylim, 
       xlab = "Distance", ylab = "Correlation", ...)
  abline(h = 0, lty = 2)
  
}



