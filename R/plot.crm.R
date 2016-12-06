#' Plots correlogram model.
#'
#' @param crm Object of class "crm" as created by makecrm()
#' @param distance 
#' @param ylim 
#' @param ... Additional parameters.
#'
#' @return plot of correlogram model
#'
#' @examples
#' 
#' mycormodel <- makecrm(acf0 = 0.8, range = 300, model = "Exp")
#' plot(mycormodel, distance = 1)
#'  
#' @export
plot.crm <- function(crm, distance = 1, ylim = c(0,1), ...) {
  
  stopifnot(distance >= 0)
  acf0 <- as.numeric(crm[[1]])
  a <- as.numeric(crm[[2]])  # range
  crm_shape <- crm[[3]]
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

# mycormodel <- makecrm(acf0 = 0.8, range = 300, model = "Nug") # OK
# mycormodel <- makecrm(acf0 = 0.8, range = 700, model = "Sph") # OK
# mycormodel <- makecrm(acf0 = 0.5, range = 500, model = "Exp") # OK
# mycormodel <- makecrm(acf0 = 0.9, range = 30, model = "Lin") # OK
# mycormodel <- makecrm(acf0 = 0.7, range = 500, model = "Cir") # OK
# mycormodel <- makecrm(acf0 = 0.8, range = 300, model = "Pen") # OK
# mycormodel <- makecrm(acf0 = 0.8, range = 300, model = "Gau") # OK
# mycormodel <- makecrm(acf0 = 0.8, range = 300, model = "Bes") # OK
# mycormodel <- makecrm(acf0 = 0.8, range = 300, model = "Log") # ?
# mycormodel <- makecrm(acf0 = 0.8, range = 2, model = "Pow") # ?
# mycormodel <- makecrm(acf0 = 0.8, range = 2, model = "Per") # ?
# plot(mycormodel, distance = 1, ylim = c(-1,1))



