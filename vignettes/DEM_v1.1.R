## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(
    comment = NA,
    quiet = TRUE,
    progress = FALSE,
    tidy = FALSE,
    cache = FALSE,
    message = FALSE,
    error = FALSE, # FALSE: always stop execution.
    warning = TRUE,
    dpi = 100
)

## ---- echo = FALSE-------------------------------------------------------
knitr::opts_knit$set(global.par = TRUE)

## ---- echo = FALSE-------------------------------------------------------
par(mar = c(3, 3, 2, 2), mgp = c(1.7, 0.5, 0), las = 1, cex.main = 1, tcl = -0.2, cex.axis = 0.8,
    cex.lab = 0.8)

## ---- message = FALSE, warning = FALSE-----------------------------------
###########################################################################################################
# KS: I think here we should say how those maps been derived?

# GH: we should ask Tom Hengl or Branislav Bajat from Belgrade University. Is it not the SRTM DEM?

# KS: I think we know how they've been derived - from point interpolation (it's the same dataset as Damiano's). But we don't want to say it's point interpolation, becasue then we should use kriging for simulations.
###########################################################################################################

## ---- fig.width = 5, fig.height = 3--------------------------------------

# load packages
library(sp)
library(spup)

# load and view the data
data(DEM)
str(dem30m)
str(dem30m_sd)
spplot(dem30m, main = list(label = "Mean DEM", cex = 1))
summary(dem30m_sd)


## ------------------------------------------------------------------------
###########################################################################################################
# KS: I think we need here to explain how we can estimate these?

# GH: Yes we do, but I think we can write "Let us assume that the spatial autocorrelation of the DEM errors is an exponentially decreasing function with a short-distance correlation of 0.78 and a range parameter of 321." Why did you choose these values? AHA I remember it is based on control point data. So we can either explain how we derived the correlogram ("geostatistical analysis of the DEM residuals derived from DEM elevations with ground truth observations at XXX locations in the study area.") If we skip this I suggest to use acf0=0.8 and range=300.

# KS: OK. I think we should just use the first sentence (I already added it above) and not mention control data - people will be asking wy didn't we just do kriging?
###########################################################################################################

## ------------------------------------------------------------------------

# define spatial correlogram model
dem_crm <- makecrm(acf0 = 0.8, range = 300, model = "Exp")


## ------------------------------------------------------------------------
###########################################################################################################
# Can we ask users to make a plot of the dem_crm and help them along with interpreting it by asking a few simple questions?

# KS: I don't think we should be asking them questions in a vignette unless we answer them. I think we can write though how they can interpret it. My attempt below.
###########################################################################################################

## ---- fig.width = 5, fig.height = 3--------------------------------------

plot(dem_crm, main = "DEM correlogram")


## ---- fig.width = 7, fig.height = 5--------------------------------------

par(mfrow = c(2, 2))
crm <- makecrm(acf0 = 0.8, range = 700, model = "Sph") 
plot(crm, main = "'Spherical', acf0 = 0.8, range = 700")
crm <- makecrm(acf0 = 0.2, range = 700, model = "Sph") 
plot(crm, main = "'Spherical', acf0 = 0.2, range = 700")
crm <- makecrm(acf0 = 0.8, range = 700, model = "Lin") 
plot(crm, main = "'Linear', acf0 = 0.8, range = 700")
crm <- makecrm(acf0 = 0.8, range = 200, model = "Gau") 
plot(crm, main = "'Gausian', acf0 = 0.8, range = 200")


## ------------------------------------------------------------------------

# define uncertainty model for the DEM
demUM <- defineUMs(uncertain = TRUE, distribution = "norm", 
                   distr_param = c(dem30m, dem30m_sd), crm = dem_crm)


## ---- fig.width = 7, fig.height = 7--------------------------------------

# create possible realizations of the DEM
dem_sample <- genSample(UMobject = demUM, n = 100, samplemethod = "ugs", nmax = 20)

# view several realizations of DEM 
spplot(dem_sample, c(1:6), main = list(label = "Examples of the DEM realizations", cex = 1))


## ---- fig.width = 5, fig.height = 3--------------------------------------

# compute and plot the slope sample statistics
# e.g. mean and standard deviation
dem_sample_mean <- mean(dem_sample)
dem_sample_sd <- sd(dem_sample)
dem_stats <- cbind(dem_sample_mean, dem_sample_sd)
spplot(dem_stats, main = list(label = "Mean and standard dev. of the DEM realizations", cex = 1))


## ---- fig.width = 5, fig.height = 3--------------------------------------

dem_crm2 <- makecrm(acf0 = 0.2, range = 300, model = "Exp")
demUM2 <- defineUMs(uncertain = TRUE, distribution = "norm", distr_param = c(dem30m, dem30m_sd), crm = dem_crm2)
dem_sample2 <- genSample(UMobject = demUM2, n = 100, samplemethod = "ugs", nmax = 20)
spplot(dem_sample, c(1), main = list(label = "dem_sample, acf0 = 0.8, range = 300m", cex = 1))
spplot(dem_sample2, c(1), main = list(label = "dem_sample2, acf0 = 0.2, range = 300m", cex = 1))


## ------------------------------------------------------------------------
###########################################################################################################
# KS :Or at least it should. Trouble is there is not that much difference becasue the epsilon is so small in comparison to the mean or sd DEM. I wonder if I do it correctly? Or in this specific case it will be like that?
###########################################################################################################

## ------------------------------------------------------------------------

# view the model
Slope


## ------------------------------------------------------------------------

# run uncertainty propagation
slope_sample <- propagate1(dem_sample, model = Slope, n = 100)


## ---- fig.width = 7, fig.height = 7--------------------------------------

# view the sample of the model output
spplot(slope_sample[c(1:6)], main = list(label = "Examples of the slope realizations", cex = 1))


## ---- fig.width = 5, fig.height = 3--------------------------------------

# compute and plot the slope sample statistics
# e.g. mean and standard deviation
slope_mean <- mean(slope_sample)
slope_sd <- sd(slope_sample, na.rm = TRUE)
slope_stats <- cbind(slope_mean, slope_sd)
spplot(slope_stats, main = list(label = "Mean and standard dev. of slope realizations", cex = 1))

# check the histogram of slope at at some random locations
par(mfrow = c(2,2))
hist(as.numeric(slope_sample@data[1256,]), main = "Location A", xlab = "Slope")
hist(as.numeric(slope_sample@data[2000,]), main = "Location B", xlab = "Slope")
hist(as.numeric(slope_sample@data[5000,]), main = "Location C", xlab = "Slope")
hist(as.numeric(slope_sample@data[5781,]), main = "Location D", xlab = "Slope")

# or quantiles
slope_q <- quantile(slope_sample, probs = c(0.1, 0.25, 0.75, 0.9), na.rm = TRUE)
spplot(slope_q, mail = list(label = "Quantiles of slope realizations", cex = 1))


## ------------------------------------------------------------------------
###########################################################################################################
# GH: Here I think we should ask users to interpret the visualised results. For example where are the highs and lows of the slope sd, is it the same as for DEM uncertainty (I expect not), check that quantile information agrees with sd, check the histogram of slope at a few locations (is it normal), etc. Perhaps invite them to repeat the analysis with a stronger or weaker spatial autocorrelation and compare results, etc. OK it should not become a computer practical but a bit of discussion and interpretation of of the results of the MC uncertainty propagation analysis is useful.

# KS: OK. I agree. I'll add the text subsequently, but can we first check if the calculations, especially of sd are OK? Becasue the sd calculated from the monte carlo sample both for the DEM and slope gets very low values. I just used standard R sd function to be applied row by row in a spatial grid data frame that is the Monte Carlo sample of DEM or slope. Additionally: what do you mean by 'agree' in "check that quantile information agrees with sd"?
###########################################################################################################

