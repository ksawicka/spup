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
# define spatial correlogram model
dem_crm <- makecrm(acf0 = 0.8, range = 300, model = "Exp")

## ---- fig.width = 5, fig.height = 3--------------------------------------
plot(dem_crm, main = "DEM correlogram")

## ---- fig.width = 7, fig.height = 5--------------------------------------
par(mfrow = c(2, 2))
crm <- makecrm(acf0 = 0.8, range = 700, model = "Sph") 
plot(crm, main = "'Spherical', acf0 = 0.8, range = 700")
crm <- makecrm(acf0 = 0.2, range = 700, model = "Sph") 
plot(crm, main = "'Spherical', acf0 = 0.2, range = 700")
crm <- makecrm(acf0 = 0.8, range = 700, model = "Lin") 
plot(crm, main = "'Linear', acf0 = 1.0, range = 700")
crm <- makecrm(acf0 = 0.8, range = 200, model = "Gau") 
plot(crm, main = "'Gausian', acf0 = 0.8, range = 200")

## ------------------------------------------------------------------------
# define uncertainty model for the DEM
demUM <- defineUM(uncertain = TRUE, distribution = "norm", 
                   distr_param = c(dem30m, dem30m_sd), crm = dem_crm)
class(demUM)

## ---- fig.width = 7, fig.height = 7--------------------------------------
# create possible realizations of the DEM
dem_sample <- genSample(UMobject = demUM, n = 100, samplemethod = "ugs", nmax = 20, asList = FALSE)

# view several realizations of DEM
spplot(dem_sample[c(5,6,3,4,1,2)], main = list(label = "Examples of the slope realizations", cex = 1))

## ---- fig.width = 5, fig.height = 6--------------------------------------
# compute and plot the slope sample statistics
# e.g. mean and standard deviation
dem_sample_mean <- mean(dem_sample)
dem_sample_sd <- sd(dem_sample)
m <- spplot(dem_sample_mean, main = list(label = "Mean of the DEM realizations", cex = 1))
s <- spplot(dem_sample_sd, main = list(label = "Standard dev. of the DEM realizations", cex = 1))
print(m, split = c(1, 1, 1, 2), more = TRUE)
print(s, split = c(1, 2, 1, 2))
rm(m, s)

## ---- fig.width = 5, fig.height = 6--------------------------------------
dem_crm2 <- makecrm(acf0 = 0.2, range = 300, model = "Exp")
demUM2 <- defineUM(uncertain = TRUE, distribution = "norm", distr_param = c(dem30m, dem30m_sd), crm = dem_crm2)
dem_sample2 <- genSample(UMobject = demUM2, n = 100, samplemethod = "ugs", nmax = 20, asList = FALSE)
s1 <- spplot(dem_sample, c(1), main = list(label = "dem_sample, acf0 = 0.8, range = 300m", cex = 1))
s2 <- spplot(dem_sample2, c(1), main = list(label = "dem_sample2, acf0 = 0.2, range = 300m", cex = 1))
print(s1, split = c(1, 1, 1, 2), more = TRUE)
print(s2, split = c(1, 2, 1, 2))
rm(s1, s2)

## ------------------------------------------------------------------------
# view the model
Slope

## ------------------------------------------------------------------------
# coerce  SpatialGridDataFrame to a list of individual SpatialGridDataFrames
dem_sample <- map(1:ncol(dem_sample), function(x){dem_sample[x]})

# or sample from uncertain input and save it in a list
dem_sample <- genSample(UMobject = demUM, n = 100, samplemethod = "ugs", nmax = 20, asList = TRUE)

## ------------------------------------------------------------------------
# run uncertainty propagation
slope_sample <- propagate(dem_sample, model = Slope, n = 100, projection = CRS("+init=epsg:3857"))

## ---- fig.width = 7, fig.height = 7--------------------------------------
# coerce slopes list to a SpatialGridDataFrame
s <- slope_sample[[1]]
for (i in 2:length(slope_sample)) {
  s@data[i] <- slope_sample[[i]]@data
}
names(s@data) <- paste("slope", c(1:ncol(s)), sep = "")
slope_sample <- s
rm(s)

# view the sample of the model output
spplot(slope_sample[c(5,6,3,4,1,2)], main = list(label = "Examples of the slope realizations", cex = 1))

## ---- fig.width = 5, fig.height = 6--------------------------------------
# compute and plot the slope sample statistics
# e.g. mean and standard deviation
slope_mean <- mean(slope_sample)
slope_sd <- sd(slope_sample, na.rm = TRUE) # na.rm = TRUE is necessary, because slope cannot be calculated at the border
m <- spplot(slope_mean, main = list(label = "Mean of the slope realizations", cex = 1))
s <- spplot(slope_sd, main = list(label = "Standard dev. of the slope realizations", cex = 1))
print(m, split = c(1, 1, 1, 2), more = TRUE)
print(s, split = c(1, 2, 1, 2))
rm(m, s)

## ---- fig.width = 7, fig.height = 3--------------------------------------
# check the histogram of slope at lowest and highest DEM 
l <- which(dem30m@data == min(dem30m@data)) # there is no slope calculated for this outer value so we need to fins a different one
l <- which(dem30m@data < 860)
l
# lets take location 400 for l.
h <- which(dem30m@data == max(dem30m@data))
h
l_mean <- mean(as.numeric(slope_sample@data[400,]))
l_sd <- sd(as.numeric(slope_sample@data[400,]))
h_mean <- mean(as.numeric(slope_sample@data[6898,]))
h_sd <- sd(as.numeric(slope_sample@data[6898,]))

par(mfrow = c(1,2))
hist(as.numeric(slope_sample@data[400,]), main = paste("Slope at low DEM,", "\n",
     "mean = ", round(l_mean, 2), ", sd = ", round(l_sd, 2), sep = ""), xlab = "Slope")
hist(as.numeric(slope_sample@data[6898,]), main = paste("Slope at high DEM,", "\n",
     "mean = ", round(h_mean, 2), ", sd = ", round(h_sd, 2), sep = ""), xlab = "Slope")
rm(l, h)

## ---- fig.width = 7, fig.height = 5--------------------------------------
# or quantiles
slope_q <- quantile(slope_sample, probs = c(0.1, 0.25, 0.75, 0.9), na.rm = TRUE)
spplot(slope_q[c(3,4,1,2)], mail = list(label = "Quantiles of slope realizations", cex = 1))

## ---- fig.width = 5, fig.height = 3--------------------------------------
slope_q$safep90 <- factor(ifelse(slope_q$prob90perc > 0.5, 1, 0), labels = c("safe","hazard"))
spplot(slope_q, "safep90", col.regions = c("green","red"), main = "Safe areas for skiing")

