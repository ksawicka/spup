graphics.off() # terminate graphics devices 
rm(list=ls()) # clean-up memory

library(maptools)
library(gstat)
library(rgdal)
library(lattice)
library(RColorBrewer)

# read and plot Zlatibor DEM:
dem30m = readGDAL("dem30m.asc")  
# dem30m = readAsciiGrid("dem30m.asc")  
names(dem30m) = "Elevation"
spplot(dem30m, zcol = "Elevation", col.regions=bpy.colors(), main = "Digital Elevation Model Zlatibor region")

# read elevation at control points
zlatibor = read.table("zlatibor.csv", sep = ",", header = TRUE)
dim(zlatibor)
names(zlatibor)
coordinates(zlatibor) = ~X+Y

# add DEM to zlatibor dataset using GIS overlay
zlatibor$DEM <- over(zlatibor,dem30m)$Elevation

# plot dem and control point locations
par(mar=c(0,0,0,0))
image(dem30m[1])
points(zlatibor, lwd=2)

# calculate DEM error at control points
zlatibor$error <- zlatibor$DEM - zlatibor$control

# summary measures and histogram
summary(zlatibor$error)
hist(zlatibor$error, col = "lightblue")

# correlation with DEM?
plot(zlatibor$DEM,zlatibor$error)

# now estimate the DEM bias and calculate a 95% confidence interval

bias <- mean(zlatibor$error)
lower <- mean(zlatibor$error)-1.64*sd(zlatibor$error)/sqrt(dim(zlatibor)[1])
upper <- mean(zlatibor$error)+1.64*sd(zlatibor$error)/sqrt(dim(zlatibor)[1])
bias; lower; upper
