# Get gsif data for 'spup'.

library(sp)
library(GSIF)

# -------------------------- Try cookfarm data -------------------------------------

data(cookfarm)

names(cookfarm$grids) # Descriptions from http://gsif.r-forge.r-project.org/cookfarm.html
# "DEM" - numeric; Digital Elevation Model
# "TWI" - numeric; SAGA GIS Topographic Wetness Index
# "MUSYM" - factor; soil mapping units e.g. "Thatuna silt loam"
# "NDRE.M" - numeric; mean value of the Normalized Difference Red Edge Index (time series of 11 RapidEye images)
# "NDRE.Sd" - numeric; standard deviation of the Normalized Difference Red Edge Index (time series of 11 RapidEye images)
# "Cook_fall_ECa" - numeric; apparent electrical conductivity image from fall
# "Cook_spr_ECa" - numeric; apparent electrical conductivity image from spring
# "X2011" - factor; cropping system in 2011
# "X2012" - factor; cropping system in 2012
# "x" - coordinates
# "y" - coordinates

dem <- cookfarm$grid
coordinates(dem) <- ~x+y
class(dem)
spplot(dem["DEM"])

# OK, but can we make up a model for these data?
# -----------------------------------------------------------------------------------


# ------------- Try afsp data (Africa Soil Profile data) ----------------------------

data(afsp)
str(afsp)

afsp_sites <- afsp[[1]]
afsp_horizons <- afsp[[2]]

# get coordinates from _sites to _horizons
afsp_horizons <- merge(afsp_horizons, afsp_sites)

# coerce to spatial object
coordinates(afsp_horizons) <- ~LONWGS84+LATWGS84
spplot(afsp_horizons["ORCDRC"])
spplot(afsp_horizons["NTO"])

# OK.

# ------------------------------ Prep soil grids -----------------------------------

library(raster)

OC_af_0_5 <- raster("af_ORCDRC_T__M_0_5_cm_250m.tif")
OC_af_5_15 <- raster("af_ORCDRC_T__M_5_15_cm_250m.tif")
OC_af_15_30 <- raster("af_ORCDRC_T__M_15_30_cm_250m.tif")
TN_af_0_30 <- raster("af_NTO_T__M_0_30_cm_250m.tif")

plot(OC_af_0_5)

# average three OC maps weighted by depth
OC_af_0_30 <- stack(OC_af_0_5, OC_af_5_15, OC_af_15_30)
OC_af_0_30 <- weighted.mean(OC_af_0_30, w = c(5, 10, 15))
writeRaster(OC_af_0_30, "af_ORCDRC_T__M_0_30_cm_calculated_250m.tif", format = "GTiff")
plot(OC_af_0_30)


# ------------------------------ Work out point data --------------------------------
# Averaging over 0 - 30 cm depth.
# Need to do this for locations where both N and OC are measured.

























