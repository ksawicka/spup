# Use ISRIC soil grids for spatial uncertainty propagation analysis example.
# Data - map of organic carbon and total nitrogen
# Data source - http://www.isric.org/content/african-soilgrids-250m-geotiffs
# Model - C/N

# library(raster)

OC_Africa <- raster("af_ORCDRC_T__M_sd1_250m.tif")

# get smaller subsets for quicker analysis
plot(OC_Africa)
Extent_Madagaskar <- drawExtent()
plot(Extent_Madagaskar)

OC_Madagaskar <- crop(OC_Africa, Extent_Madagaskar)
plot(OC_Madagaskar)
writeRaster(OC_Madagaskar, "OC_Madagaskar_250m.tif", format = "GTiff")
rm(OC_Africa)

TN_Africa <- raster("af_NTO_T__M_xd1_250m.tif")
TN_Madagaskar <- crop(TN_Africa, Extent_Madagaskar)
plot(TN_Madagaskar)
writeRaster(TN_Madagaskar, "TN_Madagaskar_250m.tif", format = "GTiff")
rm(TN_Africa)


# --------------------------------------------------------------------------------------------------------
# ------------------------------ Was trying this before known about GSIF package -------------------------
# --------------------------------------------------------------------------------------------------------

# # use WOSIS data 
# # (http://www.isric.org/data/wosis, http://www.isric.org/sites/default/files/WoSIS_tutorial_R_0.pdf)
# # from the selected region to calculate the map errors at point locations
# # and use this to build a statistical model of the error in the soilgrids map.
# # In fact it would not be necessary that the point data are from the same region,
# # they could be from another (larger) region because they are only used to
# # calibrate the error model. In fact it would probably be better if these data
# # are not from the same region because in such case one might argue that these
# # data should not only be used to calibrate the error model but also to coindition
# # the predictions at prediction locations within the study area (in which case we
# # would be ending up with kriging again).
# 
# 
# 
# # c:\Program Files (x86)\
# gdal.dir <- shortPathName("C:/Program Files (x86)/GDAL")
# ogr2ogr <- paste0(gdal.dir, "/ogr2ogr.exe")
# ogrinfo <- paste0(gdal.dir, "/ogrinfo.exe")
# system(paste(ogrinfo, '-ro WFS:\"http://wfs.isric.org/geoserver/wosis/wfs\"'))
# # ok, that works.
# 
# # example how to grab clay points for France:
# # (next line needs to be in one line, and file .shp cannot exist in folder)
# system(paste(ogr2ogr, '-f \"ESRI Shapefile\" clay_total_sub.shp WFS:\"http://wfs.isric.org/geoserver/wosis/wfs" clay_total -clipsrc 0 45 10 50'))
# library(rgdal)
# clay_total_sub <- readOGR("clay_total_sub.shp", "clay_total_sub") # this line is weird.
# 
# # get the Lon, Lat of Madagaskar area corners:
# #define crs
# wgs84 <- CRS("+init=epsg:4326")
# OC_Madagaskar_wgs84 <- projectRaster(OC_Madagaskar, crs = wgs84)
# projection(OC_Madagaskar_wgs84)
# extent(OC_Madagaskar_wgs84)
# # class       : Extent 
# # xmin        : 41.4 
# # xmax        : 53.1 
# # ymin        : -26.61 
# # ymax        : -11.53
# 
# # so, try to grab organic carbon for Madagaskar:
# system(paste(ogr2ogr, '-f \"ESRI Shapefile\" carbon_organic_Madagaskar.shp WFS:\"http://wfs.isric.org/geoserver/wosis/wfs" carbon_organic -clipsrc 41.4 -26.61 53.1 -11.53'))
# 
# # So, alternatively use this from cmd (this will save data into your user prifile folder, e.g. c:\Users\sawic002)
# # ogr2ogr -f "ESRI Shapefile" carbon_organic_Madagaskar.shp WFS:"http://wfs.isric.org/geoserver/wosis/wfs" carbon_organic -clipsrc 41.4 -26.61 53.1 -11.53
# library(rgdal)
# OC_Madagaskar_points <- readOGR(dsn = ".", layer = "carbon_organic_Madagaskar")
# 
# library(raster)
# OC_Madagaskar <- raster("OC_Madagaskar_250m.tif")
# wgs84 <- CRS("+init=epsg:4326")
# OC_Madagaskar_wgs84 <- projectRaster(OC_Madagaskar, crs = wgs84)
# plot(OC_Madagaskar_wgs84)
# plot(OC_Madagaskar_points, add = TRUE)
# 
# # OK, cool. Let's do that for the whole Africa
# OC_Africa <- raster("af_ORCDRC_T__M_sd1_250m.tif")
# plot(OC_Africa)
# wgs84 <- CRS("+init=epsg:4326") 
# OC_Africa_wgs84 <- projectRaster(OC_Africa, crs = wgs84)
# writeRaster(OC_Africa_wgs84, "OC_Africa_wgs84.tif", format = "GTiff")
# Extent_OC_Africa_wgs84 <- extent(OC_Africa_wgs84)
# Extent_OC_Africa_wgs84
# # class       : Extent 
# # xmin        : -23.21 
# # xmax        : 56.9 
# # ymin        : -34.85 
# # ymax        : 37.55 
# system(paste(ogr2ogr, '-f \"ESRI Shapefile\" carbon_organic_Africa.shp WFS:\"http://wfs.isric.org/geoserver/wosis/wfs" carbon_organic -clipsrc -23.21 -34.85 56.9 37.55'))
# OC_Africa_points <- readOGR(dsn = ".", layer = "carbon_organic_Africa")
# plot(OC_Africa_wgs84)
# plot(OC_Africa_points, add = TRUE)


