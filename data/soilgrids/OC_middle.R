# Get middle part of Africa, map and point model error.

library(raster)
library(rgdal)

OC_Africa <- raster("af_ORCDRC_T__M_sd1_250m.tif")
plot(OC_Africa)
Extent_middle <- drawExtent()
OC_middle <- crop(OC_Africa, Extent_middle)
plot(OC_middle)
wgs84 <- CRS("+init=epsg:4326")
OC_middle_wgs84 <- projectRaster(OC_middle, crs = wgs84)
projection(OC_middle_wgs84)
extent(OC_middle_wgs84)
# class       : Extent 
# xmin        : 23.39 
# xmax        : 39.76 
# ymin        : -11.35 
# ymax        : 7.982 
writeRaster(OC_middle_wgs84, "OC_middle_wgs84.tif", format = "GTiff")

# if want to use the same extent always:
OC_middle_wgs84 <- raster("OC_middle_wgs84.tif")
Extent_middle_wgs84 <- extent(OC_middle_wgs84)

gdal.dir <- shortPathName("C:/Program Files (x86)/GDAL")
ogr2ogr <- paste0(gdal.dir, "/ogr2ogr.exe")
system(paste(ogr2ogr, '-f \"ESRI Shapefile\" carbon_organic_middle.shp WFS:\"http://wfs.isric.org/geoserver/wosis/wfs" carbon_organic -clipsrc 23.39 -11.35 39.76 7.982'))
OC_middle_points <- readOGR(dsn = ".", layer = "carbon_organic_middle")
plot(OC_middle_wgs84)
plot(OC_middle_points, add = TRUE)


# Model error
OC_middle_points@data$points <- extract(OC_middle_wgs84, OC_middle_points)
plot(OC_middle_wgs84_points, add = TRUE, col = "red")

