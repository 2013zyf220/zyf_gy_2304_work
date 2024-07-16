library(raster)
library(terra)
library(sf)
library(landscapemetrics)


#=================================================================
#up2024_0604_14:11

setwd('D:/zyf_gn/zyf_gn_2301_data/ppa_2302_k1')
tif_1 <- raster('Southwest_Chongqing/Southwest_Chongqing.tif')

#=================================================================
#up2024_0604_14:11

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2302_k2/ARCGIS')

shp_1 <- shapefile(paste0('BUF1/ppa_2302_buf2.shp'))
shp_1p <- spTransform(shp_1, crs(tif_1))

tif_2 <- crop(tif_1, shp_1p)
tif_3 <- mask(tif_2, shp_1p)

writeRaster(tif_3, 'LUSE/luse_1.tif', filetype = 'GTiff', overwrite = TRUE)
