
library(raster)

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2302_k2/ARCGIS/LUSEB')

luse_b3 <- raster('landuse_b3.tif')
luse_b3_NA <- NAvalue(luse_b3)
print(luse_b3_NA)

luse_b3[is.na(luse_b3[])] <- 1
writeRaster(luse_b3, 'landuse_b4.tif', format='GTiff')
