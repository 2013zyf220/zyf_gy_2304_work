
library(raster)

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2302_k2/ARCGIS/TC')

TC_3 <- raster('TC_3.tif')
TC_3_NA <- NAvalue(TC_3)
print(TC_3_NA)

TC_3[is.na(TC_3[])] <- 0
writeRaster(TC_3, 'TC_4.tif', format='GTiff')