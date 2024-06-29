library(raster)
setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2302_k2/ARCGIS')

landuse_b4 <- raster('LUSEB/landuse_b4.tif')
landuse_b4[landuse_b4 == 1] <- 2

# 保存修改后的栅格数据
writeRaster(landuse_b4, 'LUSEB/landuse_b5.tif', format='GTiff')