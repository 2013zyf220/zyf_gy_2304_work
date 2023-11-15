library(raster)

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/raster')
disr_1 <- raster('a2301_disr3.tif')
disr_1[disr_1 >= 1200] <- 0

plot(disr_1)

