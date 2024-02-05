library(raster)
setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/shp/6/FIG/STUDY_AREA');

luse_1 <- raster('cq_luse_1.tif')
luse_2 <- luse_1
luse_2[luse_2 == 2] <- 1
luse_2[luse_2 == 3] <- 1
luse_2[luse_2 == 4] <- 1
luse_2[luse_2 == 7] <- 21

hist_1_v <- rep(0, 8)
for (ii in 1: 8){
  hist_1_v[ii] <- sum(getValues(luse_2) == ii) #to_be_set
}
  
print(hist_1_v)
writeRaster(luse_2, paste0('cq_luse_2.tif'), format = "GTiff", overwrite = TRUE)
  
