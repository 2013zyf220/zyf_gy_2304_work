library(raster)
setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/raster');
year = 2018 #to_be_set
luse_1 <- raster(paste0('ppa_2301_cq_luse_',year,'.tif')) #to_be_set ('ppa_2301_cq_luse_' or 'ppa_2301_cq_lusec_')

matrix_1 <- c(1, 4, 1) #to_be_set
rclmat <- matrix(matrix_1, ncol = 1, byrow = TRUE)
luse_1b <- reclassify(luse_1 , rclmat)
writeRaster(luse_1b, paste0('ppa_2301_cq_luseb_',year,'.tif'), format = "GTiff", overwrite = TRUE)

plot(luse_1b)

#============================
#count the number of values of 2 raster files

hist_1_v <- rep(-9999,8);
hist_1b_v <- rep(-9999,8);
for (ii in 1: 8){
  hist_1_v[ii] <- sum(getValues(luse_1) == ii);
  hist_1b_v[ii] <- sum(getValues(luse_1b) == ii);
}

#============================