library(raster)
setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/V2/DATA_LUSE_1');

year_s <- 2022 #to_be_set
year_e <- 2022 #to_be_set
year_len <- year_e - year_s + 1

matrix_1 <- c(1, 4, 1) #to_be_set
rclmat <- matrix(matrix_1, ncol = 1, byrow = TRUE)

luse_transfer <- function(f_year){
  f_luse_1 <- raster(paste0('ppa_2301_',f_year,'_luse_2.tif')) 
  f_luse_2 <- reclassify(f_luse_1 , rclmat)
  writeRaster(f_luse_2, paste0('ppa_2301_',f_year,'_luse_3.tif'), format = "GTiff", overwrite = TRUE)
  
  f_res <- list();
  f_res[[1]] <- f_luse_1;
  f_res[[2]] <- f_luse_2;
  
  return(f_res)
}
  
luse_2 <- list();
ii <- 1;
for (c_year in year_s: year_e){
  luse_2[[ii]] <- luse_transfer(c_year)
  ii <- ii + 1;
}

plot(luse_2[[1]][[2]]) #to_be_set

#============================
#count the number of values of 2 raster files

hist_1_v <- matrix(-9999, nrow = year_len, ncol = 8);
hist_2_v <- matrix(-9999, nrow = year_len, ncol = 8);

for (ii in 1: year_len){
  for (jj in 1: 8){
    hist_1_v[ii,jj] <- sum(getValues(luse_2[[ii]][[1]]) == jj);
    hist_2_v[ii,jj] <- sum(getValues(luse_2[[ii]][[2]]) == jj);   
  }
}
