library(terra)
library(data.table)
library(dplyr)
library(raster)
library(rgdal)
library(utils)
library(rgdal)
library(sf)
library(sp)
library(tidyr)
library(ggplot2)

#=========================================================

setwd('E:/zyf_gn/zyf_gn_2301_data');

extent_1 <- extent(c(106.4, 106.7, 29.4, 29.75)); #to_be_set
data_sel <- 1; #to_be_set
if(data_sel == 1){
  bh_1 <- raster(paste0('building_height_k2/CNBH10m_X107Y29_rep.tif'));
}else if(data_sel == 2){
  bh_1 <- raster(paste0('chongqing_bh/cq_bh4.tif'));
}else{
  cat('no input data');
}

bh_2 <- crop(bh_1, extent_1)
plot(bh_2)

grid_1 <- shapefile(paste0('ppa_2301_k2/shp/outputs2/2301_river_5_2022.shp')); #to_be_set
plot(grid_1, add = T)
grid_len <- length(grid_1);

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/shp/outputs2');

res_bh_6 <- list();
res_bh_7 <- list();
res_bh_6_len <- rep(0, grid_len);
res_bh_7_len <- rep(0, grid_len);
res_bh_6_mean <- rep(0, grid_len);
res_bh_7_mean <- rep(0, grid_len);
res_bh_6_std <- rep(0, grid_len);
res_bh_7_std <- rep(0, grid_len);
res_bh_ratio <- rep(0, grid_len);

bh_5_sum <- list();
bh_6_sum <- list();
for(ii in 1: grid_len){
  cat('Grid: ', ii);
  c_bh_3 <- crop(bh_2, grid_1[ii, ]);
  c_bh_4 <- mask(c_bh_3, grid_1[ii, ]);
  plot(c_bh_3);
  plot(c_bh_4);
  
  c_bh_5 <- getValues(c_bh_4);
  c_bh_6 <- na.omit(c_bh_5);
  c_bh_7 <- ifelse(is.na(c_bh_5), 0, c_bh_5);
  res_bh_6[[ii]] <- c_bh_6;
  res_bh_7[[ii]] <- c_bh_7;
  res_bh_6_len[ii] <- length(c_bh_6);
  res_bh_7_len[ii] <- length(c_bh_7);
  res_bh_7_mean[ii] <- mean(c_bh_7);
  res_bh_6_std[ii] <- sd(c_bh_6);
  res_bh_7_std[ii] <- sd(c_bh_7);
  res_bh_ratio[ii] <- res_bh_6_len[ii]/res_bh_7_len[ii]
  bh_5_sum[[ii]] <- c_bh_5;
  bh_6_sum[[ii]] <- c_bh_6;
  if(all(is.na(c_bh_5)) == TRUE){
    res_bh_6_mean[ii] <- 0;
  }else{
    res_bh_6_mean[ii] <- mean(c_bh_6);
  }
}

#==============================================

bh_f1 <- function(f_year){
  cat('Number: ', f_year);
  
  f_grid_1a <- shapefile(paste0('2301_river_5_', f_year,'.shp'));
  f_grid_1b <- spTransform(f_grid_1a, '+init=epsg:4326');
  f_grid_2 <- st_read(paste0('2301_river_5_', f_year,'.shp'));
  
  f_grid_2$bh_6_mean <- res_bh_6_mean
  f_grid_2$bh_7_mean <- res_bh_7_mean
  f_grid_2$bh_6_std <- res_bh_6_std
  f_grid_2$bh_7_std <- res_bh_7_std
  f_grid_2$bh_ratio <- res_bh_ratio
  #st_write(f_grid_2, paste0('2301_river_5b_', f_year,'.shp'))
  return(f_grid_2)
}  

#==============================================

year_s <- 2021; #to_be_set
year_e <- 2021; #to_be_set
res_list <- list();
for(c_year in year_s: year_e){
  res_list[[c_year]] <- bh_f1(c_year);
  bh_data_export <- matrix(0, nrow = grid_len, ncol = 7);
  bh_data_export[, 1] <- res_bh_6_len;
  bh_data_export[, 2] <- res_bh_7_len;
  bh_data_export[, 3] <- res_bh_6_mean;
  bh_data_export[, 4] <- res_bh_7_mean;
  bh_data_export[, 5] <- res_bh_6_std;
  bh_data_export[, 6] <- res_bh_7_std;
  bh_data_export[, 7] <- res_bh_ratio;
  colnames(bh_data_export) <- c("len_1", "len_2", "mean_1", "mean_2", "std_1", "std_2", "ratio")
  #write.csv(bh_data_export, file = paste0('2301_bh_', data_sel, '_', c_year,'.csv'), row.names = FALSE)
}

#==============================================

#display data
plot(bh_2)
plot(grid_1[2,], add = T) #to_be_set
