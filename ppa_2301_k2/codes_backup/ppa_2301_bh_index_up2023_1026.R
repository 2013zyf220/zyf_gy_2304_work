library(terra)
library(data.table)
library(dplyr)
library(raster)
library(rgdal)
library(utils)
#=========================================================

setwd('E:/zyf_gn/zyf_gn_2301_data');

bh_1 <- raster(paste0('building_height_k2/CNBH10m_X107Y29_rep.tif'));
extent_1 <- extent(c(106.4, 106.8, 29.4, 29.8)); #to_be_set

bh_2 <- crop(bh_1, extent_1)
plot(bh_2)

grid_1 <- shapefile(paste0('ppa_2301_k2/shp/outputs2/2301_river_5_2022.shp')); #to_be_set
plot(grid_1, add = T)
grid_len <- length(grid_1);

bh_f1 <- function(f_grid_num){
  cat('Number: ', f_grid_num)
  f_res_list <- list();
  
  f_bh_2 <- crop(bh_1, grid_1[f_grid_num, ]);
  f_bh_3 <- mask(f_bh_2, grid_1[f_grid_num, ]);
  plot(f_bh_2);
  plot(f_bh_3);
  
  f_bh_4 <- getValues(f_bh_3);
  f_bh_5 <- na.omit(f_bh_4);
  f_bh_6 <- ifelse(is.na(f_bh_4), 0, f_bh_4);
  f_bh_5_len <- length(f_bh_5);
  f_bh_6_len <- length(f_bh_6);
  f_bh_5_mean <- mean(f_bh_5);
  f_bh_6_mean <- mean(f_bh_6);
  f_bh_5_std <- sd(f_bh_5);
  f_bh_6_std <- sd(f_bh_6);
  f_bh_ratio <- f_bh_5_len/f_bh_6_len
  
  f_res_list[['1']] <- f_bh_5;
  f_res_list[['2']] <- f_bh_6;
  f_res_list[['1_len']] <- f_bh_5_len;
  f_res_list[['2_len']] <- f_bh_6_len;
  f_res_list[['1_mean']] <- f_bh_5_mean;
  f_res_list[['2_mean']] <- f_bh_6_mean;
  f_res_list[['1_std']] <- f_bh_5_std;
  f_res_list[['2_std']] <- f_bh_6_std;
  f_res_list[['ratio']] <- f_bh_ratio;
  return(f_res_list);
}

#=========================================================

res_list <- list();
for(ii in 1: grid_len){
  res_list[[ii]] <- bh_f1(ii);
}


