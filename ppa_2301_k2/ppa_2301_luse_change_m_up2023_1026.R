library(terra)
library(data.table)
library(rgdal)
library(raster)
library(sf)
library(sp)
library(dplyr)
library(tidyr)
library(landscapemetrics)
library(ggplot2)
#rm(list = ls())
#=========================================================

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2');
grid_1_loc <- 'shp/outputs2/2301_river_5_2022.shp';  #to_be_set

grid_1 = shapefile(paste0(grid_1_loc));
grid_len <- length(grid_1)

grid_1b = vect(paste0(grid_1_loc)); #to_be_set
plot(grid_1b)

lusec_m1 <- function(f_year_a, f_year_b, f_grid_num){
  cat('Number:', f_grid_num);
  f_luse_a1 <- raster(paste0('raster/ppa_2301_cq_luse_', f_year_a,'.tif'));
  f_luse_a2 <- crop(f_luse_a1, grid_1[f_grid_num, ]);
  f_luse_a3 <- mask(f_luse_a2, grid_1[f_grid_num, ])
  plot(f_luse_a3);
  f_luse_a4 <- na.omit(getValues(f_luse_a3));
  
  f_luse_b1 <- raster(paste0('raster/ppa_2301_cq_luse_', f_year_b,'.tif'));
  f_luse_b2 <- crop(f_luse_b1, grid_1[f_grid_num, ]);
  f_luse_b3 <- mask(f_luse_b2, grid_1[f_grid_num, ])
  plot(f_luse_b3);
  f_luse_b4 <- na.omit(getValues(f_luse_b3));
  
  f_luse_r1 <- cbind(f_luse_a4, f_luse_b4);
  colnames(f_luse_r1) <- list('year_a','year_b');
  f_luse_row <- nrow(f_luse_r1);
  f_lusec_m <- matrix(0, nrow = 8, ncol = 8);
  
  for (ii in 1: f_luse_row){
    f_lusec_m[f_luse_r1[ii,1], f_luse_r1[ii,2]] <- f_lusec_m[f_luse_r1[ii,1], f_luse_r1[ii,2]] + 1
  }
  return(f_lusec_m)
}

#===========================

lusec_m1_res <- list();
lusec_m1_res_c1 <- rep(0, grid_len)

for(ii in 1: grid_len){
  lusec_m1_res[[ii]] <- lusec_m1(2001, 2021, ii); #to_be_set
  lusec_m1_res_c1[ii] <- lusec_m1_res[[ii]][1,8];
}
