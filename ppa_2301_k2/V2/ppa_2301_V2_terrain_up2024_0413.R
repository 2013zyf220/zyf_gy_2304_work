library(rgdal)
library(raster)
library(sf)
library(sp)
library(dplyr)
library(tidyr)
library(landscapemetrics)
library(ggplot2)
library(horizon)
#rm(list = ls())

#==================================

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/V2')

buf_set <- '4'  #to_be_set_key
shp_1 <- shapefile(paste0('DATA_SHP_1/BUF/cq_water_bs', buf_set, 'ip.shp'))
img_f <- function(f_vari){
  if(f_vari == 'ndvi'){
    f_img_1 <- raster(paste0('DATA_VEG_1/ppa_2301_ndvi_gee_img_s', buf_set, 'p.tif'))
  }else if(f_vari == 'dem'){
    f_img_1 <- raster('DATA_TERRAIN_1/ppa_2301_dem_gee.tif')
  }else if(f_vari == 'slope'){
    f_img_1 <- raster('DATA_TERRAIN_1/ppa_2301_slope_gee.tif')
  }else if(f_vari == 'aspect'){
    f_img_1 <- raster('DATA_TERRAIN_1/ppa_2301_aspect_gee.tif')
  }else{
    print('ERROR')
  } 
  return(f_img_1)
}

#======================================

data_mean_f <- function(f_vari, f_grid_num){
  cat('f_grid_num', f_grid_num)
  f_img_1 <- img_f(f_vari)
  f_data_1 <- crop(f_img_1, extent(shp_1[f_grid_num,]))
  f_data_2 <- mask(f_data_1, shp_1[f_grid_num,])
  
  plot(f_data_2)
  plot(shp_1[f_grid_num, ], add = T)
  
  f_data_3 <- getValues(f_data_2)
  f_data_3_mean <- mean(f_data_3, na.rm = TRUE)
  return(f_data_3_mean)
}

grid_len <- 185 #to_be_set
data_mean <-  matrix(0, nrow = grid_len, ncol = 5) 
data_mean[,1] <- 1: grid_len
for(ii in 1: grid_len){
  data_mean[ii,2] <- data_mean_f('ndvi', ii)
  data_mean[ii,3] <- data_mean_f('dem', ii)
  data_mean[ii,4] <- data_mean_f('slope', ii)
  data_mean[ii,5] <- data_mean_f('aspect', ii)
}

df <- data.frame(NUMBER = data_mean[,1], ndvi = data_mean[,2], dem = data_mean[,3], slope = data_mean[,4], aspect = data_mean[,5])
write.csv(df, file = paste0('DATA_ANA_1/ppa_2301_terrain_', buf_set,'.csv'), row.names = FALSE)


