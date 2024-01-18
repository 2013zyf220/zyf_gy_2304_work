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

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/shp/6')

vari_1 <- 'ndvi' #to_be_set_key
buffer_set <- 'd01s5' #to_be_set_key
buffer_set2 <- 5  #to_be_set_key

if(vari_1 == 'ndvi'){
  img_1 <- raster(paste0('res2/ppa_2301_ndvi_img_s', buffer_set2, 'p.tif'))
}else if(vari_1 == 'terrain'){
  img_1 <- raster('res2/ppa_2301_dem_gee.tif')
}else{
  print('ERROR')
}

shp_1 <- shapefile(paste0('arcgis/cq_water_', buffer_set, '.shp')) #to_be_set
#======================================


data_mean_f <- function(f_grid_num){
  cat('f_grid_num', f_grid_num)
  f_data_1 <- crop(img_1, extent(shp_1[f_grid_num,]))
  f_data_2 <- mask(f_data_1, shp_1[f_grid_num,])
  
  plot(shp_1[f_grid_num,])
  plot(f_data_2, add = T)
  
  f_data_3 <- getValues(f_data_2)
  f_data_3_mean <- mean(f_data_3, na.rm = TRUE)
  return(f_data_3_mean)
}

grid_len <- 185 #to_be_set
data_mean <-  matrix(0, nrow = grid_len, ncol = 2) 
data_mean[,1] <- 1: grid_len
for(ii in 1: grid_len){
  data_mean[ii,2] <- data_mean_f(ii)
}

colnames(data_mean) <- c('NUMBER', vari_1)
write.csv(data_mean, file = paste0('res2/ppa_2301_data_', vari_1, '_', buffer_set,'.csv'), row.names = FALSE)


