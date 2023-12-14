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

setwd('E:/zyf_gn/zyf_gn_2301_data')

bh_set <- 4 #to_be_set_key
buffer_set <- 200 #to_be_set_key

shp_1a <- shapefile(paste0('ppa_2301_k2/shp/2/2301_cq_water_b12_buf', buffer_set, '_a01.shp')) #to_be_set
shp_1b <- spTransform(shp_1a, '+init=epsg:4326');

svf_mean_f <- function(f_grid_num){
  cat('f_grid_num', f_grid_num)
  f_svf_1 <- raster(paste0('ppa_2301_k2/raster1/svf_bh_', bh_set, '_grid_', f_grid_num, '.tif'))
  f_svf_2 <- crop(f_svf_1, shp_1b[f_grid_num,])
  f_svf_3 <- mask(f_svf_2, shp_1b[f_grid_num,])
  
  plot(shp_1b[f_grid_num,])
  plot(f_svf_3, add = T)
  
  f_svf_4 <- getValues(f_svf_3)
  f_svf_4_mean <- mean(f_svf_4, na.rm = TRUE)
  return(f_svf_4_mean)
}

svf_mean <-  matrix(0, nrow = 185, ncol = 2) #to_be_Set
grid_orders <- 1:185 #to_be_Set
for(ii in grid_orders){
  svf_mean[ii,2] <- svf_mean_f(ii)
}
svf_mean[,1] <- grid_orders
colnames(svf_mean) <- c('NUMBER', 'XS_SVF')
write.csv(svf_mean, file = paste0('ppa_2301_k2/shp/3/ppa_2301_svf_bh_', bh_set, '_buf', buffer_set,'.csv'), row.names = FALSE)


