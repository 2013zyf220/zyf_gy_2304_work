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
shp_sub_set <- 4; #to_be_set_key
bh_set <- 4 #to_be_set_key

c_buf <- 'x050s5' #to_be_set
buf_1 <- 100 #to_be_set
shp_1 <- shapefile(paste0('ppa_2301_k2/shp/5/res1/2301_cq_water_b13_buf', c_buf, '_buf', buf_1,'.shp')) #to_be_set
shp_2 <- shp_1;

shp_sub <- list()
shp_sub[[1]] <- 1:48 #to_be_set
shp_sub[[2]] <- 49:71 #to_be_set
shp_sub[[3]] <- 72:97 #to_be_set
shp_sub[[4]] <- 98:142 #to_be_set
shp_sub[[5]] <- 143:185 #to_be_set

bh_1 <- list()
bh_1[[1]] <- raster(paste0('building_height_k2/CNBH10m_X107Y29_rep2.tif')); #to_be_set
bh_1[[2]] <- raster(paste0('chongqing_bh/cq_bh4.tif')); #to_be_set
bh_1[[3]] <- raster(paste0('ppa_2301_k2/raster/bh_sum_6.tif')); #to_be_set
bh_1[[4]] <- raster(paste0('ppa_2301_k2/raster/bh_sum_7.tif')); #to_be_set
bh_2 <- bh_1[[bh_set]]

for(ii in shp_sub[[shp_sub_set]]){
  cat('ii:', ii, '___')
  c_bh_3 <- crop(bh_2, shp_2[ii,])
  c_bh_4 <- mask(c_bh_3, shp_2[ii,])
  
  plot(c_bh_4)
  plot(shp_2[ii,], add = T)
  
  c_bh_5 <- svf(c_bh_4)
  writeRaster(c_bh_5, filename = paste0('ppa_2301_k2/shp/5/res1/svf/svf_bh_', bh_set, '_grid_', ii, '_', c_buf, '.tif'))  #to_be_set
}
