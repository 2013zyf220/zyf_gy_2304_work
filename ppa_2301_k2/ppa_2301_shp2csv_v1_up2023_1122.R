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

#==========================================================

shp2csv_f <- function(f_data_1, f_var, f_out){
  f_data_2 <- st_read(f_data_1)
  f_data_3 <- as.data.frame(f_data_2)
  f_data_4 <- f_data_3[, f_var]
  write.csv(f_data_4, file = f_out, row.names = FALSE)
  
  f_res_list <- list();
  f_res_list[[1]] <- f_data_2;
  f_res_list[[2]] <- f_data_3;
  f_res_list[[3]] <- f_data_4;
  return(f_res_list)
}


#==========================================================

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/shp/3');

order_1 <- 1; #to_be_set_key
buffer_1 <- 1000; #to_be_set_key

data_terrain_1 <- paste0('ppa_2301_terrain_1_buf', buffer_1, '.shp');
data_terrain_var <- c('NUMBER', 'XT_CEN_X', 'XT_CEN_Y', 'XT_DEM_MEA', 'XT_DEM_STD', 'XT_SLOPE_M', 'XT_ASPECT_', 'XT_DIS_CEN');
data_terrain_out <- paste0('ppa_2301_terrain_1_buf', buffer_1, '.csv');
data_ndvi_1 <- paste0('ppa_2301_ndvi_s', order_1,  '_buf', buffer_1, '.shp');
data_ndvi_var <- c('NUMBER', 'XN_CEN_X2', 'XN_CEN_Y2', 'XN_NDVI_ME');
data_ndvi_out <- paste0('ppa_2301_ndvi_s', order_1,  '_buf', buffer_1, '.csv'); 

#==========================================================

data_terrain_res <- shp2csv_f(data_terrain_1, data_terrain_var, data_terrain_out)
data_ndvi_res <- shp2csv_f(data_ndvi_1, data_ndvi_var, data_ndvi_out)

