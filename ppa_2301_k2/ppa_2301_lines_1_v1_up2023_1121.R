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

#=======================================================================

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/shp/3');
lines_1 <- st_read('ppa_2301_lines_1.shp');

lines_1_NUMBER <-lines_1$NUMBER;
lines_1_angle <- lines_1$XA_ANGLE_2;
lines_1_out <- data.frame(NUMBER = lines_1_NUMBER, XA_ANGLE_2 = lines_1_angle);
write.csv(lines_1_out, file = paste0('ppa_2301_lines_1.csv'), row.names = FALSE);
