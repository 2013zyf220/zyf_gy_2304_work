#up2023_0924 11:24

library(rgdal)
library(raster)
library(sf)
library(sp)
library(dplyr)
library(tidyr)
library(landscapemetrics)
library(ggplot2)
#rm(list = ls())


#============================================================================
setwd("E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/shp")
grid_1 <- shapefile("2301_cq_water_10_a2.shp")
grid_2 <- spTransform(grid_1, "+init=epsg:4326")

setwd("E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/raster")
year = 2021
luse <- raster(paste0('ppa_2301_cq_luse_',year,'.tif')) #land cover data

setwd("E:/zyf_gy/zyf_gy_2304_work/ppa_2301_k2/outputs")
plot(luse)
plot(grid_2, add=T)


grid_num = 1 #to_be_set
plot(luse)
plot(grid_2[grid_num,],add = T);


