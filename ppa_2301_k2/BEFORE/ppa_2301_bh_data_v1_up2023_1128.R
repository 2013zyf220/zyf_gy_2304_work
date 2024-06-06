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
bh_1 <- raster(paste0('building_height_k2/CNBH10m_X107Y29_rep.tif')); 
cq_area <- shapefile(paste0('ppa_2301_k2/shp/outputs/ppa_2301_cq1_area.shp')); 
bh_2 <- crop(bh_1, cq_area)

bh_2[is.na(bh_2)] <- 0
plot(bh_2)
plot(cq_area, add = T)
writeRaster(bh_2, filename = paste0('building_height_k2/CNBH10m_X107Y29_rep2.tif'))


