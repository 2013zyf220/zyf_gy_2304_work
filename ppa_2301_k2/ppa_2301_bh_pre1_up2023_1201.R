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
bh_1 <- raster(paste0('chongqing_bh/cq_bh3.tif')); 


bh_1[is.na(bh_1)] <- 0

writeRaster(bh_1, filename = paste0('chongqing_bh/cq_bh4.tif'))


