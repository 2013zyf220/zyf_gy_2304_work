
library(rgdal)
library(raster)
library(sf)
library(sp)
library(dplyr)
library(tidyr)
library(landscapemetrics)
library(ggplot2)
#rm(list = ls())
#list_lsm()
#============================================================================
#input and display data

year = 2021 #to_be_set

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/raster')
luse <- raster(paste0('ppa_2301_cq_luseb_',year,'.tif')) #land cover data

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/shp')
grid_2 <- st_read(paste0('2301_river_3_',year,'.shp'))

plot(luse)
plot(grid_2, add = T)

ii <- 1 #to_be_set
ls_0 <- sample_lsm(luse, grid_2[ii,], what = c('lsm_c_pland'), shape = 'square'); #to_be_set
class_ls <- data.frame(class = rep(c(1:8),1), metric = c(rep('pland',8))) #to_be_set
ls <- left_join(class_ls, ls_0)
ls$value <- ifelse(is.na(ls$value), 0, ls$value)