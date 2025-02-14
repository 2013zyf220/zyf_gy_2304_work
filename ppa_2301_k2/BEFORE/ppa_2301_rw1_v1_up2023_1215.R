library(rgdal)
library(raster)
library(sf)
library(sp)
library(dplyr)
library(tidyr)
library(landscapemetrics)
library(ggplot2)
#rm(list = ls())

setwd("E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/shp/rw")
rw_1 <- st_read("2301_cq_water_rw_7.shp")
rw_2 <- as.data.frame(rw_1)
rw_3 <- rw_2[,c("line_num","line_len")]
colnames(rw_3) <- c("NUMBER","XA_RIVERW")
rw_4 <- rw_3[order(rw_3$NUMBER), ]
write.csv(rw_4, "2301_cq_water_rw_7.csv")
