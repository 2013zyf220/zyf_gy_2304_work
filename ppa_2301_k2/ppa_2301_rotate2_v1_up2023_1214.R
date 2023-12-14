library(rgdal)
library(raster)
library(sf)
library(sp)
library(dplyr)
library(tidyr)
library(landscapemetrics)
library(ggplot2)
#rm(list = ls())

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/shp/rotate')

#==================================

shp_1 <- st_read('2301_cq_water_rotate_4.shp')
shp_2 <- as.data.frame(shp_1)
shp_3 <- shp_2[,c("NUMBER","XA_ANGLE_2","NUMBER2")]
write.csv(shp_3, '2301_cq_water_rotate_4b.csv')
#==================================

angled_f1 <- function(ii){
  f_data_1 <- shp_3[shp_3$NUMBER == ii, "XA_ANGLE_2"]
  f_data_2a <- shp_3[shp_3$NUMBER == ii + 1, c("XA_ANGLE_2", "NUMBER2")]
  f_data_2b <- f_data_2a[f_data_2a$NUMBER2 == 1, "XA_ANGLE_2"]
  f_data_3 <- f_data_2b - f_data_1
  return(f_data_3)
}

angled_f2 <- function(ii){
  f_data_1 <- shp_3[shp_3$NUMBER == ii, c("XA_ANGLE_2","NUMBER2")]
  f_data_2 <- f_data_1[f_data_1$NUMBER2 == 1, "XA_ANGLE_2"]
  f_data_3 <- f_data_1[f_data_1$NUMBER2 == 2, "XA_ANGLE_2"]
  f_data_4 <- f_data_3 - f_data_2
  return(f_data_4)
}

angled_f3 <- function(ii){
  f_data_1 <- shp_3[shp_3$NUMBER == ii, "XA_ANGLE_2"]
  f_data_2 <- shp_3[shp_3$NUMBER == ii + 1, "XA_ANGLE_2"]
  f_data_3 <- f_data_2 - f_data_1
  return(f_data_3)
}

#==================================

angle_dif <- rep(0,185) #to_be_set
angle_sub0 <- c(1:185) #to_be_set
angle_sub1 <- c(47, 96, 141, 184) #to_be_set
angle_sub2 <- c(48, 97, 142, 185) #to_be_set
for(ii in angle_sub0){
  if (ii %in% angle_sub1){
    angle_dif[ii] <- angled_f1(ii)
  }
  else if(ii %in% angle_sub2){
    angle_dif[ii] <- angled_f2(ii)
  }
  else{
    angle_dif[ii] <- angled_f3(ii)
  }
}

write.csv(angle_dif, '2301_cq_water_rotate_4c.csv')
