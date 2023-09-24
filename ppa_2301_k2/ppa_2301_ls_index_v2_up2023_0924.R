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
grid_1 <- shapefile("2301_cq_water_09_a11.shp")
grid_2 <- spTransform(grid_1, "+init=epsg:4326")

setwd("E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/raster")
year = 2021
luse <- raster(paste0('ppa_2301_cq_luse_',year,'.tif')) #land cover data

setwd("E:/zyf_gy/zyf_gy_2304_work/ppa_2301_k2/outputs")
plot(luse)
plot(grid_2, add=T)

len_grid <- 5 #to_be_set
#len_grid <- length(grid_2) #to_be_set


ls_metric <- data.frame()
for (ii in 1:len_grid) {
  c_grid_id <- as.integer(ii);
  c_grid_x <- grid_2@polygons[[ii]]@labpt[1];
  c_grid_y <- grid_2@polygons[[ii]]@labpt[2];
  
  c_lss_0 <- sample_lsm(luse, grid_2[ii,], what = c("lsm_c_pland","lsm_c_cohesion"), shape = "square"); #to_be_set
  c_lsc_0 <- sample_lsm(luse, grid_2[ii,], what = c("lsm_c_pland"), shape = "circle"); #to_be_set
  
  c_class_lss <- data.frame(class = rep(c(1:8),2), metric=c(rep("pland",8),rep("cohesion",8))) #to_be_set
  c_lss <- left_join(c_class_lss, c_lss_0)
  c_lss$value <- ifelse(is.na(c_lss$value), 0, c_lss$value)
  
  c_class_lsc <- data.frame(class = rep(c(1:8),1), metric=c(rep("pland",8))) #to_be_set
  c_lsc <- left_join(c_class_lsc, c_lsc_0)
  c_lsc$value <- ifelse(is.na(c_lsc$value), 0, c_lsc$value)
  
  c_pland_s <- c_lss %>% filter(metric == "pland")
  c_pland_s$value <- as.numeric(round(c_pland_s$value,2))
  c_pland_s_imp <- c_pland_s$value[c_pland_s$class == 8]
  
  c_cohesion <- c_lss %>% filter(metric == "cohesion")
  c_cohesion$value <- as.numeric(round(c_cohesion$value,2))
  c_cohesion_imp <- c_cohesion$value[c_cohesion$class == 8]
  
  c_pland_c <- c_lsc %>% filter(metric == "pland")
  c_pland_c$value <- as.numeric(round(c_pland_c$value,2))
  c_pland_c_imp <- c_pland_c$value[c_pland_c$class == 8]
  
  c_df <- data.frame(grid_id = c_grid_id, grid_x = c_grid_x, grid_y = c_grid_y, pland_s_imp = c_pland_s_imp, cohesion_imp = c_cohesion_imp, pland_c_imp = c_pland_c_imp) 
  
  ls_metric <- rbind(ls_metric,c_df)
}

write.csv(ls_metric,paste0("ls_",year,".csv"),row.names = FALSE)

