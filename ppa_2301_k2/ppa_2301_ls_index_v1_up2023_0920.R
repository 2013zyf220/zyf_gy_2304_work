#up2023_0920 17:08

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

setwd("E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2")
year = 2021
luse <- raster(paste0('ppa_2301_cq_luse_',year,'.tif')) #land cover data


plot(luse)
plot(grid_2, add=T)


ls_metric <- data.frame()
for (ii in 1:length(grid_2)) {
  t_grid_id <- as.integer(ii)
  t_grid_x <- grid_2@polygons[[ii]]@labpt[1]
  t_grid_y <- grid_2@polygons[[ii]]@labpt[2]
  
  t_ls_0 <- sample_lsm(luse, grid_2[ii,],
                     what = c("lsm_c_pland"),
                     shape = "circle")
  
  t_class_ls <- data.frame(class = c(1:8)) #the number of land cover types
  t_ls <- left_join(t_class_ls, t_ls_0)
  t_ls$value <- ifelse(is.na(t_ls$value), 0, t_ls$value)
  t_ls$value <- as.numeric(round(t_ls$value,2))
  
  t_pcrop_ls <- t_ls$value[t_ls$class==1]
  t_pfor_ls <- t_ls$value[t_ls$class==2]
  t_pimp_ls <- t_ls$value[t_ls$class==8]
  
  t_df <- data.frame(grid_id = t_grid_id,grid_x = t_grid_x,grid_y = t_grid_y,ls_pcrop = t_pcrop_ls,ls_pfor = t_pfor_ls,ls_pimp = t_pimp_ls) 
  ls_metric <- rbind(ls_metric,t_df)
}

write.csv(ls_metric,paste0("ls_",year,".csv"),row.names = FALSE)


#============================================================================
#mean patch area

fg_metric <- data.frame()
for (ii in 1:length(grid_2)) {
  t_grid_id <- as.integer(ii)
  t_grid_x <- grid_2@polygons[[ii]]@labpt[1]
  t_grid_y <- grid_2@polygons[[ii]]@labpt[2]
  
  t_fg_0 <- sample_lsm(luse, grid_2[ii,],
                     what = c("lsm_c_area_mn","lsm_c_division","lsm_c_ai"),
                     shape = "square")
  
  
  t_class_fg <- data.frame(class = rep(c(1:8),3), metric=c(rep("area_mn",8),rep("ai",8),rep("division",8)))
  t_fg <- left_join(t_class_fg, t_fg_0)
  t_fg$value <- ifelse(is.na(t_fg$value), 0, t_fg$value)
  t_fg$value <- as.numeric(round(t_fg$value,2))
  
  t_area_mn <- t_fg %>% filter(metric=="area_mn")       
  t_imp_area <- t_area_mn$value[t_area_mn$class==8]
  t_crop_area <- t_area_mn$value[t_area_mn$class==1]
  
  t_division <- t_fg %>% filter(metric=="division")
  t_crop_division <- t_division$value[t_division$class==1]
  t_imp_division <- t_division$value[t_division$class==8]
  
  t_ai <- t_fg %>% filter(metric=="ai")
  t_imp_ai <- t_ai$value[t_ai$class==8]
  
  t_df <- data.frame(grid_id = t_grid_id, grid_x = t_grid_x, grid_y = t_grid_y, imp_area = t_imp_area, 
                   imp_division = t_imp_division, imp_ai = t_imp_ai)
  
  fg_metric <- rbind(fg_metric,t_df)
}

write.csv(fg_metric,paste0("fg_",year,".csv"),row.names = FALSE)



#============================================================================


rst_metric <- data.frame()
for (ii in 1:length(grid_2)) {
  
  t_grid_id <- as.integer(ii)
  t_grid_x <- grid_2@polygons[[ii]]@labpt[1]
  t_grid_y <- grid_2@polygons[[ii]]@labpt[2]
  
  t_rst_0 = sample_lsm(luse, grid_2[ii,], what = c("lsm_c_pland","lsm_c_cohesion"),
                     shape = "square") 
  
  t_class_rst <- data.frame(class = rep(c(1:8),2), metric=c(rep("pland",8),rep("cohesion",8)))
  t_rst <- left_join(t_class_rst, t_rst_0)
  t_rst$value <- ifelse(is.na(t_rst$value), 0, t_rst$value)
  
  t_pland <- t_rst %>% filter(metric=="pland")
  t_pland$value <- as.numeric(round(t_pland$value,2))
  t_pimp_rst <- t_pland$value[t_pland$class==8]
  
  t_cohesion <- t_rst %>% filter(metric=="cohesion")
  t_cohesion$value <- as.numeric(round(t_cohesion$value,2))
  t_cohesion_imp <- t_cohesion$value[t_cohesion$class==8]
  
  t_df <- data.frame(grid_id = t_grid_id, grid_x = t_grid_x, grid_y = t_grid_y, rst_pimp = t_pimp_rst, rst_cohesion_imp = t_cohesion_imp) 
  
  rst_metric <- rbind(rst_metric,t_df)
}

write.csv(rst_metric,paste0("rst_",year,".csv"),row.names = FALSE)

