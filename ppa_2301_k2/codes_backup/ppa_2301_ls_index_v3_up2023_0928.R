#up2023_0928 10:55

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

year = 2019 #to_be_set

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/shp/outputs')
grid_1a <- shapefile(paste0('2301_river_3_',year,'.shp'))
grid_1b <- spTransform(grid_1a, '+init=epsg:4326')
grid_2 <- st_read(paste0('2301_river_3_',year,'.shp'))

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/raster')
luse <- raster(paste0('ppa_2301_cq_luseb_',year,'.tif')) #land cover data

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/shp/outputs')
plot(luse)
plot(grid_1b, add = T)

plot(luse)
plot(grid_2, add = T)

#============================================================================
#calculate indexes

len_grid_1 <- length(grid_1b) #to_be_set
len_grid_2 <- length(grid_1b) #to_be_set

grid_2$rx_grid_x <- rep(0, len_grid_2)
grid_2$rx_grid_y <- rep(0, len_grid_2)
grid_2$rx_ps_imp <- rep(0, len_grid_2)
grid_2$rx_pc_imp <- rep(0, len_grid_2)
grid_2$rx_co_imp <- rep(0, len_grid_2)
grid_2$rx_ai_imp <- rep(0, len_grid_2)
grid_2$rx_pd_imp <- rep(0, len_grid_2)
grid_2$rx_lsi_imp <- rep(0, len_grid_2)
grid_2$rx_ps_gre <- rep(0, len_grid_2)
grid_2$rx_pc_gre <- rep(0, len_grid_2)
grid_2$rx_co_gre <- rep(0, len_grid_2)
grid_2$rx_ai_gre <- rep(0, len_grid_2)
grid_2$rx_pd_gre <- rep(0, len_grid_2)
grid_2$rx_lsi_gre <- rep(0, len_grid_2)
grid_2$rx_ps_wat <- rep(0, len_grid_2)
grid_2$rx_pc_wat <- rep(0, len_grid_2)
grid_2$rx_co_wat <- rep(0, len_grid_2)
grid_2$rx_ai_wat <- rep(0, len_grid_2)
grid_2$rx_pd_wat <- rep(0, len_grid_2)
grid_2$rx_lsi_wat <- rep(0, len_grid_2)

ls_metric <- data.frame()
for (ii in 1:len_grid_1) {
  cat("Gird_",ii)
  c_grid_id <- as.integer(ii);
  c_grid_x <- grid_1b@polygons[[ii]]@labpt[1];
  c_grid_y <- grid_1b@polygons[[ii]]@labpt[2];
  
  c_lss_0 <- sample_lsm(luse, grid_2[ii,], what = c('lsm_c_pland','lsm_c_cohesion'), shape = 'square'); #to_be_set
  c_lsc_0 <- sample_lsm(luse, grid_2[ii,], what = c('lsm_c_pland'), shape = 'circle'); #to_be_set
  
  c_class_lss <- data.frame(class = rep(c(1:8),5), metric=c(rep('pland',8),rep('cohesion',8), rep('ai',8), rep('pd',8), rep('lsi',8))) #to_be_set
  c_lss <- left_join(c_class_lss, c_lss_0)
  c_lss$value <- ifelse(is.na(c_lss$value), 0, c_lss$value)
  
  c_class_lsc <- data.frame(class = rep(c(1:8),1), metric=c(rep('pland',8))) #to_be_set
  c_lsc <- left_join(c_class_lsc, c_lsc_0)
  c_lsc$value <- ifelse(is.na(c_lsc$value), 0, c_lsc$value)
  
  c_pland_s <- c_lss %>% filter(metric == 'pland')
  c_pland_s$value <- as.numeric(round(c_pland_s$value,2))
  c_pland_s_gre <- c_pland_s$value[c_pland_s$class == 1]
  c_pland_s_wat <- c_pland_s$value[c_pland_s$class == 5]
  c_pland_s_imp <- c_pland_s$value[c_pland_s$class == 8]

  c_cohesion <- c_lss %>% filter(metric == 'cohesion') #patch cohesion index
  c_cohesion$value <- as.numeric(round(c_cohesion$value,2))
  c_cohesion_gre <- c_cohesion$value[c_cohesion$class == 1]
  c_cohesion_wat <- c_cohesion$value[c_cohesion$class == 5]
  c_cohesion_imp <- c_cohesion$value[c_cohesion$class == 8]
  
  c_ai <- c_lss %>% filter(metric == 'ai') #aggregation index
  c_ai$value <- as.numeric(round(c_ai$value,2))
  c_ai_gre <- c_ai$value[c_ai$class == 1]
  c_ai_wat <- c_ai$value[c_ai$class == 5]
  c_ai_imp <- c_ai$value[c_ai$class == 8]
  
  c_pd <- c_lss %>% filter(metric == 'pd') #patch density
  c_pd$value <- as.numeric(round(c_pd$value,2))
  c_pd_gre <- c_pd$value[c_pd$class == 1]
  c_pd_wat <- c_pd$value[c_pd$class == 5]
  c_pd_imp <- c_pd$value[c_pd$class == 8]
  
  c_lsi <- c_lss %>% filter(metric == 'lsi') #patch density
  c_lsi$value <- as.numeric(round(c_lsi$value,2))
  c_lsi_gre <- c_lsi$value[c_lsi$class == 1]
  c_lsi_wat <- c_lsi$value[c_lsi$class == 5]
  c_lsi_imp <- c_lsi$value[c_lsi$class == 8]
  
  c_pland_c <- c_lsc %>% filter(metric == 'pland')
  c_pland_c$value <- as.numeric(round(c_pland_c$value,2))
  c_pland_c_gre <- c_pland_c$value[c_pland_c$class == 1]
  c_pland_c_wat <- c_pland_c$value[c_pland_c$class == 5]
  c_pland_c_imp <- c_pland_c$value[c_pland_c$class == 8]
  
  c_df <- data.frame(rx_grid_id = c_grid_id, rx_grid_x = c_grid_x, rx_grid_y = c_grid_y, 
                     rx_pland_s_imp = c_pland_s_imp, rx_cohesion_imp = c_cohesion_imp, rx_pland_c_imp = c_pland_c_imp, 
                     rx_ai_imp = c_ai_imp, rx_pd_imp = c_pd_imp,  rx_lsi_imp = c_lsi_imp,
                     rx_pland_s_gre = c_pland_s_gre, rx_cohesion_gre = c_cohesion_gre, rx_pland_c_gre = c_pland_c_gre, 
                     rx_ai_gre = c_ai_gre, rx_pd_gre = c_pd_gre,  rx_lsi_gre = c_lsi_gre,
                     rx_pland_s_wat = c_pland_s_wat, rx_cohesion_wat = c_cohesion_wat, rx_pland_c_wat = c_pland_c_wat, 
                     rx_ai_wat = c_ai_wat, rx_pd_wat = c_pd_wat,  rx_lsi_wat = c_lsi_wat
                     ) 
  ls_metric <- rbind(ls_metric,c_df)
  
  grid_2$rx_grid_id[ii] <- c_grid_id
  grid_2$rx_grid_x[ii] <- c_grid_x
  grid_2$rx_grid_y[ii] <- c_grid_y
  grid_2$rx_ps_imp[ii] <- c_pland_s_imp
  grid_2$rx_pc_imp[ii] <- c_pland_c_imp
  grid_2$rx_co_imp[ii] <- c_cohesion_imp
  grid_2$rx_ai_imp[ii] <- c_ai_imp
  grid_2$rx_pd_imp[ii] <- c_pd_imp
  grid_2$rx_lsi_imp[ii] <- c_lsi_imp
  grid_2$rx_ps_gre[ii] <- c_pland_s_gre
  grid_2$rx_pc_gre[ii] <- c_pland_c_gre
  grid_2$rx_co_gre[ii] <- c_cohesion_gre
  grid_2$rx_ai_gre[ii] <- c_ai_gre
  grid_2$rx_pd_gre[ii] <- c_pd_gre
  grid_2$rx_lsi_gre[ii] <- c_lsi_gre
  grid_2$rx_ps_wat[ii] <- c_pland_s_wat
  grid_2$rx_pc_wat[ii] <- c_pland_c_wat
  grid_2$rx_co_wat[ii] <- c_cohesion_wat
  grid_2$rx_ai_wat[ii] <- c_ai_wat
  grid_2$rx_pd_wat[ii] <- c_pd_wat
  grid_2$rx_lsi_wat[ii] <- c_lsi_wat
}

#============================================================================
#export shp file

st_write(grid_2, paste0('2301_river_4_',year,'.shp'))
write.csv(ls_metric,paste0('2301_ls_',year,'.csv'),row.names = FALSE)

