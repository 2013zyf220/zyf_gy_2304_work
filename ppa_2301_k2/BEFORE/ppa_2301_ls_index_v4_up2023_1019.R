library(rgdal)
library(raster)
library(sf)
library(sp)
library(dplyr)
library(tidyr)
library(landscapemetrics)
library(ggplot2)
#rm(list = ls())
#list_lsm() #show lsm indexes that can be calculated in 'landscapemetrics'

#============================================================================

ls_index_f <- function(f_year){
  
  setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/shp/outputs')
  f_grid_1a <- shapefile(paste0('2301_river_3_', f_year,'.shp'))
  f_grid_1b <- spTransform(f_grid_1a, '+init=epsg:4326')
  f_grid_2 <- st_read(paste0('2301_river_3_', f_year,'.shp'))
  
  setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/raster')
  f_luse <- raster(paste0('ppa_2301_cq_luseb_', f_year,'.tif')) #land cover data
  
  setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/shp/outputs')
  plot(f_luse)
  plot(f_grid_1b, add = T)
  
  plot(f_luse)
  plot(f_grid_2, add = T)  
  
  f_len_grid_1 <- length(f_grid_1b) #to_be_set
  f_len_grid_2 <- length(f_grid_1b) #to_be_set
  
  f_grid_2$rx_grid_x <- rep(0, f_len_grid_2)
  f_grid_2$rx_grid_y <- rep(0, f_len_grid_2)
  f_grid_2$rx_ps_imp <- rep(0, f_len_grid_2)
  f_grid_2$rx_pc_imp <- rep(0, f_len_grid_2)
  f_grid_2$rx_co_imp <- rep(0, f_len_grid_2)
  f_grid_2$rx_ai_imp <- rep(0, f_len_grid_2)
  f_grid_2$rx_ps_gre <- rep(0, f_len_grid_2)
  f_grid_2$rx_pc_gre <- rep(0, f_len_grid_2)
  f_grid_2$rx_co_gre <- rep(0, f_len_grid_2)
  f_grid_2$rx_ai_gre <- rep(0, f_len_grid_2)
  f_grid_2$rx_ps_wat <- rep(0, f_len_grid_2)
  f_grid_2$rx_pc_wat <- rep(0, f_len_grid_2)
  f_grid_2$rx_co_wat <- rep(0, f_len_grid_2)
  f_grid_2$rx_ai_wat <- rep(0, f_len_grid_2)
  f_grid_2$rx_pd <- rep(0, f_len_grid_2)
  f_grid_2$rx_lsi <- rep(0, f_len_grid_2)
  
  f_ls_metric <- data.frame()
  
  for (ii in 1: f_len_grid_1) {
    cat("Year_",f_year,'--Gird_', ii)
    fc_grid_id <- as.integer(ii);
    fc_grid_x <- f_grid_1b@polygons[[ii]]@labpt[1];
    fc_grid_y <- f_grid_1b@polygons[[ii]]@labpt[2];
    
    fc_lss_0 <- sample_lsm(f_luse, f_grid_2[ii,], what = c('lsm_c_pland','lsm_c_cohesion', 'lsm_c_ai','lsm_l_pd','lsm_l_lsi'), shape = 'square'); #to_be_set
    fc_lsc_0 <- sample_lsm(f_luse, f_grid_2[ii,], what = c('lsm_c_pland'), shape = 'circle'); #to_be_set
    
    fc_class_lss <- data.frame(class = rep(c(1:8),3), metric=c(rep('pland',8),rep('cohesion',8), rep('ai',8))) #to_be_set
    fc_lss <- left_join(fc_class_lss, fc_lss_0)
    fc_lss$value <- ifelse(is.na(fc_lss$value), 0, fc_lss$value)
    
    fc_class_lsc <- data.frame(class = rep(c(1:8),1), metric=c(rep('pland',8))) #to_be_set
    fc_lsc <- left_join(fc_class_lsc, fc_lsc_0)
    fc_lsc$value <- ifelse(is.na(fc_lsc$value), 0, fc_lsc$value)
    
    fc_pland_s <- fc_lss %>% filter(metric == 'pland')
    fc_pland_s$value <- as.numeric(round(fc_pland_s$value, 2))
    fc_pland_s_gre <- fc_pland_s$value[fc_pland_s$class == 1]
    fc_pland_s_wat <- fc_pland_s$value[fc_pland_s$class == 5]
    fc_pland_s_imp <- fc_pland_s$value[fc_pland_s$class == 8]
    
    fc_cohesion <- fc_lss %>% filter(metric == 'cohesion') #patch cohesion index
    fc_cohesion$value <- as.numeric(round(fc_cohesion$value,2))
    fc_cohesion_gre <- fc_cohesion$value[fc_cohesion$class == 1]
    fc_cohesion_wat <- fc_cohesion$value[fc_cohesion$class == 5]
    fc_cohesion_imp <- fc_cohesion$value[fc_cohesion$class == 8]
    
    fc_ai <- fc_lss %>% filter(metric == 'ai') #aggregation index
    fc_ai$value <- as.numeric(round(fc_ai$value,2))
    fc_ai_gre <- fc_ai$value[fc_ai$class == 1]
    fc_ai_wat <- fc_ai$value[fc_ai$class == 5]
    fc_ai_imp <- fc_ai$value[fc_ai$class == 8]
    
    fc_pd <- fc_lss_0 %>% filter(metric == 'pd') #patch density
    fc_pd$value <- as.numeric(round(fc_pd$value,2))
    fc_pd_v <- fc_pd$value
    
    fc_lsi <- fc_lss_0 %>% filter(metric == 'lsi') #patch density
    fc_lsi$value <- as.numeric(round(fc_lsi$value,2))
    fc_lsi_v <- fc_lsi$value

    fc_pland_c <- fc_lsc %>% filter(metric == 'pland')
    fc_pland_c$value <- as.numeric(round(fc_pland_c$value,2))
    fc_pland_c_gre <- fc_pland_c$value[fc_pland_c$class == 1]
    fc_pland_c_wat <- fc_pland_c$value[fc_pland_c$class == 5]
    fc_pland_c_imp <- fc_pland_c$value[fc_pland_c$class == 8]
    
    fc_df <- data.frame(rx_grid_id = fc_grid_id, rx_grid_x = fc_grid_x, rx_grid_y = fc_grid_y, 
                       rx_pland_s_imp = fc_pland_s_imp, rx_cohesion_imp = fc_cohesion_imp, rx_pland_c_imp = fc_pland_c_imp, 
                       rx_ai_imp = fc_ai_imp, 
                       rx_pland_s_gre = fc_pland_s_gre, rx_cohesion_gre = fc_cohesion_gre, rx_pland_c_gre = fc_pland_c_gre, 
                       rx_ai_gre = fc_ai_gre, 
                       rx_pland_s_wat = fc_pland_s_wat, rx_cohesion_wat = fc_cohesion_wat, rx_pland_c_wat = fc_pland_c_wat, 
                       rx_ai_wat = fc_ai_wat, rx_pd = fc_pd_v,  rx_lsi = fc_lsi_v) 
    
    f_ls_metric <- rbind(f_ls_metric,fc_df)
  
    f_grid_2$rx_grid_id[ii] <- fc_grid_id
    f_grid_2$rx_grid_x[ii] <- fc_grid_x
    f_grid_2$rx_grid_y[ii] <- fc_grid_y
    f_grid_2$rx_ps_imp[ii] <- fc_pland_s_imp
    f_grid_2$rx_pc_imp[ii] <- fc_pland_c_imp
    f_grid_2$rx_co_imp[ii] <- fc_cohesion_imp
    f_grid_2$rx_ai_imp[ii] <- fc_ai_imp
    f_grid_2$rx_ps_gre[ii] <- fc_pland_s_gre
    f_grid_2$rx_pc_gre[ii] <- fc_pland_c_gre
    f_grid_2$rx_co_gre[ii] <- fc_cohesion_gre
    f_grid_2$rx_ai_gre[ii] <- fc_ai_gre
    f_grid_2$rx_ps_wat[ii] <- fc_pland_s_wat
    f_grid_2$rx_pc_wat[ii] <- fc_pland_c_wat
    f_grid_2$rx_co_wat[ii] <- fc_cohesion_wat
    f_grid_2$rx_ai_wat[ii] <- fc_ai_wat
    f_grid_2$rx_pd[ii] <- fc_pd_v
    f_grid_2$rx_lsi[ii] <- fc_lsi_v
  }
  
  st_write(f_grid_2, paste0('2301_river_4_', f_year,'.shp'))
  write.csv(f_ls_metric,paste0('2301_ls_', f_year,'.csv'),row.names = FALSE)
  
}

#=========================================================================

year_s <- 2021; #to_be_set
year_e <- 2021; #to_be_set

for(c_year in year_s: year_e){
  ls_index_f(c_year)
}