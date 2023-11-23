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

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2')

year_1 <- 2021; #to_be_set
buffer_1 <- 1000; #to_be_set

if(buffer_1 == 200){
  shp_0 <- 'shp/2/2301_cq_water_b12_buf200_a01.shp'
}else if(buffer_1 == 400){
  shp_0 <- 'shp/2/2301_cq_water_b12_buf400_a01.shp'
}else if(buffer_1 == 500){
  shp_0 <- 'shp/2/2301_cq_water_b12_buf500_a01.shp'
}else if(buffer_1 == 600){
  shp_0 <- 'shp/2/2301_cq_water_b12_buf600_a01.shp'
}else if(buffer_1 == 800){
  shp_0 <- 'shp/2/2301_cq_water_b12_buf800_a01.shp'
}else if(buffer_1 == 1000){
  shp_0 <- 'shp/2/2301_cq_water_b12_buf1000_a01.shp'
}else{
  cat('ERROR')
}

#============================================================================

ls_index_f <- function(f_year){
  
  f_grid_1a <- shapefile(shp_0)
  f_grid_1b <- spTransform(f_grid_1a, '+init=epsg:4326')
  f_grid_2 <- st_read(shp_0)
  f_luse <- raster(paste0('raster/ppa_2301_cq_luseb_', f_year,'.tif')) #land cover data
  
  setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/shp/3')
  plot(f_luse)
  plot(f_grid_1b, add = T)
  
  plot(f_luse)
  plot(f_grid_2, add = T)  
  
  f_len_grid_1 <- length(f_grid_1b) #to_be_set
  f_len_grid_2 <- length(f_grid_1b) #to_be_set
  
  f_grid_2$XL_grid_x <- rep(0, f_len_grid_2)
  f_grid_2$XL_grid_y <- rep(0, f_len_grid_2)
  f_grid_2$XL_ps_imp <- rep(0, f_len_grid_2)
  f_grid_2$XL_pc_imp <- rep(0, f_len_grid_2)
  f_grid_2$XL_co_imp <- rep(0, f_len_grid_2)
  f_grid_2$XL_ai_imp <- rep(0, f_len_grid_2)
  f_grid_2$XL_ps_gre <- rep(0, f_len_grid_2)
  f_grid_2$XL_pc_gre <- rep(0, f_len_grid_2)
  f_grid_2$XL_co_gre <- rep(0, f_len_grid_2)
  f_grid_2$XL_ai_gre <- rep(0, f_len_grid_2)
  f_grid_2$XL_ps_wat <- rep(0, f_len_grid_2)
  f_grid_2$XL_pc_wat <- rep(0, f_len_grid_2)
  f_grid_2$XL_co_wat <- rep(0, f_len_grid_2)
  f_grid_2$XL_ai_wat <- rep(0, f_len_grid_2)
  f_grid_2$XL_pd <- rep(0, f_len_grid_2)
  f_grid_2$XL_lsi <- rep(0, f_len_grid_2)
  
  f_ls_metric <- data.frame()
  
  for (ii in 1: f_len_grid_1) {
    cat("Year_",f_year,'--Gird_', ii)
    fc_grid_id <- as.integer(ii);
    fc_grid_x <- f_grid_1b@polygons[[ii]]@labpt[1];
    fc_grid_y <- f_grid_1b@polygons[[ii]]@labpt[2];
    
    fc_lss_0 <- sample_lsm(f_luse, f_grid_1b[ii,], what = c('lsm_c_pland','lsm_c_cohesion', 'lsm_c_ai','lsm_l_pd','lsm_l_lsi'), shape = 'square'); #to_be_set
    fc_lsc_0 <- sample_lsm(f_luse, f_grid_1b[ii,], what = c('lsm_c_pland'), shape = 'circle'); #to_be_set
    
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
    
    fc_df <- data.frame(NUMBER = fc_grid_id, XL_grid_x = fc_grid_x, XL_grid_y = fc_grid_y, 
                       XL_ps_imp = fc_pland_s_imp, XL_co_imp = fc_cohesion_imp, XL_pc_imp = fc_pland_c_imp, 
                       XL_ai_imp = fc_ai_imp, 
                       XL_ps_gre = fc_pland_s_gre, XL_co_gre = fc_cohesion_gre, XL_pc_gre = fc_pland_c_gre, 
                       XL_ai_gre = fc_ai_gre, 
                       XL_ps_wat = fc_pland_s_wat, XL_co_wat = fc_cohesion_wat, XL_pc_wat = fc_pland_c_wat, 
                       XL_ai_wat = fc_ai_wat, XL_pd = fc_pd_v,  XL_lsi = fc_lsi_v) 
    
    f_ls_metric <- rbind(f_ls_metric,fc_df)
  
    f_grid_2$XL_grid_id[ii] <- fc_grid_id
    f_grid_2$XL_grid_x[ii] <- fc_grid_x
    f_grid_2$XL_grid_y[ii] <- fc_grid_y
    f_grid_2$XL_ps_imp[ii] <- fc_pland_s_imp
    f_grid_2$XL_pc_imp[ii] <- fc_pland_c_imp
    f_grid_2$XL_co_imp[ii] <- fc_cohesion_imp
    f_grid_2$XL_ai_imp[ii] <- fc_ai_imp
    f_grid_2$XL_ps_gre[ii] <- fc_pland_s_gre
    f_grid_2$XL_pc_gre[ii] <- fc_pland_c_gre
    f_grid_2$XL_co_gre[ii] <- fc_cohesion_gre
    f_grid_2$XL_ai_gre[ii] <- fc_ai_gre
    f_grid_2$XL_ps_wat[ii] <- fc_pland_s_wat
    f_grid_2$XL_pc_wat[ii] <- fc_pland_c_wat
    f_grid_2$XL_co_wat[ii] <- fc_cohesion_wat
    f_grid_2$XL_ai_wat[ii] <- fc_ai_wat
    f_grid_2$XL_pd[ii] <- fc_pd_v
    f_grid_2$XL_lsi[ii] <- fc_lsi_v
  }
  
  st_write(f_grid_2, paste0('ppa_2301_lsi_', f_year, '_buf', buffer_1, '.shp'))
  write.csv(f_ls_metric,paste0('ppa_2301_lsi_', f_year, '_buf', buffer_1, '.csv'),row.names = FALSE)
}

#=========================================================================

ls_index_f(year_1)
