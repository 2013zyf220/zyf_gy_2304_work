library(raster)
library(terra)
library(sf)
library(landscapemetrics)
#library(MASS)
#library(skimr)
#library(DataExplorer)
#library(tidyverse)
#library(caret)
#library(pROC)
#library(ggplot2)
#library(readxl)

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2302_k2/ARCGIS')

#=================================================================
#up2024_0604_23:04

luse_1p <- raster('LUSE/luse_1p.tif')
shp_1 <- shapefile(paste0('BUF1/ppa_2302_buf2.shp'))

shp_1p <- spTransform(shp_1, crs(luse_1p))
luse_2 <- crop(luse_1p, shp_1p)
luse_3 <- mask(luse_2, shp_1p)

#=================================================================
#up2024_0604_23:04

buf_set <- 20 #to_be_set
buf_1 <- shapefile(paste0('LINES/streets_5_buf', buf_set, '.shp'))
buf_1p <- spTransform(buf_1, crs(luse_1p))
buf_2 <- buf_1p[1:300,]

plot(luse_3)
plot(buf_2, add = T)

#===================================
#up2024_0604_20:30

ls_index_f <- function(f_luse, f_buf_set){
  f_shp_0 <- paste0('LINES/streets_5_buf', f_buf_set, '.shp')
  f_grid_1 <- shapefile(f_shp_0)[1:300,]
  f_grid_2 <- st_read(f_shp_0)[1:300,]
  f_len_grid_1 <- length(f_grid_1) #to_be_set
  
  f_grid_2$XL_grid_x <- rep(0, f_len_grid_1)
  f_grid_2$XL_grid_y <- rep(0, f_len_grid_1)
  f_grid_2$XL_ps_imp <- rep(0, f_len_grid_1)
  f_grid_2$XL_pc_imp <- rep(0, f_len_grid_1)
  f_grid_2$XL_co_imp <- rep(0, f_len_grid_1)
  f_grid_2$XL_shdi <- rep(0, f_len_grid_1)
  f_ls_metric <- data.frame()
  
  for (ii in 1: f_len_grid_1) {
    cat(ii)
    fc_grid_id <- as.integer(ii);
    fc_grid_x <- f_grid_1@polygons[[ii]]@labpt[1];
    fc_grid_y <- f_grid_1@polygons[[ii]]@labpt[2];
    
    fc_lss_0 <- sample_lsm(f_luse, f_grid_1[ii,], what = c('lsm_c_pland','lsm_c_cohesion', 'lsm_c_ai','lsm_c_shape_mn','lsm_c_enn_mn','lsm_c_ed','lsm_c_lsi','lsm_c_area_mn','lsm_l_lsi','lsm_l_pd','lsm_l_shdi','lsm_l_frac_mn'), shape = 'square'); #to_be_set
    fc_lsc_0 <- sample_lsm(f_luse, f_grid_1[ii,], what = c('lsm_c_pland'), shape = 'circle'); #to_be_set
    
    fc_class_lss <- data.frame(class = rep(c(1:8),8), metric=c(rep('pland',8),rep('cohesion',8), rep('ai',8), rep('shape_mn',8), rep('enn_mn',8), rep('ed',8), rep('lsi',8), rep('area_mn',8))) #to_be_set
    fc_lss <- left_join(fc_class_lss, fc_lss_0)
    fc_lss$value <- ifelse(is.na(fc_lss$value), 0, fc_lss$value)
    
    fc_class_lsc <- data.frame(class = rep(c(1:8),1), metric=c(rep('pland',8))) #to_be_set
    fc_lsc <- left_join(fc_class_lsc, fc_lsc_0)
    fc_lsc$value <- ifelse(is.na(fc_lsc$value), 0, fc_lsc$value)
    
    fc_pland_s <- fc_lss %>% filter(metric == 'pland')
    fc_pland_s$value <- as.numeric(round(fc_pland_s$value, 2))
    fc_pland_s_imp <- fc_pland_s$value[fc_pland_s$class == 7]
    
    fc_cohesion <- fc_lss %>% filter(metric == 'cohesion') #patch cohesion index
    fc_cohesion$value <- as.numeric(round(fc_cohesion$value,2))
    fc_cohesion_imp <- fc_cohesion$value[fc_cohesion$class == 7]
    
    fc_shdi <- fc_lss_0 %>% filter(metric == 'shdi') #patch density
    fc_shdi$value <- as.numeric(round(fc_shdi$value,2))
    fc_shdi_v <- fc_shdi$value
    
    fc_pland_c <- fc_lsc %>% filter(metric == 'pland')
    fc_pland_c$value <- as.numeric(round(fc_pland_c$value,2))
    fc_pland_c_imp <- fc_pland_c$value[fc_pland_c$class == 7]
    
    fc_df <- data.frame(NUMBER = fc_grid_id, XL_grid_x = fc_grid_x, XL_grid_y = fc_grid_y,
                        XL_ps_imp = fc_pland_s_imp, XL_co_imp = fc_cohesion_imp, XL_pc_imp = fc_pland_c_imp, 
                        XL_shdi = fc_shdi_v) 
    
    f_ls_metric <- rbind(f_ls_metric,fc_df)
    
    f_grid_2$XL_grid_id[ii] <- fc_grid_id
    f_grid_2$XL_grid_x[ii] <- fc_grid_x
    f_grid_2$XL_grid_y[ii] <- fc_grid_y
    f_grid_2$XL_ps_imp[ii] <- fc_pland_s_imp
    f_grid_2$XL_pc_imp[ii] <- fc_pland_c_imp
    f_grid_2$XL_co_imp[ii] <- fc_cohesion_imp
    f_grid_2$XL_shdi[ii] <- fc_shdi_v
  }
  f_ls_metric_2 <- round(f_ls_metric, 3) 
  write.csv(f_ls_metric_2,paste0('LUSE/ppa_2302_lsi_buf', f_buf_set, '.csv'), row.names = FALSE)
  
  f_res_list <- list()
  f_res_list[['ls_metric']] <- f_ls_metric_2
  return(f_res_list)
}

#=============================================================
#up2024_0604_20:30

lsi_1 <- ls_index_f(luse_3, 20)
