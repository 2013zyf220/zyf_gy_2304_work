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

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2')

#============================================================================

ls_index_f <- function(f_year, f_buffer){
  f_shp_0 <- paste0('shp/2/2301_cq_water_b12_buf', f_buffer, '_a01.shp')
  f_grid_1a <- shapefile(f_shp_0)
  f_grid_1b <- spTransform(f_grid_1a, '+init=epsg:4326')
  f_grid_2 <- st_read(f_shp_0)
  f_luse <- raster(paste0('raster/ppa_2301_cq_luseb_', f_year,'.tif')) #land cover data
  
  plot(f_luse)
  plot(f_grid_1b, add = T)
  
  f_len_grid_1 <- length(f_grid_1b) #to_be_set
  f_len_grid_2 <- length(f_grid_1b) #to_be_set
  
  f_grid_2$XL_grid_x <- rep(0, f_len_grid_2)
  f_grid_2$XL_grid_y <- rep(0, f_len_grid_2)
  f_grid_2$XL_ps_imp <- rep(0, f_len_grid_2)
  f_grid_2$XL_pc_imp <- rep(0, f_len_grid_2)
  f_grid_2$XL_co_imp <- rep(0, f_len_grid_2)
  f_grid_2$XL_ai_imp <- rep(0, f_len_grid_2)
  f_grid_2$XL_shape_mn_imp <- rep(0, f_len_grid_2)
  f_grid_2$XL_enn_mn_imp <- rep(0, f_len_grid_2)
  f_grid_2$XL_ed_imp <- rep(0, f_len_grid_2)
  f_grid_2$XL_lsi_imp <- rep(0, f_len_grid_2)
  f_grid_2$XL_area_mn_imp <- rep(0, f_len_grid_2)
  f_grid_2$XL_ps_gre <- rep(0, f_len_grid_2)
  f_grid_2$XL_pc_gre <- rep(0, f_len_grid_2)
  f_grid_2$XL_co_gre <- rep(0, f_len_grid_2)
  f_grid_2$XL_ai_gre <- rep(0, f_len_grid_2)
  f_grid_2$XL_shape_mn_gre <- rep(0, f_len_grid_2)
  f_grid_2$XL_enn_mn_gre <- rep(0, f_len_grid_2)
  f_grid_2$XL_ed_gre <- rep(0, f_len_grid_2)
  f_grid_2$XL_lsi_gre <- rep(0, f_len_grid_2)
  f_grid_2$XL_area_mn_gre <- rep(0, f_len_grid_2)
  f_grid_2$XL_ps_wat <- rep(0, f_len_grid_2)
  f_grid_2$XL_pc_wat <- rep(0, f_len_grid_2)
  f_grid_2$XL_co_wat <- rep(0, f_len_grid_2)
  f_grid_2$XL_ai_wat <- rep(0, f_len_grid_2)
  f_grid_2$XL_shape_mn_wat <- rep(0, f_len_grid_2)
  f_grid_2$XL_enn_mn_wat <- rep(0, f_len_grid_2)
  f_grid_2$XL_ed_wat <- rep(0, f_len_grid_2)
  f_grid_2$XL_lsi_wat <- rep(0, f_len_grid_2)
  f_grid_2$XL_area_mn_wat <- rep(0, f_len_grid_2)
  f_grid_2$XL_pd <- rep(0, f_len_grid_2)
  f_grid_2$XL_lsi <- rep(0, f_len_grid_2)
  f_grid_2$XL_shdi <- rep(0, f_len_grid_2)
  f_grid_2$XL_frac_mn <- rep(0, f_len_grid_2)
  f_grid_2$XL_cohesion <- rep(0, f_len_grid_2)
  f_ls_metric <- data.frame()
  
  for (ii in 1: f_len_grid_1) {
    cat("Year_",f_year,'--Gird_', ii)
    fc_grid_id <- as.integer(ii);
    fc_grid_x <- f_grid_1b@polygons[[ii]]@labpt[1];
    fc_grid_y <- f_grid_1b@polygons[[ii]]@labpt[2];
    
    fc_lss_0 <- sample_lsm(f_luse, f_grid_1b[ii,], what = c('lsm_c_pland','lsm_c_cohesion', 'lsm_c_ai','lsm_c_shape_mn','lsm_c_enn_mn','lsm_c_ed','lsm_c_lsi','lsm_c_area_mn','lsm_l_lsi','lsm_l_pd','lsm_l_shdi','lsm_l_frac_mn','lsm_l_cohesion'), shape = 'square'); #to_be_set
    fc_lsc_0 <- sample_lsm(f_luse, f_grid_1b[ii,], what = c('lsm_c_pland'), shape = 'circle'); #to_be_set
    
    fc_class_lss <- data.frame(class = rep(c(1:8),8), metric=c(rep('pland',8),rep('cohesion',8), rep('ai',8), rep('shape_mn',8), rep('enn_mn',8), rep('ed',8), rep('lsi',8), rep('area_mn',8))) #to_be_set
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
    
    fc_shape_mn <- fc_lss %>% filter(metric == 'shape_mn') #aggregation index
    fc_shape_mn$value <- as.numeric(round(fc_shape_mn$value,2))
    fc_shape_mn_gre <- fc_shape_mn$value[fc_shape_mn$class == 1]
    fc_shape_mn_wat <- fc_shape_mn$value[fc_shape_mn$class == 5]
    fc_shape_mn_imp <- fc_shape_mn$value[fc_shape_mn$class == 8]

    fc_enn_mn <- fc_lss %>% filter(metric == 'enn_mn') #aggregation index
    fc_enn_mn$value <- as.numeric(round(fc_enn_mn$value,2))
    fc_enn_mn_gre <- fc_enn_mn$value[fc_enn_mn$class == 1]
    fc_enn_mn_wat <- fc_enn_mn$value[fc_enn_mn$class == 5]
    fc_enn_mn_imp <- fc_enn_mn$value[fc_enn_mn$class == 8]

    fc_ed <- fc_lss %>% filter(metric == 'ed') #aggregation index
    fc_ed$value <- as.numeric(round(fc_ed$value,2))
    fc_ed_gre <- fc_ed$value[fc_ed$class == 1]
    fc_ed_wat <- fc_ed$value[fc_ed$class == 5]
    fc_ed_imp <- fc_ed$value[fc_ed$class == 8]

    fc_area_mn <- fc_lss %>% filter(metric == 'area_mn') #aggregation index
    fc_area_mn$value <- as.numeric(round(fc_area_mn$value,2))
    fc_area_mn_gre <- fc_area_mn$value[fc_area_mn$class == 1]
    fc_area_mn_wat <- fc_area_mn$value[fc_area_mn$class == 5]
    fc_area_mn_imp <- fc_area_mn$value[fc_area_mn$class == 8]
    
    fc_lsi <- fc_lss %>% filter(metric == 'lsi') #aggregation index
    fc_lsi$value <- as.numeric(round(fc_lsi$value,2))
    fc_lsi_gre <- fc_lsi$value[fc_lsi$class == 1]
    fc_lsi_wat <- fc_lsi$value[fc_lsi$class == 5]
    fc_lsi_imp <- fc_lsi$value[fc_lsi$class == 8]
    
    fc_pd <- fc_lss_0 %>% filter(metric == 'pd') #patch density
    fc_pd$value <- as.numeric(round(fc_pd$value,2))
    fc_pd_v <- fc_pd$value
    
    fc_lsi <- fc_lss_0 %>% filter(metric == 'lsi') #patch density
    fc_lsi$value <- as.numeric(round(fc_lsi$value,2))
    fc_lsi_v <- fc_lsi$value

    fc_shdi <- fc_lss_0 %>% filter(metric == 'shdi') #patch density
    fc_shdi$value <- as.numeric(round(fc_shdi$value,2))
    fc_shdi_v <- fc_shdi$value
    
    fc_frac_mn <- fc_lss_0 %>% filter(metric == 'frac_mn') #patch density
    fc_frac_mn$value <- as.numeric(round(fc_frac_mn$value,2))
    fc_frac_mn_v <- fc_frac_mn$value

    fc_cohesion <- fc_lss_0 %>% filter(metric == 'fc_cohesion') #patch density
    fc_cohesion$value <- as.numeric(round(fc_cohesion$value,2))
    fc_cohesion_v <- fc_cohesion$value
    
    fc_pland_c <- fc_lsc %>% filter(metric == 'pland')
    fc_pland_c$value <- as.numeric(round(fc_pland_c$value,2))
    fc_pland_c_gre <- fc_pland_c$value[fc_pland_c$class == 1]
    fc_pland_c_wat <- fc_pland_c$value[fc_pland_c$class == 5]
    fc_pland_c_imp <- fc_pland_c$value[fc_pland_c$class == 8]
    
    fc_df <- data.frame(NUMBER = fc_grid_id, XL_grid_x = fc_grid_x, XL_grid_y = fc_grid_y, 
                       XL_ps_imp = fc_pland_s_imp, XL_co_imp = fc_cohesion_imp, XL_pc_imp = fc_pland_c_imp, 
                       XL_ai_imp = fc_ai_imp, XL_shape_mn_imp = fc_shape_mn_imp, XL_enn_mn_imp = fc_enn_mn_imp, XL_ed_imp = fc_ed_imp,XL_lsi_imp = fc_lsi_imp,XL_area_mn_imp = fc_area_mn_imp, 
                       XL_ps_gre = fc_pland_s_gre, XL_co_gre = fc_cohesion_gre, XL_pc_gre = fc_pland_c_gre, 
                       XL_ai_gre = fc_ai_gre, XL_shape_mn_gre = fc_shape_mn_gre, XL_enn_mn_gre = fc_enn_mn_gre, XL_ed_gre = fc_ed_gre,XL_lsi_gre = fc_lsi_gre,XL_area_mn_gre = fc_area_mn_gre,
                       XL_ps_wat = fc_pland_s_wat, XL_co_wat = fc_cohesion_wat, XL_pc_wat = fc_pland_c_wat, 
                       XL_ai_wat = fc_ai_wat, XL_shape_mn_wat = fc_shape_mn_wat, XL_enn_mn_wat = fc_enn_mn_wat, XL_ed_wat = fc_ed_wat,XL_lsi_wat = fc_lsi_wat,XL_area_mn_wat = fc_area_mn_wat,
                       XL_pd = fc_pd_v, XL_lsi = fc_lsi_v, XL_shdi = fc_shdi_v, XL_frac_mn = fc_frac_mn_v, XL_cohesion = fc_cohesion_v) 
    
    f_ls_metric <- rbind(f_ls_metric,fc_df)
  
    f_grid_2$XL_grid_id[ii] <- fc_grid_id
    f_grid_2$XL_grid_x[ii] <- fc_grid_x
    f_grid_2$XL_grid_y[ii] <- fc_grid_y
    f_grid_2$XL_ps_imp[ii] <- fc_pland_s_imp
    f_grid_2$XL_pc_imp[ii] <- fc_pland_c_imp
    f_grid_2$XL_co_imp[ii] <- fc_cohesion_imp
    f_grid_2$XL_ai_imp[ii] <- fc_ai_imp
    f_grid_2$XL_shape_mn_imp[ii] <- fc_shape_mn_imp
    f_grid_2$XL_enn_mn_imp[ii] <- fc_enn_mn_imp
    f_grid_2$XL_ed_imp[ii] <- fc_ed_imp
    f_grid_2$XL_lsi_imp[ii] <- fc_lsi_imp
    f_grid_2$XL_area_mn_imp[ii] <- fc_area_mn_imp
    f_grid_2$XL_ps_gre[ii] <- fc_pland_s_gre
    f_grid_2$XL_pc_gre[ii] <- fc_pland_c_gre
    f_grid_2$XL_co_gre[ii] <- fc_cohesion_gre
    f_grid_2$XL_ai_gre[ii] <- fc_ai_gre
    f_grid_2$XL_shape_mn_gre[ii] <- fc_shape_mn_gre
    f_grid_2$XL_enn_mn_gre[ii] <- fc_enn_mn_gre
    f_grid_2$XL_ed_gre[ii] <- fc_ed_gre
    f_grid_2$XL_lsi_gre[ii] <- fc_lsi_gre
    f_grid_2$XL_area_mn_gre[ii] <- fc_area_mn_gre
    f_grid_2$XL_ps_wat[ii] <- fc_pland_s_wat
    f_grid_2$XL_pc_wat[ii] <- fc_pland_c_wat
    f_grid_2$XL_co_wat[ii] <- fc_cohesion_wat
    f_grid_2$XL_ai_wat[ii] <- fc_ai_wat
    f_grid_2$XL_shape_mn_wat[ii] <- fc_shape_mn_wat
    f_grid_2$XL_enn_mn_wat[ii] <- fc_enn_mn_wat
    f_grid_2$XL_ed_wat[ii] <- fc_ed_wat
    f_grid_2$XL_lsi_wat[ii] <- fc_lsi_wat
    f_grid_2$XL_area_mn_wat[ii] <- fc_area_mn_wat
    f_grid_2$XL_pd[ii] <- fc_pd_v
    f_grid_2$XL_lsi[ii] <- fc_lsi_v
    f_grid_2$XL_shdi[ii] <- fc_shdi_v
    f_grid_2$XL_frac_mn[ii] <- fc_frac_mn_v
    f_grid_2$XL_cohesion[ii] <- fc_cohesion_v
  }
  
  st_write(f_grid_2, paste0('shp/3/ppa_2301_lsi_', f_year, '_buf', f_buffer, '.shp'))
  write.csv(f_ls_metric,paste0('shp/3/ppa_2301_lsi_', f_year, '_buf', f_buffer, '.csv'),row.names = FALSE)
}

#=========================================================================
years <- c(2020,2021,2022); #to_be_set
buffers <- c(200,400,500,600,800,1000); #to_be_set
for(c_year in years){
  for(c_buffer in buffers){
    cat('Year:', c_year, '.  Buffer:', c_buffer)
    ls_index_f(c_year, c_buffer)
  }
}

