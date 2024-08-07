library(raster)
library(terra)
library(sf)
library(landscapemetrics)
library(dplyr)

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2302_k2/ARCGIS')

#=================================================================
#up2024_0604_23:04

#luse_1p <- raster('LUSE/luse_1p.tif')
luse_1p <- raster('LUSEB/landuse_b4.tif')
luse_2p <- raster('LUSEB/landuse_b5.tif')
#=================================================================
#up2024_0604_23:04

buf_set <- 100 #to_be_set
buf_num <- 300 #to_be_set
buf_1 <- shapefile(paste0('LINES/streets_5_buf', buf_set, '.shp'))
buf_1p <- spTransform(buf_1, crs(luse_1p))
buf_2 <- buf_1p[1:buf_num,]

plot(luse_1p)
plot(buf_2, add = T)

class_imp <- 1 #to_be_set
class_bld <- 2 #to_be_set
class_veg <- 3 #to_be_set

#===================================
#up2024_0604_20:30

ls_index_f <- function(f_luse_set, f_buf_set){
  if(f_luse_set == 1){
    f_luse <- luse_1p
  }else if(f_luse_set == 2){
    f_luse <- luse_2p
  }else{
    print('ERROR')
  }
  f_shp_0 <- paste0('LINES/streets_5_buf', f_buf_set, '.shp')
  f_grid_1 <- shapefile(f_shp_0)[1:buf_num,] 
  f_grid_2 <- st_read(f_shp_0)[1:buf_num,]
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
    fc_pland_s_bld <- fc_pland_s$value[fc_pland_s$class == class_bld]
    fc_pland_s_veg <- fc_pland_s$value[fc_pland_s$class == class_veg]
    
    fc_cohesion <- fc_lss %>% filter(metric == 'cohesion') #patch cohesion index
    fc_cohesion$value <- as.numeric(round(fc_cohesion$value,2))
    
    fc_cohesion_bld <- fc_cohesion$value[fc_cohesion$class == class_bld]
    fc_cohesion_veg <- fc_cohesion$value[fc_cohesion$class == class_veg]
    
    fc_shdi <- fc_lss_0 %>% filter(metric == 'shdi') #patch density
    fc_shdi$value <- as.numeric(round(fc_shdi$value,2))
    fc_shdi_v <- fc_shdi$value
    
    fc_pland_c <- fc_lsc %>% filter(metric == 'pland')
    fc_pland_c$value <- as.numeric(round(fc_pland_c$value,2))
    
    fc_pland_c_bld <- fc_pland_c$value[fc_pland_c$class == class_bld]
    fc_pland_c_veg <- fc_pland_c$value[fc_pland_c$class == class_veg]
    
    if(f_luse_set == 1){
      fc_pland_s_imp <- fc_pland_s$value[fc_pland_s$class == class_imp]
      fc_cohesion_imp <- fc_cohesion$value[fc_cohesion$class == class_imp]
      fc_pland_c_imp <- fc_pland_c$value[fc_pland_c$class == class_imp]
    }else{
      fc_pland_s_imp <- -9999
      fc_cohesion_imp <- -9999
      fc_pland_c_imp <- -9999
    }
    
    fc_df <- data.frame(NUMBER = fc_grid_id, XL_grid_x = fc_grid_x, XL_grid_y = fc_grid_y,
                        XL_ps_imp = fc_pland_s_imp, XL_co_imp = fc_cohesion_imp, XL_pc_imp = fc_pland_c_imp, 
                        XL_ps_bld = fc_pland_s_bld, XL_co_bld = fc_cohesion_bld, XL_pc_bld = fc_pland_c_bld, 
                        XL_ps_veg = fc_pland_s_veg, XL_co_veg = fc_cohesion_veg, XL_pc_veg = fc_pland_c_veg, 
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
#up2024_0620_16:30

buf_set <- 150 #to_be_set
lsi_1 <- ls_index_f(1, buf_set)$ls_metric
lsi_2 <- ls_index_f(2, buf_set)$ls_metric

colnames(lsi_2) <- c("NUMBER2", "XL_grid_x2", "XL_grid_y2", "XL_ps_imp2", "XL_co_imp2","XL_pc_imp2", 
                     "XL_ps_bld2", "XL_co_bld2","XL_pc_bld2", "XL_ps_veg2", "XL_co_veg2","XL_pc_veg2", "XL_shdi")
#=============================================================
#up2024_0620_16:30

index_name2 <- paste0('index_1m_df2_buf', buf_set, '.csv')
index_name3 <- paste0('index_1m_df3_buf', buf_set, '.csv')

index_1 <- read.csv(index_name2)[1:buf_num,]
index_2 <- cbind(lsi_1, index_1)
index_3 <- cbind(lsi_2, index_2)
write.csv(index_3,paste0(index_name3), row.names = FALSE)
