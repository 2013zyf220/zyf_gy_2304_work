library(raster)
library(terra)
library(sf)
library(landscapemetrics)
library(dplyr)

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2302_k2/ARCGIS')
#======================================================

luse_1p <- raster('LUSE/luse_1p.tif')
class_tc <- 2 #to_be_set
buf_num <- 300 #to_be_set

#======================================================


ls_index_f <- function(f_buf_set){
  f_luse <- luse_1p
  f_shp_0 <- paste0('LINES/streets_5_buf', f_buf_set, '.shp')
  f_grid_1 <- shapefile(f_shp_0)[1:buf_num,] 
  f_grid_2 <- st_read(f_shp_0)[1:buf_num,]
  f_len_grid_1 <- length(f_grid_1) #to_be_set
  
  f_grid_2$XL_grid_x <- rep(0, f_len_grid_1)
  f_grid_2$XL_grid_y <- rep(0, f_len_grid_1)
  f_grid_2$XL_ps_tc <- rep(0, f_len_grid_1)
  f_ls_metric <- data.frame()
  
  for (ii in 1: f_len_grid_1) {
    cat(ii)
    fc_grid_id <- as.integer(ii);
    fc_grid_x <- f_grid_1@polygons[[ii]]@labpt[1];
    fc_grid_y <- f_grid_1@polygons[[ii]]@labpt[2];
    fc_lss_0 <- sample_lsm(f_luse, f_grid_1[ii,], what = c('lsm_c_pland'), shape = 'square'); #to_be_set

    fc_class_lss <- data.frame(class = rep(c(2,5,6,7,9),1), metric=c(rep('pland', 5))) #to_be_set
    fc_lss <- left_join(fc_class_lss, fc_lss_0)
    fc_lss$value <- ifelse(is.na(fc_lss$value), 0, fc_lss$value)
    
    fc_pland_s <- fc_lss %>% filter(metric == 'pland')
    fc_pland_s$value <- as.numeric(round(fc_pland_s$value, 2))
    fc_pland_s_tc <- fc_pland_s$value[fc_pland_s$class == class_tc]
    
    fc_df <- data.frame(NUMBER = fc_grid_id, XL_grid_x = fc_grid_x, XL_grid_y = fc_grid_y, XL_ps_tc = fc_pland_s_tc) 
    f_ls_metric <- rbind(f_ls_metric,fc_df)
    
    f_grid_2$XL_grid_id[ii] <- fc_grid_id
    f_grid_2$XL_grid_x[ii] <- fc_grid_x
    f_grid_2$XL_grid_y[ii] <- fc_grid_y
    f_grid_2$XL_ps_tc[ii] <- fc_pland_s_tc
  }
  
  f_ls_metric_2 <- round(f_ls_metric, 3) 
  write.csv(f_ls_metric_2,paste0('LUSE/ppa_2302_tc_buf', f_buf_set, '.csv'), row.names = FALSE)
  
  f_res_list <- list()
  f_res_list[['ls_metric']] <- f_ls_metric_2
  return(f_res_list)
}

#============

buf_set <- 100 #to_be_set
tc_1 <- ls_index_f(buf_set)$ls_metric
colnames(tc_1) <- c("NUMBER3", "XL_grid_x3", "XL_grid_y3", "XL_ps_tc")

index_name3 <- paste0('index_1m_df3_buf', buf_set, '.csv')
index_name4 <- paste0('index_1m_df4_buf', buf_set, '.csv')
index_3 <- read.csv(index_name3)[1:buf_num,]
index_4 <- cbind(index_3, tc_1)
write.csv(index_4, paste0(index_name4), row.names = FALSE)
