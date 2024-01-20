library(terra)
library(data.table)
library(dplyr)
library(raster)
library(rgdal)
library(utils)
library(rgdal)
library(sf)
library(sp)
library(tidyr)
library(ggplot2)

#=========================================================

setwd('E:/zyf_gn/zyf_gn_2301_data');
extent_1 <- extent(c(106.4, 106.7, 29.4, 29.75)); #to_be_set

bh_1a <- raster(paste0('building_height_k2/CNBH10m_X107Y29_rep2.tif')); #to_be_set
bh_1b <- raster(paste0('chongqing_bh/cq_bh5.tif')); #to_be_set
bh_1c <- raster(paste0('ppa_2301_k2/raster/bh_sum_6.tif')); #to_be_set
bh_1d <- raster(paste0('ppa_2301_k2/raster/bh_sum_7.tif')); #to_be_set
bh_list <- list(bh_1a, bh_1b, bh_1c, bh_1d); #to_be_set

#=================================================
bh_f1 <- function(f_bh_1, f_buffer){
  f_bh_2 <- crop(f_bh_1, extent_1) #to_be_set
  
  f_buffer_2 <- paste0('ppa_2301_k2/shp/6/arcgis/cq_water_', f_buffer, '.shp') #to_be_set
  f_grid_0 <- shapefile(f_buffer_2); #to_be_set
  f_grid_1 <- f_grid_0
  f_grid_len <- length(f_grid_1);
  
  plot(f_bh_2)
  plot(f_grid_1, add = T)
  
  f_res_bh_5 <- list();
  f_res_bh_6 <- list();
  f_res_bh_7 <- list();
  f_res_bh_6_len <- rep(0, f_grid_len);
  f_res_bh_7_len <- rep(0, f_grid_len);
  f_res_bh_6_mean <- rep(0, f_grid_len);
  f_res_bh_7_mean <- rep(0, f_grid_len);
  f_res_bh_6_std <- rep(0, f_grid_len);
  f_res_bh_7_std <- rep(0, f_grid_len);
  f_res_bh_ratio <- rep(0, f_grid_len);
  f_res_bh_max <- rep(0, f_grid_len);
  f_res_bh_sum <- rep(0, f_grid_len);
  f_res_bh_ci <- rep(0, f_grid_len);
  f_res_NUMBER <- rep(0, f_grid_len);
  
  for(f_ii in 1: f_grid_len){
    cat('Grid: ', f_ii);
    fc_bh_3 <- crop(f_bh_2, f_grid_1[f_ii, ]);
    fc_bh_4 <- mask(fc_bh_3, f_grid_1[f_ii, ]);
    #plot(fc_bh_3);
    #plot(fc_bh_4);
    
    fc_bh_5 <- getValues(fc_bh_4);
    fc_bh_6 <- na.omit(fc_bh_5);
    fc_bh_7 <- fc_bh_6[fc_bh_6 != 0]
    
    f_res_bh_5[[f_ii]] <- fc_bh_5;
    f_res_bh_6[[f_ii]] <- fc_bh_6;
    f_res_bh_7[[f_ii]] <- fc_bh_7;
    f_res_bh_6_len[f_ii] <- length(fc_bh_6);
    f_res_bh_7_len[f_ii] <- length(fc_bh_7);
    f_res_bh_ratio[f_ii] <- f_res_bh_7_len[f_ii]/f_res_bh_6_len[f_ii]
    f_res_NUMBER[f_ii] <- f_ii
    
    c_1 <- length(fc_bh_5) - 2;
    c_2 <- length(fc_bh_7) - 2;
    if(sum(is.na(fc_bh_5)) > c_1){
      f_res_bh_6_mean[f_ii] <- 0;
      f_res_bh_7_mean[f_ii] <- 0;
      f_res_bh_6_std[f_ii] <- 0;
      f_res_bh_7_std[f_ii] <- 0;
      f_res_bh_max[f_ii] <- 0;
      f_res_bh_sum[f_ii] <- 0;
      f_res_bh_ci[f_ii] <- 0;
    }else if(sum(is.na(fc_bh_7)) > c_2){
      f_res_bh_6_mean[f_ii] <- 0;
      f_res_bh_7_mean[f_ii] <- 0;
      f_res_bh_6_std[f_ii] <- 0;
      f_res_bh_7_std[f_ii] <- 0;
      f_res_bh_max[f_ii] <- 0;
      f_res_bh_sum[f_ii] <- 0;
      f_res_bh_ci[f_ii] <- 0;      
    }else{
      f_res_bh_6_mean[f_ii] <- mean(fc_bh_6);
      f_res_bh_7_mean[f_ii] <- mean(fc_bh_7);
      f_res_bh_6_std[f_ii] <- sd(fc_bh_6);
      f_res_bh_7_std[f_ii] <- sd(fc_bh_7);
      f_res_bh_max[f_ii] <- max(fc_bh_7);
      f_res_bh_sum[f_ii] <- sum(fc_bh_7);
      f_res_bh_ci[f_ii] <- sum(fc_bh_7)/(f_res_bh_6_len[f_ii] * max(fc_bh_7))
    }
  }
  
  f_res_list <- list();
  f_res_list[['bh_5']] <- f_res_bh_5
  f_res_list[['bh_6']] <- f_res_bh_6
  f_res_list[['bh_7']] <- f_res_bh_7
  f_res_list[['bh_6_len']] <- f_res_bh_6_len
  f_res_list[['bh_7_len']] <- f_res_bh_7_len
  f_res_list[['bh_6_mean']] <- f_res_bh_6_mean
  f_res_list[['bh_7_mean']] <- f_res_bh_7_mean
  f_res_list[['bh_6_std']] <- f_res_bh_6_std
  f_res_list[['bh_7_std']] <- f_res_bh_7_std
  f_res_list[['bh_ratio']] <- f_res_bh_ratio
  f_res_list[['bh_max']] <- f_res_bh_max
  f_res_list[['bh_sum']] <- f_res_bh_sum
  f_res_list[['bh_ci']] <- f_res_bh_ci
  f_res_list[['NUMBER']] <- f_res_NUMBER
  f_res_list[['grid_len']] <- f_grid_len
  return(f_res_list)
}

#==============================================



buffers <- c('d01s5') #to_be_set_key
res_1a_sum <- list()
res_1b_sum <- list()
res_1c_sum <- list()
res_1d_sum <- list()
for(c_buf in buffers){
  cat('buffer:', c_buf)
  res_1a_sum[[c_buf]] <- bh_f1(bh_1a, c_buf);
  res_1b_sum[[c_buf]] <- bh_f1(bh_1b, c_buf);
  res_1c_sum[[c_buf]] <- bh_f1(bh_1c, c_buf);
  res_1d_sum[[c_buf]] <- bh_f1(bh_1d, c_buf);
  
  c_grid_2 <- st_read(paste0('ppa_2301_k2/shp/6/arcgis/cq_water_', c_buf, '.shp')) #to_be_set
  c_grid_2$XB1a_len_1 <- res_1a_sum[[c_buf]][['bh_6_len']]
  c_grid_2$XB1a_len_2 <- res_1a_sum[[c_buf]][['bh_7_len']]
  c_grid_2$XB1a_mean_1 <- res_1a_sum[[c_buf]][['bh_6_mean']]
  c_grid_2$XB1a_mean_2 <- res_1a_sum[[c_buf]][['bh_7_mean']]
  c_grid_2$XB1a_std_1 <- res_1a_sum[[c_buf]][['bh_6_std']]
  c_grid_2$XB1a_std_2 <- res_1a_sum[[c_buf]][['bh_7_std']]
  c_grid_2$XB1a_ratio <- res_1a_sum[[c_buf]][['bh_ratio']]
  c_grid_2$XB1a_ci <- res_1a_sum[[c_buf]][['bh_ci']]
  
  c_grid_2$XB1b_len_1 <- res_1b_sum[[c_buf]][['bh_6_len']]
  c_grid_2$XB1b_len_2 <- res_1b_sum[[c_buf]][['bh_7_len']]
  c_grid_2$XB1b_mean_1 <- res_1b_sum[[c_buf]][['bh_6_mean']]
  c_grid_2$XB1b_mean_2 <- res_1b_sum[[c_buf]][['bh_7_mean']]
  c_grid_2$XB1b_std_1 <- res_1b_sum[[c_buf]][['bh_6_std']]
  c_grid_2$XB1b_std_2 <- res_1b_sum[[c_buf]][['bh_7_std']]
  c_grid_2$XB1b_ratio <- res_1b_sum[[c_buf]][['bh_ratio']]
  c_grid_2$XB1b_ci <- res_1b_sum[[c_buf]][['bh_ci']]
  
  c_grid_2$XB1c_len_1 <- res_1c_sum[[c_buf]][['bh_6_len']]
  c_grid_2$XB1c_len_2 <- res_1c_sum[[c_buf]][['bh_7_len']]
  c_grid_2$XB1c_mean_1 <- res_1c_sum[[c_buf]][['bh_6_mean']]
  c_grid_2$XB1c_mean_2 <- res_1c_sum[[c_buf]][['bh_7_mean']]
  c_grid_2$XB1c_std_1 <- res_1c_sum[[c_buf]][['bh_6_std']]
  c_grid_2$XB1c_std_2 <- res_1c_sum[[c_buf]][['bh_7_std']]
  c_grid_2$XB1c_ratio <- res_1c_sum[[c_buf]][['bh_ratio']]
  c_grid_2$XB1c_ci <- res_1c_sum[[c_buf]][['bh_ci']]
  
  c_grid_2$XB1d_len_1 <- res_1d_sum[[c_buf]][['bh_6_len']]
  c_grid_2$XB1d_len_2 <- res_1d_sum[[c_buf]][['bh_7_len']]
  c_grid_2$XB1d_mean_1 <- res_1d_sum[[c_buf]][['bh_6_mean']]
  c_grid_2$XB1d_mean_2 <- res_1d_sum[[c_buf]][['bh_7_mean']]
  c_grid_2$XB1d_std_1 <- res_1d_sum[[c_buf]][['bh_6_std']]
  c_grid_2$XB1d_std_2 <- res_1d_sum[[c_buf]][['bh_7_std']]
  c_grid_2$XB1d_ratio <- res_1d_sum[[c_buf]][['bh_ratio']]
  c_grid_2$XB1d_ci <- res_1d_sum[[c_buf]][['bh_ci']]
  #st_write(c_grid_2, paste0('ppa_2301_k2/shp/5/res1/ppa_2301_bh_1_buf', c_buf, '.shp'))
  
  c_grid_len <- res_1a_sum[[c_buf]][['grid_len']]
  c_bh_data_export <- matrix(0, nrow = c_grid_len, ncol = 33); #to_be_set
  colnames(c_bh_data_export) <- c("XB1a_len_1", "XB1a_len_2", "XB1a_mean_1", "XB1a_mean_2", "XB1a_std_1", "XB1a_std_2", "XB1a_ratio", "XB1a_ci", 
                                  "XB1b_len_1", "XB1b_len_2", "XB1b_mean_1", "XB1b_mean_2", "XB1b_std_1", "XB1b_std_2", "XB1b_ratio", "XB1b_ci",
                                  "XB1c_len_1", "XB1c_len_2", "XB1c_mean_1", "XB1c_mean_2", "XB1c_std_1", "XB1c_std_2", "XB1c_ratio", "XB1c_ci",
                                  "XB1d_len_1", "XB1d_len_2", "XB1d_mean_1", "XB1d_mean_2", "XB1d_std_1", "XB1d_std_2", "XB1d_ratio", "XB1d_ci", "NUMBER")
  
  c_bh_data_export[, 1] <- res_1a_sum[[c_buf]][['bh_6_len']]
  c_bh_data_export[, 2] <- res_1a_sum[[c_buf]][['bh_7_len']]
  c_bh_data_export[, 3] <- res_1a_sum[[c_buf]][['bh_6_mean']]
  c_bh_data_export[, 4] <- res_1a_sum[[c_buf]][['bh_7_mean']]
  c_bh_data_export[, 5] <- res_1a_sum[[c_buf]][['bh_6_std']]
  c_bh_data_export[, 6] <- res_1a_sum[[c_buf]][['bh_7_std']]
  c_bh_data_export[, 7] <- res_1a_sum[[c_buf]][['bh_ratio']]
  c_bh_data_export[, 8] <- res_1a_sum[[c_buf]][['bh_ci']]
  
  c_bh_data_export[, 9] <- res_1b_sum[[c_buf]][['bh_6_len']]
  c_bh_data_export[, 10] <- res_1b_sum[[c_buf]][['bh_7_len']]
  c_bh_data_export[, 11] <- res_1b_sum[[c_buf]][['bh_6_mean']]
  c_bh_data_export[, 12] <- res_1b_sum[[c_buf]][['bh_7_mean']]
  c_bh_data_export[, 13] <- res_1b_sum[[c_buf]][['bh_6_std']]
  c_bh_data_export[, 14] <- res_1b_sum[[c_buf]][['bh_7_std']]
  c_bh_data_export[, 15] <- res_1b_sum[[c_buf]][['bh_ratio']]
  c_bh_data_export[, 16] <- res_1b_sum[[c_buf]][['bh_ci']]
  
  c_bh_data_export[, 17] <- res_1c_sum[[c_buf]][['bh_6_len']]
  c_bh_data_export[, 18] <- res_1c_sum[[c_buf]][['bh_7_len']]
  c_bh_data_export[, 19] <- res_1c_sum[[c_buf]][['bh_6_mean']]
  c_bh_data_export[, 20] <- res_1c_sum[[c_buf]][['bh_7_mean']]
  c_bh_data_export[, 21] <- res_1c_sum[[c_buf]][['bh_6_std']]
  c_bh_data_export[, 22] <- res_1c_sum[[c_buf]][['bh_7_std']]
  c_bh_data_export[, 23] <- res_1c_sum[[c_buf]][['bh_ratio']]
  c_bh_data_export[, 24] <- res_1c_sum[[c_buf]][['bh_ci']]
  
  c_bh_data_export[, 25] <- res_1d_sum[[c_buf]][['bh_6_len']]
  c_bh_data_export[, 26] <- res_1d_sum[[c_buf]][['bh_7_len']]
  c_bh_data_export[, 27] <- res_1d_sum[[c_buf]][['bh_6_mean']]
  c_bh_data_export[, 28] <- res_1d_sum[[c_buf]][['bh_7_mean']]
  c_bh_data_export[, 29] <- res_1d_sum[[c_buf]][['bh_6_std']]
  c_bh_data_export[, 30] <- res_1d_sum[[c_buf]][['bh_7_std']]
  c_bh_data_export[, 31] <- res_1d_sum[[c_buf]][['bh_ratio']]
  c_bh_data_export[, 32] <- res_1d_sum[[c_buf]][['bh_ci']]
  c_bh_data_export[, 33] <- res_1a_sum[[c_buf]][['NUMBER']]
  
  write.csv(c_bh_data_export, file = paste0('ppa_2301_k2/shp/6/res2/ppa_2301_bh_1_buf', c_buf, '.csv'), row.names = FALSE)
}

