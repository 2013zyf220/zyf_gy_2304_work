library(terra)
library(data.table)
library(dplyr)
library(raster)
library(rgdal)
library(utils)
library(rgdal)
library(sf)
library(sp)
library(ggplot2)

#=========================================================

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/V2');
extent_1 <- extent(c(106.4, 106.7, 29.4, 29.75))
bh_1 <- raster(paste0('DATA_BH_1/cq_bh4pb.tif'))

#=================================================

bh_f1 <- function(f_bh_1, f_buffer){
  f_bh_2 <- crop(f_bh_1, extent_1)
  f_grid_1 <- shapefile(paste0('DATA_SHP_1/BUF/cq_water_bs', f_buffer, 'ip.shp'));
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
    fc_bh_3[is.na(fc_bh_3)] <- 0
    fc_bh_4 <- mask(fc_bh_3, f_grid_1[f_ii, ]);
    plot(fc_bh_3);
    plot(fc_bh_4);
    
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

buffers <- '4' #to_be_set_key
res_1_sum <- list()

for(c_buf in buffers){
  cat('buffer:', c_buf)
  res_1_sum[[c_buf]] <- bh_f1(bh_1, c_buf);
  
  c_grid_2 <- st_read(paste0('DATA_SHP_1/BUF/cq_water_bs', c_buf, 'ip.shp'))
  
  c_grid_2$XB1_len_1 <- res_1_sum[[c_buf]][['bh_6_len']]
  c_grid_2$XB1_len_2 <- res_1_sum[[c_buf]][['bh_7_len']]
  c_grid_2$XB1_mean_1 <- res_1_sum[[c_buf]][['bh_6_mean']]/3
  c_grid_2$XB1_mean_2 <- res_1_sum[[c_buf]][['bh_7_mean']]
  c_grid_2$XB1_std_1 <- res_1_sum[[c_buf]][['bh_6_std']]/3
  c_grid_2$XB1_std_2 <- res_1_sum[[c_buf]][['bh_7_std']]
  c_grid_2$XB1_ratio <- res_1_sum[[c_buf]][['bh_ratio']]*100
  c_grid_2$XB1_ci <- res_1_sum[[c_buf]][['bh_ci']]
  
  #st_write(c_grid_2, paste0('DATA_ANA_1/ppa_2301_bh_bs', c_buf, '.shp'))
  
  c_grid_len <- res_1_sum[[c_buf]][['grid_len']]
  c_bh_data_export <- matrix(0, nrow = c_grid_len, ncol = 9)
  colnames(c_bh_data_export) <- c("XB1_len_2", "XB1_len_1", "XB1_mean_2", "XB1_mean_1", "XB1_std_2", "XB1_std_1", "XB1_ratio", "XB1_ci","NUMBER")
  
  c_bh_data_export[, 1] <- res_1_sum[[c_buf]][['bh_7_len']]
  c_bh_data_export[, 2] <- res_1_sum[[c_buf]][['bh_6_len']]
  c_bh_data_export[, 3] <- res_1_sum[[c_buf]][['bh_7_mean']]
  c_bh_data_export[, 4] <- res_1_sum[[c_buf]][['bh_6_mean']]/3
  c_bh_data_export[, 5] <- res_1_sum[[c_buf]][['bh_7_std']]
  c_bh_data_export[, 6] <- res_1_sum[[c_buf]][['bh_6_std']]/3
  c_bh_data_export[, 7] <- res_1_sum[[c_buf]][['bh_ratio']]*100
  c_bh_data_export[, 8] <- res_1_sum[[c_buf]][['bh_ci']]
  c_bh_data_export[, 9] <- res_1_sum[[c_buf]][['NUMBER']]
  
  write.csv(c_bh_data_export, file = paste0('DATA_ANA_1/ppa_2301_bh_bs', c_buf, '.csv'), row.names = FALSE)
}

#===================================
#check
#grid_ii <- 26 #to_be_set
#check_grid_1 <- shapefile(paste0('DATA_SHP_1/BUF/cq_water_bs4ip.shp'));
#check_bh_2 <- crop(bh_1, extent_1) #to_be_set
#plot(check_bh_2)
#check_bh_3 <- crop(check_bh_2, check_grid_1[grid_ii, ])
#check_bh_3[is.na(check_bh_3)] <- 0
#plot(check_bh_3)
#plot(check_grid_1[grid_ii, ], add = T)
#check_bh_3m <- as.matrix(check_bh_3)
#check_bh_3m_x <- ncol(check_bh_3m)
#check_bh_3m_y <- nrow(check_bh_3m)
#check_bh_4 <- mask(check_bh_3, check_grid_1[grid_ii, ])
#plot(check_bh_4)
#plot(check_grid_1[grid_ii, ], add = T)
#check_bh_4m <- as.matrix(check_bh_4)
#check_bh_4m_x <- ncol(check_bh_4m)
#check_bh_4m_y <- nrow(check_bh_4m)

#check_bh_5 <- getValues(check_bh_4)
#check_bh_6 <- na.omit(check_bh_5)
#check_bh_7 <- check_bh_6[check_bh_6 != 0]
