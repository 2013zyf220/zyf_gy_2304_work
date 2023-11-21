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
buffer_1 <- 1000   #to_be_set_key

if(buffer_1 == 200){
  buffer_2 <- '2301_cq_water_b11_buf200_a05.shp';
}else if(buffer_1 == 400){
  buffer_2 <- '2301_cq_water_b11_buf400_a04.shp';
}else if(buffer_1 == 500){
  buffer_2 <- '2301_cq_water_b11_buf500_a01.shp';
}else if(buffer_1 == 600){
  buffer_2 <- '2301_cq_water_b11_buf600_a08.shp';
}else if(buffer_1 == 800){
  buffer_2 <- '2301_cq_water_b11_buf800_a09.shp';
}else if(buffer_1 == 1000){
  buffer_2 <- '2301_cq_water_b11_buf1000_a16.shp';
}else{
  print('ERROR')
}

grid_0 <- shapefile(paste0('ppa_2301_k2/shp/2/', buffer_2)); #to_be_set
grid_1 <- spTransform(grid_0, '+init=epsg:4326')
plot(grid_1)
grid_len <- length(grid_1);

bh_1a <- raster(paste0('building_height_k2/CNBH10m_X107Y29_rep.tif')); #to_be_set
bh_1b <- raster(paste0('chongqing_bh/cq_bh4.tif')); #to_be_set
bh_1c <- raster(paste0('ppa_2301_k2/raster/bh_sum_3.tif')); #to_be_set
bh_1d <- raster(paste0('ppa_2301_k2/raster/bh_sum_2.tif')); #to_be_set
bh_list <- list(bh_1a, bh_1b, bh_1c, bh_1d); #to_be_set

plot(bh_1a, add = T);
#=================================================
bh_f1 <- function(f_bh_1){
  f_res_bh_5 <- list();
  f_res_bh_6 <- list();
  f_res_bh_7 <- list();
  f_res_bh_6_len <- rep(0, grid_len);
  f_res_bh_7_len <- rep(0, grid_len);
  f_res_bh_6_mean <- rep(0, grid_len);
  f_res_bh_7_mean <- rep(0, grid_len);
  f_res_bh_6_std <- rep(0, grid_len);
  f_res_bh_7_std <- rep(0, grid_len);
  f_res_bh_ratio <- rep(0, grid_len);
  
  f_bh_2 <- crop(f_bh_1, extent_1) #to_be_set
  plot(f_bh_2)
  
  for(f_ii in 1: grid_len){
    cat('Grid: ', f_ii);
    fc_bh_3 <- crop(f_bh_2, grid_1[f_ii, ]);
    fc_bh_4 <- mask(fc_bh_3, grid_1[f_ii, ]);
    #plot(fc_bh_3);
    #plot(fc_bh_4);
    
    fc_bh_5 <- getValues(fc_bh_4);
    fc_bh_6 <- na.omit(fc_bh_5);
    fc_bh_7 <- ifelse(is.na(fc_bh_5), 0, fc_bh_5);
    
    f_res_bh_5[[f_ii]] <- fc_bh_5;
    f_res_bh_6[[f_ii]] <- fc_bh_6;
    f_res_bh_7[[f_ii]] <- fc_bh_7;
    f_res_bh_6_len[f_ii] <- length(fc_bh_6);
    f_res_bh_7_len[f_ii] <- length(fc_bh_7);
    f_res_bh_ratio[f_ii] <- f_res_bh_6_len[f_ii]/f_res_bh_7_len[f_ii]
    
    c_1 <- length(fc_bh_5) - 2;
    if(sum(is.na(fc_bh_5)) > c_1){
      f_res_bh_6_mean[f_ii] <- 0;
      f_res_bh_7_mean[f_ii] <- 0;
      f_res_bh_6_std[f_ii] <- 0;
      f_res_bh_7_std[f_ii] <- 0;
    }else{
      f_res_bh_6_mean[f_ii] <- mean(fc_bh_6);
      f_res_bh_7_mean[f_ii] <- mean(fc_bh_7);
      f_res_bh_6_std[f_ii] <- sd(fc_bh_6);
      f_res_bh_7_std[f_ii] <- sd(fc_bh_7);
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
  
  return(f_res_list)
}

#==============================================

res_1a <- bh_f1(bh_1a);
res_1b <- bh_f1(bh_1b);
res_1c <- bh_f1(bh_1c);
res_1d <- bh_f1(bh_1d);

grid_2 <- st_read(paste0('ppa_2301_k2/shp/2/', buffer_2));
  
grid_2$bh1a_L6 <- res_1a[['bh_6_len']]
grid_2$bh1a_L7 <- res_1a[['bh_7_len']]
grid_2$bh1a_M6 <- res_1a[['bh_6_mean']]
grid_2$bh1a_M7 <- res_1a[['bh_7_mean']]
grid_2$bh1a_S6 <- res_1a[['bh_6_std']]
grid_2$bh1a_S7 <- res_1a[['bh_7_std']]
grid_2$bh1a_r <- res_1a[['bh_ratio']]
grid_2$bh1b_L6 <- res_1b[['bh_6_len']]
grid_2$bh1b_L7 <- res_1b[['bh_7_len']]
grid_2$bh1b_M6 <- res_1b[['bh_6_mean']]
grid_2$bh1b_M7 <- res_1b[['bh_7_mean']]
grid_2$bh1b_S6 <- res_1b[['bh_6_std']]
grid_2$bh1b_S7 <- res_1b[['bh_7_std']]
grid_2$bh1b_r <- res_1b[['bh_ratio']]
  
grid_2$bh1c_L6 <- res_1c[['bh_6_len']]
grid_2$bh1c_L7 <- res_1c[['bh_7_len']]
grid_2$bh1c_M6 <- res_1c[['bh_6_mean']]
grid_2$bh1c_M7 <- res_1c[['bh_7_mean']]
grid_2$bh1c_S6 <- res_1c[['bh_6_std']]
grid_2$bh1c_S7 <- res_1c[['bh_7_std']]
grid_2$bh1c_r <- res_1c[['bh_ratio']]
grid_2$bh1d_L6 <- res_1d[['bh_6_len']]
grid_2$bh1d_L7 <- res_1d[['bh_7_len']]
grid_2$bh1d_M6 <- res_1d[['bh_6_mean']]
grid_2$bh1d_M7 <- res_1d[['bh_7_mean']]
grid_2$bh1d_S6 <- res_1d[['bh_6_std']]
grid_2$bh1d_S7 <- res_1d[['bh_7_std']]
grid_2$bh1d_r <- res_1d[['bh_ratio']]
st_write(grid_2, paste0('ppa_2301_k2/shp/3/ppa_2301_bh_1_buf', buffer_1, '.shp'))


#==============================================

bh_data_export <- matrix(0, nrow = grid_len, ncol = 28); #to_be_set
bh_data_export[, 1] <- res_1a[['bh_6_len']]
bh_data_export[, 2] <- res_1a[['bh_7_len']]
bh_data_export[, 3] <- res_1a[['bh_6_mean']]
bh_data_export[, 4] <- res_1a[['bh_7_mean']]
bh_data_export[, 5] <- res_1a[['bh_6_std']]
bh_data_export[, 6] <- res_1a[['bh_7_std']]
bh_data_export[, 7] <- res_1a[['bh_ratio']]
bh_data_export[, 8] <- res_1b[['bh_6_len']]
bh_data_export[, 9] <- res_1b[['bh_7_len']]
bh_data_export[, 10] <- res_1b[['bh_6_mean']]
bh_data_export[, 11] <- res_1b[['bh_7_mean']]
bh_data_export[, 12] <- res_1b[['bh_6_std']]
bh_data_export[, 13] <- res_1b[['bh_7_std']]
bh_data_export[, 14] <- res_1b[['bh_ratio']]

bh_data_export[, 15] <- res_1c[['bh_6_len']]
bh_data_export[, 16] <- res_1c[['bh_7_len']]
bh_data_export[, 17] <- res_1c[['bh_6_mean']]
bh_data_export[, 18] <- res_1c[['bh_7_mean']]
bh_data_export[, 19] <- res_1c[['bh_6_std']]
bh_data_export[, 20] <- res_1c[['bh_7_std']]
bh_data_export[, 21] <- res_1c[['bh_ratio']]
bh_data_export[, 22] <- res_1d[['bh_6_len']]
bh_data_export[, 23] <- res_1d[['bh_7_len']]
bh_data_export[, 24] <- res_1d[['bh_6_mean']]
bh_data_export[, 25] <- res_1d[['bh_7_mean']]
bh_data_export[, 26] <- res_1d[['bh_6_std']]
bh_data_export[, 27] <- res_1d[['bh_7_std']]
bh_data_export[, 28] <- res_1d[['bh_ratio']]
colnames(bh_data_export) <- c("1a_len_1", "1a_len_2", "1a_mean_1", "1a_mean_2", "1a_std_1", "1a_std_2", "1a_ratio", 
                              "1b_len_1", "1b_len_2", "1b_mean_1", "1b_mean_2", "1b_std_1", "1b_std_2", "1b_ratio",
                              "1c_len_1", "1c_len_2", "1c_mean_1", "1c_mean_2", "1c_std_1", "1c_std_2", "1c_ratio", 
                              "1d_len_1", "1d_len_2", "1d_mean_1", "1d_mean_2", "1d_std_1", "1d_std_2", "1d_ratio")
write.csv(bh_data_export, file = paste0('ppa_2301_k2/shp/3/ppa_2301_bh_1_buf', buffer_1, '.csv'), row.names = FALSE)

