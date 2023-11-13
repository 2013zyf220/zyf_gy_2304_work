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

bh_1a <- raster(paste0('building_height_k2/CNBH10m_X107Y29_rep.tif')); #to_be_set
bh_1b <- raster(paste0('chongqing_bh/cq_bh4.tif')); #to_be_set
bh_1c <- raster(paste0('ppa_2301_k2/raster/bh_sum_3.tif')); #to_be_set
bh_1d <- raster(paste0('ppa_2301_k2/raster/bh_sum_2.tif')); #to_be_set
bh_list <- list(bh_1a, bh_1b, bh_1c, bh_1d); #to_be_set

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/shp/outputs2');
grid_1 <- shapefile(paste0('2301_river_5_2022.shp')); #to_be_set
plot(grid_1)
grid_len <- length(grid_1);

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
  
  for(ii in 1: grid_len){
    cat('Grid: ', ii);
    fc_bh_3 <- crop(f_bh_2, grid_1[ii, ]);
    fc_bh_4 <- mask(fc_bh_3, grid_1[ii, ]);
    #plot(fc_bh_3);
    #plot(fc_bh_4);
    
    fc_bh_5 <- getValues(fc_bh_4);
    fc_bh_6 <- na.omit(fc_bh_5);
    fc_bh_7 <- ifelse(is.na(fc_bh_5), 0, fc_bh_5);
    
    f_res_bh_5[[ii]] <- fc_bh_5;
    f_res_bh_6[[ii]] <- fc_bh_6;
    f_res_bh_7[[ii]] <- fc_bh_7;
    f_res_bh_6_len[ii] <- length(fc_bh_6);
    f_res_bh_7_len[ii] <- length(fc_bh_7);
    f_res_bh_ratio[ii] <- f_res_bh_6_len[ii]/f_res_bh_7_len[ii]
    
    c_1 <- length(fc_bh_5) - 2;
    if(sum(is.na(fc_bh_5)) > c_1){
      f_res_bh_6_mean[ii] <- 0;
      f_res_bh_7_mean[ii] <- 0;
      f_res_bh_6_std[ii] <- 0;
      f_res_bh_7_std[ii] <- 0;
    }else{
      f_res_bh_6_mean[ii] <- mean(fc_bh_6);
      f_res_bh_7_mean[ii] <- mean(fc_bh_7);
      f_res_bh_6_std[ii] <- sd(fc_bh_6);
      f_res_bh_7_std[ii] <- sd(fc_bh_7);
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
year_s <- 2021; #to_be_set
year_e <- 2021; #to_be_set
res_1a <- bh_f1(bh_1a);
res_1b <- bh_f1(bh_1b);
res_1c <- bh_f1(bh_1c);
res_1d <- bh_f1(bh_1d);

for(c_year in year_s: year_e){
  c_grid_1a <- shapefile(paste0('2301_river_5_', c_year,'.shp'));
  c_grid_1b <- spTransform(c_grid_1a, '+init=epsg:4326');
  c_grid_2 <- st_read(paste0('2301_river_5_', c_year,'.shp'));

  c_grid_2$bh1a_L6 <- res_1a[['bh_6_len']]
  c_grid_2$bh1a_L7 <- res_1a[['bh_7_len']]
  c_grid_2$bh1a_M6 <- res_1a[['bh_6_mean']]
  c_grid_2$bh1a_M7 <- res_1a[['bh_7_mean']]
  c_grid_2$bh1a_S6 <- res_1a[['bh_6_std']]
  c_grid_2$bh1a_S7 <- res_1a[['bh_7_std']]
  c_grid_2$bh1a_r <- res_1a[['bh_ratio']]
  c_grid_2$bh1b_L6 <- res_1b[['bh_6_len']]
  c_grid_2$bh1b_L7 <- res_1b[['bh_7_len']]
  c_grid_2$bh1b_M6 <- res_1b[['bh_6_mean']]
  c_grid_2$bh1b_M7 <- res_1b[['bh_7_mean']]
  c_grid_2$bh1b_S6 <- res_1b[['bh_6_std']]
  c_grid_2$bh1b_S7 <- res_1b[['bh_7_std']]
  c_grid_2$bh1b_r <- res_1b[['bh_ratio']]

  c_grid_2$bh1c_L6 <- res_1c[['bh_6_len']]
  c_grid_2$bh1c_L7 <- res_1c[['bh_7_len']]
  c_grid_2$bh1c_M6 <- res_1c[['bh_6_mean']]
  c_grid_2$bh1c_M7 <- res_1c[['bh_7_mean']]
  c_grid_2$bh1c_S6 <- res_1c[['bh_6_std']]
  c_grid_2$bh1c_S7 <- res_1c[['bh_7_std']]
  c_grid_2$bh1c_r <- res_1c[['bh_ratio']]
  c_grid_2$bh1d_L6 <- res_1d[['bh_6_len']]
  c_grid_2$bh1d_L7 <- res_1d[['bh_7_len']]
  c_grid_2$bh1d_M6 <- res_1d[['bh_6_mean']]
  c_grid_2$bh1d_M7 <- res_1d[['bh_7_mean']]
  c_grid_2$bh1d_S6 <- res_1d[['bh_6_std']]
  c_grid_2$bh1d_S7 <- res_1d[['bh_7_std']]
  c_grid_2$bh1d_r <- res_1d[['bh_ratio']]
  st_write(c_grid_2, paste0('2301_river_5b_', c_year,'.shp'))
}

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
write.csv(bh_data_export, file = paste0('2301_bh_sum.csv'), row.names = FALSE)

