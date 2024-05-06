library(MASS)
library(skimr)
library(DataExplorer)
library(tidyverse)
library(caret)
library(pROC)
library(ggplot2)
library(readxl)
library(raster)

setwd("E:/zyf_gn/zyf_gn_2301_data/ppa_2302_k2/ARCGIS")

#=========================================================================
#up2024_0503_19:27_s
#basic setting

bh_1 <- raster('BH/BH_CP_4.tif')
ele_1 <- raster(paste0('DEM/DEM_ELE_2.tif'))
slp_1 <- raster(paste0('DEM/DEM_SLP_2.tif'))
asp_1 <- raster(paste0('DEM/DEM_ASP_2.tif'))

buf_set <- 20 #to_be_set
strs_1 <- shapefile(paste0('LINES/streets_5_buf', buf_set, '.shp'))
strs_1_size <- nrow(strs_1)

plot(bh_1)
#plot(ele_1)
#plot(slp_1)
#plot(asp_1)
plot(strs_1, add = T)

#up2024_0503_19:27_e
#=========================================================================
#up2024_0503_19:37_s

bh_index_f <- function(f_buf){
  f_buf_size <- nrow(f_buf)
  f_bh_NUMBER <- 1: f_buf_size
  f_bh_3_len <- rep(0, f_buf_size) 
  f_bh_4_len <- rep(0, f_buf_size) 
  f_bh_ratio <- rep(0, f_buf_size) 
  
  f_bh_3_mean <- rep(0, f_buf_size)
  f_bh_4_mean <- rep(0, f_buf_size)
  f_bh_3_std <- rep(0, f_buf_size)
  f_bh_4_std <- rep(0, f_buf_size)
  f_bh_max <- rep(0, f_buf_size)
  f_bh_sum <- rep(0, f_buf_size)
  f_bh_ci <- rep(0, f_buf_size)
  
  f_bh_sub_2v <- list()
  f_bh_sub_3 <- list()
  f_bh_sub_4 <- list()
  for(ii in 1: f_buf_size){
    if(ii %% 50 == 0){
      cat('buffer:', ii, '\n')
    }

    fc_bh_sub_1 <- crop(bh_1, extent(f_buf[ii, ]))
    fc_bh_sub_1[is.na(fc_bh_sub_1)] <- 0
    fc_bh_sub_2 <- mask(fc_bh_sub_1, f_buf[ii, ])
    f_bh_sub_2v[[ii]] <- getValues(fc_bh_sub_2)
    f_bh_sub_3[[ii]] <- na.omit(f_bh_sub_2v[[ii]])
    f_bh_sub_4[[ii]] <- f_bh_sub_3[[ii]][f_bh_sub_3[[ii]] != 0]
    
    f_bh_3_len[ii] <- length(f_bh_sub_3[[ii]])
    f_bh_4_len[ii] <- length(f_bh_sub_4[[ii]])
    f_bh_ratio[ii] <- f_bh_4_len[ii]/f_bh_3_len[ii]
    
    if(f_bh_4_len[ii] == 0){
      f_bh_3_mean[ii] <- 0
      f_bh_4_mean[ii] <- 0
      f_bh_3_std[ii] <- 0
      f_bh_4_std[ii] <- 0
      f_bh_max[ii] <- 0
      f_bh_sum[ii] <- 0
      f_bh_ci[ii] <- 0      
    }else{
      f_bh_3_mean[ii] <- mean(f_bh_sub_3[[ii]])
      f_bh_4_mean[ii] <- mean(f_bh_sub_4[[ii]])
      f_bh_3_std[ii] <- sd(f_bh_sub_3[[ii]])
      f_bh_4_std[ii] <- sd(f_bh_sub_4[[ii]])
      f_bh_max[ii] <- max(f_bh_sub_4[[ii]])
      f_bh_sum[ii] <- sum(f_bh_sub_4[[ii]])
      f_bh_ci[ii] <- sum(f_bh_sub_4[[ii]])/(f_bh_4_len[ii] * f_bh_max[ii])      
    }
  }
  
  f_res_list <- list()
  f_res_list[['NUMBER']] <- f_bh_NUMBER
  f_res_list[['bh_2v']] <- f_bh_sub_2v
  f_res_list[['bh_3']] <- f_bh_sub_3
  f_res_list[['bh_4']] <- f_bh_sub_4
  
  f_res_list[['bh_3_len']] <- f_bh_3_len
  f_res_list[['bh_4_len']] <- f_bh_4_len
  f_res_list[['bh_ratio']] <- f_bh_ratio
  
  f_res_list[['bh_3_mean']] <- f_bh_3_mean
  f_res_list[['bh_4_mean']] <- f_bh_4_mean
  f_res_list[['bh_3_std']] <- f_bh_3_std
  f_res_list[['bh_4_std']] <- f_bh_4_std
  f_res_list[['bh_max']] <- f_bh_max
  f_res_list[['bh_sum']] <- f_bh_sum
  f_res_list[['bh_ci']] <- f_bh_ci
  
  return(f_res_list)
}

#up2024_0503_19:37_e
#=========================================================================
#up2024_0503_19:39_s

dem_index_f <- function(f_dem, f_buf){
  f_buf_size <- nrow(f_buf)
  f_dem_3_mean <- rep(0, f_buf_size)
  f_dem_3_std <- rep(0, f_buf_size)
  
  f_dem_sub_2v <- list()
  f_dem_sub_3 <- list()
  for(ii in 1: f_buf_size){
    fc_dem_sub_1 <- crop(f_dem, extent(f_buf[ii, ]))
    fc_dem_sub_1[is.na(fc_dem_sub_1)] <- 0
    fc_dem_sub_2 <- mask(fc_dem_sub_1, f_buf[ii, ])
    f_dem_sub_2v[[ii]] <- getValues(fc_dem_sub_2)
    f_dem_sub_3[[ii]] <- na.omit(f_dem_sub_2v[[ii]])
    f_dem_3_mean[ii] <- mean(f_dem_sub_3[[ii]])
    f_dem_3_std[ii] <- sd(f_dem_sub_3[[ii]])
  }
  
  f_res_list <- list()
  f_res_list[['dem_2v']] <- f_dem_sub_2v
  f_res_list[['dem_3']] <- f_dem_sub_3
  
  f_res_list[['NUMBER']] <- 1: f_buf_size
  f_res_list[['mean']] <- f_dem_3_mean
  f_res_list[['std']] <- f_dem_3_std
  return(f_res_list)
}

#up2024_0503_19:39_e
#==================================
#up2024_0503_19:41_s

index_bh_1 <- bh_index_f(strs_1)
index_ele_1 <- dem_index_f(ele_1, strs_1)
index_slp_1 <- dem_index_f(slp_1, strs_1)
index_asp_1 <- dem_index_f(asp_1, strs_1)

index_1m <- matrix(0, nrow = strs_1_size, ncol = 11)
index_1m[ ,1] <- index_bh_1$bh_3_mean
index_1m[ ,2] <- index_bh_1$bh_4_mean
index_1m[ ,3] <- index_bh_1$bh_3_std
index_1m[ ,4] <- index_bh_1$bh_4_std
index_1m[ ,5] <- index_bh_1$bh_ratio
index_1m[ ,6] <- index_bh_1$bh_max
index_1m[ ,7] <- index_bh_1$bh_sum
index_1m[ ,8] <- index_bh_1$bh_ci
index_1m[ ,9] <- index_ele_1$mean
index_1m[ ,10] <- index_ele_1$std
index_1m[ ,11] <- index_slp_1$mean

index_col_names <- c('bh_3_mean', 'bh_4_mean', 'bh_3_std', 'bh_4_std', 'bh_ratio', 'bh_max', 'bh_sum', 'bh_ci', 'ele_mean', 'ele_std', 'slp_mean') #to_be_set

index_1m_df <- as.data.frame(index_1m)
colnames(index_1m_df) <- index_col_names
write.csv(index_1m_df, file = "index_1m_df.csv", row.names = FALSE)
#up2024_0503_19:41_e
#==================================
#check

check_bh_set_1 <- 25  #to_be_set
check_bh_1 <- crop(bh_1, extent(strs_1[check_bh_set_1, ]))
plot(check_bh_1)
check_bh_1[is.na(check_bh_1)] <- 0
plot(check_bh_1)
check_bh_2 <- mask(check_bh_1, strs_1[check_bh_set_1, ])
plot(check_bh_2)

check_dem_set_1 <- 19  #to_be_set
check_slp_1 <- crop(slp_1, extent(strs_1[check_dem_set_1, ]))
plot(check_slp_1)
check_slp_1[is.na(check_slp_1)] <- 0
plot(check_slp_1)
check_slp_2 <- mask(check_slp_1, strs_1[check_dem_set_1, ])
plot(check_slp_2)