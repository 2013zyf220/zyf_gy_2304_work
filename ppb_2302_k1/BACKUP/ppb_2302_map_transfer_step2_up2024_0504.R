


library(raster)
library(sp)
library(sf)
library(rgdal)
library(ggplot2)

setwd('E:/zyf_gn/zyf_gn_2301_data/ppb_2302_k1/outputs')

#=====================================
#up2024_0503_17:50_s

years <- seq(1998, 2022, 6)  #to_be_set
len_years <- length(years)
len_years_2 <- len_years - 1

years_start <- rep(0, len_years_2)
years_end <- rep(0, len_years_2)
for(ii in 1: len_years_2){
  years_start[ii] <- years[ii]
  years_end[ii] <- years[ii + 1]
}

years_start_all <- years_start[1]
years_end_all <- years_end[len_years_2]

shp_data_1 <- c('chongqing_1')  #to_be_set
len_shp_data_1 <- length(shp_data_1)

type_set <- 8  #to_be_set

#up2024_0503_17:50_e
#=====================================
#up2024_0503_17:50_s

#to_be_set_s
overlay_1_f <- function(f_1, f_2, f_3, f_4, f_5){
  f_res_1 <- ifelse(f_1 == 8, 1, NA)
  f_res_1[f_2 == 1] <- 2
  f_res_1[f_3 == 1] <- 3
  f_res_1[f_4 == 1] <- 4
  f_res_1[f_5 == 1] <- 5
  return(f_res_1)
}
#to_be_set_e

#up2024_0503_17:50_e
#=====================================
#up2024_0503_17:56_s

overlay_2_f <- function(f_shp_data){
  f_data_start <- raster(paste0('map_start_', type_set, '_', years_start_all,  '_', f_shp_data, '.tif'))
  f_data_list <- list()
  for(ii in 1: len_years_2){
    f_data_list[[ii]] <- raster(paste0('map_trans_', type_set, '_', years_start[ii], '_', years_end[ii], '_', f_shp_data, '.tif'))
  }
  f_overlay_res_1 <- overlay(f_data_start, f_data_list[[1]], f_data_list[[2]], f_data_list[[3]], f_data_list[[4]], fun = overlay_1_f) #to_be_set
  f_out_name_set <- paste0('map_trans2_', type_set, '_', years_start_all, '_', years_end_all, '_', f_shp_data, '.tif')
  writeRaster(f_overlay_res_1, filename = f_out_name_set, format = 'GTiff', overwrite = TRUE)
}

#up2024_0503_17:56_e
#=====================================
#up2024_0503_17:56_s

for(ii in 1: len_shp_data_1){
  overlay_2_f(shp_data_1[ii])
}

#up2024_0503_17:56_e
#=====================================
