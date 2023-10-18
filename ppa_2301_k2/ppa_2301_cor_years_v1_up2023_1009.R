library(rgdal)
library(raster)
library(sf)
library(sp)
library(dplyr)
library(tidyr)
library(landscapemetrics)
library(ggplot2)
library(PerformanceAnalytics)
library(car)
#rm(list = ls())

#===================================================

#initial setting
setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/shp/outputs')
years <- seq(2015, 2016, by = 1) #to_be_set
year_len <- length(years)

index_x <- list('bedrooms', 'bathrooms', 'sqft.living') #to_be_set
index_y <- list('price', 'price2') #to_be_set
row_len <- length(index_x)
col_len <- length(index_y)

#set empty arrays
lm_cor_years <- array(0, dim = c(row_len, col_len, year_len))
lm_slope_years <- array(0, dim = c(row_len, col_len, year_len))
lm_p_years <- array(0, dim = c(row_len, col_len, year_len))
lm_r_years <- array(0, dim = c(2, col_len, year_len))

ii <- 1
for (c_year in years){
  
  #input data of individual yearss
  c_lm_cor_year <- read.csv(paste0('2301_lm_cor_', c_year,'.csv'));
  c_lm_slope_year <- read.csv(paste0('2301_lm_slope_', c_year,'.csv'));
  c_lm_p_year <- read.csv(paste0('2301_lm_p_', c_year,'.csv'));
  c_lm_r_year <- read.csv(paste0('2301_lm_r_', c_year,'.csv'));
  
  for (jj in 1:row_len){
    for (kk in 1:col_len){
      lm_cor_years[jj,kk,ii] <- c_lm_cor_year[jj,kk]
      lm_slope_years[jj,kk,ii] <- c_lm_slope_year[jj,kk]
      lm_p_years[jj,kk,ii] <- c_lm_p_year[jj,kk]
   }
  }
  
  for (jj in 1:2){
    for (kk in 1:col_len){
      lm_r_years[jj,kk,ii] <- c_lm_r_year[jj,kk]
    }
  }
  ii <- ii + 1;
}

#================================================
#Plot line charts
for (jj in 1:row_len){
  for (kk in 1:col_len){
    plot(lm_cor_years[jj,kk,], type = 'l', col = 'blue', xlab = 'Year', ylab = index_y[[kk]], main = paste0('cor_', index_x[[jj]],'_vs_',index_x[[kk]]))
  }
}


