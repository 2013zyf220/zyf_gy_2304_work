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

setwd("E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/shp/3")
#============================================================

orders <- c(3,4)
buffers <- c(800,1000)

res_r2_rcd <- matrix(0, nrow = length(orders), ncol = length(buffers))
res_r2_rci <- matrix(0, nrow = length(orders), ncol = length(buffers))
res_r2_crci <- matrix(0, nrow = length(orders), ncol = length(buffers))
ii <- 0
for(c_order in orders){
  ii <- ii + 1
  jj <- 0
  for(c_buffer in buffers){
    jj <- jj + 1
    f_data_1 <- read.csv(paste0('2301_lm_r2_s', c_order, '_buf', c_buffer, '.csv'))
    res_r2_rcd[ii,jj] <- f_data_1[1,2]
    res_r2_rci[ii,jj] <- f_data_1[1,3]
    res_r2_crci[ii,jj] <- f_data_1[1,4]
  }
}

colnames(res_r2_rcd) <- buffers
#write.csv(res_r2_rcd, file = paste0('2301_sum_res_r2_rcd.csv'), row.names = TRUE)

