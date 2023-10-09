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

#============================================================

#initial setting
index_y <- list('rx_rci', 'rx_crci')

cor_data_f <- function(f_year){
  
  #set empty lists
  f_model_list <- list()
  f_model_sum_list <- list()
  f_model_anova_list <- list()
  f_res_list <- list()
  
  #input data
  setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/shp')
  f_data_1 <- read.csv(paste0('2301_river_6_', f_year,'.csv')) 
  setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/shp/outputs')
  
  f_data_2 = f_data_1[,c(16,18,38,39,41)] #to_be_set
  f_row <- 3 #to_be_set
  f_col <- 2 #to_be_set
  
  #plot data
  plot(f_data_2)
  chart.Correlation(f_data_2, method = 'pearson', pch = 19, col = 'blue', tl.cex = 1.2)
  hist(f_data_2$rx_crci) 
  
  #correlation analysis
  f_cor_1 <- cor(f_data_2)
  
  #model analysis
  f_model_list[[1]] = lm(rx_rci ~ rx_ps_imp + rx_co_imp + XG_ANGLE_2, data = f_data_2)
  f_model_list[[2]] = lm(rx_crci ~ rx_ps_imp + rx_co_imp + XG_ANGLE_2, data = f_data_2)
  
  f_lm_cor <- matrix(0, nrow = f_row, ncol = f_col) #to_be_set
  f_lm_slope <- matrix(0, nrow = f_row, ncol = f_col) #to_be_set
  f_lm_p <- matrix(0, nrow = f_row, ncol = f_col) #to_be_set
  f_lm_r <- matrix(0, nrow = 2, ncol = f_col) #to_be_set
#============================================================

  for (ii in 1: f_col){
    
    #plot model
    par(mfrow=c(2,2))
    plot(f_model_list[[ii]])
    
    #model results
    f_model_sum_list[[ii]] <- summary(f_model_list[[ii]])
    f_model_anova_list[[ii]] <- anova(f_model_list[[ii]])
  
    f_lm_cor[1,ii] <- f_cor_1[index_y[[ii]],'rx_ps_imp']
    f_lm_cor[2,ii] <- f_cor_1[index_y[[ii]],'rx_co_imp']
    f_lm_cor[3,ii] <- f_cor_1[index_y[[ii]],'XG_ANGLE_2']
    f_lm_slope[1,ii] <- f_model_list[[ii]]$coefficients['rx_ps_imp']
    f_lm_slope[2,ii] <- f_model_list[[ii]]$coefficients['rx_co_imp']
    f_lm_slope[3,ii] <- f_model_list[[ii]]$coefficients['XG_ANGLE_2']
    f_lm_p[1,ii] <- f_model_sum_list[[ii]]$coefficients['rx_ps_imp','Pr(>|t|)']
    f_lm_p[2,ii] <- f_model_sum_list[[ii]]$coefficients['rx_co_imp','Pr(>|t|)']
    f_lm_p[3,ii] <- f_model_sum_list[[ii]]$coefficients['XG_ANGLE_2','Pr(>|t|)']
    f_lm_r[1,ii] <- f_model_sum_list[[ii]]$r.squared
    f_lm_r[2,ii] <- f_model_sum_list[[ii]]$adj.r.squared
  }
  
  write.csv(f_lm_cor, file = paste0('2301_lm_cor_',f_year,'.csv'), row.names = FALSE)
  write.csv(f_lm_slope, file = paste0('2301_lm_slope_',f_year,'.csv'), row.names = FALSE)
  write.csv(f_lm_p, file = paste0('2301_lm_p_',f_year,'.csv'), row.names = FALSE)
  write.csv(f_lm_r, file = paste0('2301_lm_r_',f_year,'.csv'), row.names = FALSE)
  
  f_res_list[[ii]][['lm_cor']] <- f_lm_cor 
  f_res_list[[ii]][['lm_slope']] <- f_lm_slope 
  f_res_list[[ii]][['lm_p']] <- f_lm_p
  f_res_list[[ii]][['lm_r']] <- f_lm_r
  
  return(f_res_list)
}
#==========================

years <- seq(2015, 2016, by = 1) #to_be_set
year_len <- length(years)
res_list <- list()
ii <- 1
for (c_year in years){
  res_list[[ii]] <- cor_data_f(c_year)
  ii <- ii + 1
}

