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
index_x <- list('XG_NDVI_ME', 'XG_SLOPE_M', 'rx_ps_imp', 'rx_co_imp', 'rx_ps_gre', 'rx_co_gre', 'XG_ANGLE_2')    #to_be_set
index_y <- list('rx_rci', 'rx_crci', 'rx_rcd')    #to_be_set
cor_data_f <- function(f_year){
  cat('Analysis Year:', f_year)
  #set empty lists
  f_model_list <- list()
  f_model_sum_list <- list()
  f_model_anova_list <- list()
  f_res_list <- list()
  
  #input data
  setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/shp/outputs2')
  f_data_1 <- read.csv(paste0('2301_river_6_', f_year,'.csv')) 
  f_data_1b = f_data_1[,c(3,7,16,18,22,24,37,38,39,44)]    #to_be_set
  f_data_2 <- f_data_1b[f_data_1b$rx_rci != 0, ] #to_be_set
  setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/shp/outputs2')
  
  #set parameters
  f_row <- length(index_x)
  f_col <- length(index_y)
  
  #plot data
  plot(f_data_2)
  chart.Correlation(f_data_2, method = 'pearson', pch = 19, col = 'blue', tl.cex = 1.2)
  hist(f_data_2$rx_crci)  #to_be_set
  
  #correlation analysis
  f_cor_1 <- cor(f_data_2)
  
  #model analysis
  f_model_list[[1]] <- lm(rx_rci ~ XG_NDVI_ME + XG_SLOPE_M + rx_ps_imp + rx_co_imp + rx_ps_gre + rx_co_gre + XG_ANGLE_2, data = f_data_2) #to_be_set
  f_model_list[[2]] <- lm(rx_crci ~ XG_NDVI_ME + XG_SLOPE_M + rx_ps_imp + rx_co_imp + rx_ps_gre + rx_co_gre + XG_ANGLE_2, data = f_data_2) #to_be_set
  f_model_list[[3]] <- lm(rx_rcd ~ XG_NDVI_ME + XG_SLOPE_M + rx_ps_imp + rx_co_imp + rx_ps_gre + rx_co_gre + XG_ANGLE_2, data = f_data_2) #to_be_set
  #set empty arrays
  f_lm_cor <- matrix(0, nrow = f_row, ncol = f_col)
  f_lm_slope <- matrix(0, nrow = f_row, ncol = f_col)
  f_lm_p <- matrix(0, nrow = f_row, ncol = f_col)
  f_lm_r <- matrix(0, nrow = 3, ncol = f_col)
#============================================================

  for (ii in 1: f_col){
    
    #plot model
    par(mfrow = c(2,2))
    plot(f_model_list[[ii]])
    
    #model results
    f_model_sum_list[[ii]] <- summary(f_model_list[[ii]])
    f_model_anova_list[[ii]] <- anova(f_model_list[[ii]])
    
    for (jj in 1: f_row){
      f_lm_cor[jj,ii] <- f_cor_1[index_y[[ii]],index_x[[jj]]]
      f_lm_slope[jj,ii] <- f_model_list[[ii]]$coefficients[index_x[[jj]]]
      f_lm_p[jj,ii] <- f_model_sum_list[[ii]]$coefficients[index_x[[jj]],'Pr(>|t|)']
    }

    f_lm_r[1,ii] <- f_model_sum_list[[ii]]$r.squared
    f_lm_r[2,ii] <- f_model_sum_list[[ii]]$adj.r.squared
    f_lm_r[3,ii] <- f_model_sum_list[[ii]]$fstatistic[1]
    
    f_res_list[[ii]] <- list()
    f_res_list[[ii]][['model_sum_list']] <- f_model_sum_list[[ii]]
    f_res_list[[ii]][['model_anova_list']] <- f_model_anova_list[[ii]]
  }
  
  f_res_list[['lm_cor']] <- f_lm_cor
  f_res_list[['lm_slope']] <- f_lm_slope
  f_res_list[['lm_p']] <- f_lm_p
  f_res_list[['lm_r']] <- f_lm_r
  
  write.csv(f_lm_cor, file = paste0('2301_lm_cor_',f_year,'.csv'), row.names = FALSE)
  write.csv(f_lm_slope, file = paste0('2301_lm_slope_',f_year,'.csv'), row.names = FALSE)
  write.csv(f_lm_p, file = paste0('2301_lm_p_',f_year,'.csv'), row.names = FALSE)
  write.csv(f_lm_r, file = paste0('2301_lm_r_',f_year,'.csv'), row.names = FALSE)
  
  return(f_res_list)
}
#==========================

years <- seq(2019, 2022, by = 1) #to_be_set
year_len <- length(years)
res_list <- list()
ii <- 1
for (c_year in years){
  res_list[[ii]] <- cor_data_f(c_year)
  ii <- ii + 1
}

