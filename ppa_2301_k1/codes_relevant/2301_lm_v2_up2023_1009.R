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
index_y <- list('price', 'price2')
index_x <- list('bedrooms', 'bathrooms', 'sqft.living')

cor_data_f <- function(f_year) {
  
  #set empty lists
  f_model_list <- list()
  f_model_sum_list <- list()
  f_model_anova_list <- list()
  f_res_list <- list()
  
  #input data
  setwd('E:/zyf_gy/zyf_gy_2304_work/ppa_2301_k1/codes_relevant') 
  f_data_1 <- read.csv(paste0('housing_data_', f_year,'.csv')) 
  setwd('E:/zyf_gy/zyf_gy_2304_work/ppa_2301_k1/codes_relevant/outputs') 
  
  f_data_2 = f_data_1[,c(2,3,4,5,6,7)] #to_be_set
  f_row <- 3 #to_be_set
  f_col <- 2 #to_be_set
  
  #plot data
  plot(f_data_2)
  chart.Correlation(f_data_2, method = 'pearson', pch = 19, col = 'blue', tl.cex = 1.2)
  hist(f_data_2$price) 
  
  #correlation analysis
  f_cor_1 <- cor(f_data_2)
  
  #model analysis
  f_model_list[[1]] = lm(price ~ bedrooms + bathrooms + sqft.living, data = f_data_2)
  f_model_list[[2]] = lm(price2 ~ bedrooms + bathrooms + sqft.living, data = f_data_2)
  
  #set empty arrays
  f_lm_cor <- matrix(0, nrow = f_row, ncol = f_col) #to_be_set
  f_lm_slope <- matrix(0, nrow = f_row, ncol = f_col) #to_be_set
  f_lm_p <- matrix(0, nrow = f_row, ncol = f_col) #to_be_set
  f_lm_r <- matrix(0, nrow = 2, ncol = f_col) #to_be_set
#============================================================
  
  for (ii in 1:f_col){
    
    #plot model
    par(mfrow = c(2,2))
    plot(f_model_list[[ii]])
    
    #model results
    f_model_sum_list[[ii]] <- summary(f_model_list[[ii]])
    f_model_anova_list[[ii]] <- anova(f_model_list[[ii]])
    
    f_lm_cor[1,ii] <- f_cor_1[index_y[[ii]],'bedrooms']
    f_lm_cor[2,ii] <- f_cor_1[index_y[[ii]],'bathrooms']
    f_lm_cor[3,ii] <- f_cor_1[index_y[[ii]],'sqft.living']
    f_lm_slope[1,ii] <- f_model_list[[ii]]$coefficients['bedrooms']
    f_lm_slope[2,ii] <- f_model_list[[ii]]$coefficients['bathrooms']
    f_lm_slope[3,ii] <- f_model_list[[ii]]$coefficients['sqft.living']
    f_lm_p[1,ii] <- f_model_sum_list[[ii]]$coefficients['bedrooms','Pr(>|t|)']
    f_lm_p[2,ii] <- f_model_sum_list[[ii]]$coefficients['bathrooms','Pr(>|t|)']
    f_lm_p[3,ii] <- f_model_sum_list[[ii]]$coefficients['sqft.living','Pr(>|t|)']
    f_lm_r[1,ii] <- f_model_sum_list[[ii]]$r.squared
    f_lm_r[2,ii] <- f_model_sum_list[[ii]]$adj.r.squared
  }
  
  rownames(f_lm_cor) <- index_x
  colnames(f_lm_cor) <- index_y
  rownames(f_lm_slope) <- index_x
  colnames(f_lm_slope) <- index_y
  rownames(f_lm_p) <- index_x
  colnames(f_lm_p) <- index_y
  rownames(f_lm_r) <- list('r2', 'r2_adj')
  colnames(f_lm_r) <- index_y
  
  #export results to csv file
  write.csv(f_lm_cor, file = paste0('2301_lm_cor_',f_year,'.csv'), row.names = FALSE)
  write.csv(f_lm_slope, file = paste0('2301_lm_slope_',f_year,'.csv'), row.names = FALSE)
  write.csv(f_lm_p, file = paste0('2301_lm_p_',f_year,'.csv'), row.names = FALSE)
  write.csv(f_lm_r, file = paste0('2301_lm_r_',f_year,'.csv'), row.names = FALSE)
  
  f_res_list[['lm_cor']] <- f_lm_cor
  f_res_list[['lm_slope']] <- f_lm_slope
  f_res_list[['lm_p']] <- f_lm_p
  f_res_list[['lm_r']] <- f_lm_r
  
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



