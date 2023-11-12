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
index_x <- list('XG_NDVI_ME', 'XG_SLOPE_M', 'rx_co_imp', 'bh1a_M6')    #to_be_set
index_y <- list('rx_rcd', 'rx_rci', 'rx_crci')    #to_be_set

row_1 <- length(index_x)
col_1 <- length(index_y)

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
  f_data_1b = f_data_1[,c(3,7,18,33,34,35,44)]    #to_be_set
  f_data_1c <- f_data_1b[f_data_1b$rx_rci != 0, ] #to_be_set
  f_data_2 <- f_data_1c[-c(49,142,143), ] #to_be_set
  setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/shp/outputs2')
  
  f_data_out_1 <- f_data_2[,c(1,6)]  #to_be_set
  
  #plot data
  plot(f_data_2)
  chart.Correlation(f_data_2, method = 'pearson', pch = 19, col = 'blue', tl.cex = 1.2)
  #hist(f_data_2$rx_crci)  #to_be_set
  
  #correlation analysis
  f_cor_1 <- cor(f_data_2)
  
  #model analysis
  f_model_formula <- list();
  for(kk in 1:3){
    f_model_formula[[kk]] <- as.formula(paste0(index_y[kk], ' ~ ', paste(index_x, collapse = ' + ')));
    f_model_list[[kk]] <- lm(f_model_formula[[kk]], data = f_data_2) #to_be_set
  }
  
  #set empty arrays
  f_lm_cor <- matrix(0, nrow = row_1, ncol = col_1)
  f_lm_slope <- matrix(0, nrow = row_1, ncol = col_1)
  f_lm_p <- matrix(0, nrow = row_1, ncol = col_1)
  f_lm_r2 <- matrix(0, nrow = 3, ncol = col_1)
#============================================================

  for (ii in 1: col_1){
    
    #plot model
    par(mfrow = c(2,2))
    plot(f_model_list[[ii]])
    
    #model results
    f_model_sum_list[[ii]] <- summary(f_model_list[[ii]])
    f_model_anova_list[[ii]] <- anova(f_model_list[[ii]])
    
    for (jj in 1: row_1){
      f_lm_cor[jj,ii] <- f_cor_1[index_y[[ii]],index_x[[jj]]]
      f_lm_slope[jj,ii] <- f_model_list[[ii]]$coefficients[index_x[[jj]]]
      f_lm_p[jj,ii] <- f_model_sum_list[[ii]]$coefficients[index_x[[jj]],'Pr(>|t|)']
    }

    f_lm_r2[1,ii] <- f_model_sum_list[[ii]]$r.squared
    f_lm_r2[2,ii] <- f_model_sum_list[[ii]]$adj.r.squared
    f_lm_r2[3,ii] <- f_model_sum_list[[ii]]$fstatistic[1]
    
    f_res_list[[ii]] <- list()
    f_res_list[[ii]][['model_sum_list']] <- f_model_sum_list[[ii]]
    f_res_list[[ii]][['model_anova_list']] <- f_model_anova_list[[ii]]
  }
  
  colnames(f_lm_cor) <- index_y
  rownames(f_lm_cor) <- index_x
  colnames(f_lm_slope) <- index_y
  rownames(f_lm_slope) <- index_x  
  colnames(f_lm_p) <- index_y
  rownames(f_lm_p) <- index_x  
  
  f_res_list[['lm_cor']] <- f_lm_cor
  f_res_list[['lm_slope']] <- f_lm_slope
  f_res_list[['lm_p']] <- f_lm_p
  f_res_list[['lm_r']] <- f_lm_r2
  f_res_list[['out_1']] <- f_data_out_1
  f_res_list[['model_formula']] <- f_model_formula
  write.csv(f_lm_cor, file = paste0('2301_lm_cor_',f_year,'.csv'), row.names = FALSE)
  write.csv(f_lm_slope, file = paste0('2301_lm_slope_',f_year,'.csv'), row.names = FALSE)
  write.csv(f_lm_p, file = paste0('2301_lm_p_',f_year,'.csv'), row.names = FALSE)
  write.csv(f_lm_r2, file = paste0('2301_lm_r2_',f_year,'.csv'), row.names = FALSE)
  
  return(f_res_list)
}
#==========================

years <- seq(2021, 2021, by = 1) #to_be_set
year_len <- length(years)
res_list <- list()
ii <- 1
for (c_year in years){
  res_list[[ii]] <- cor_data_f(c_year)
  ii <- ii + 1
}

#==========================
cor_exa_1 <- res_list[[1]][['out_1']] #to_be_set
cor_exa_2 <- cor(cor_exa_1)
