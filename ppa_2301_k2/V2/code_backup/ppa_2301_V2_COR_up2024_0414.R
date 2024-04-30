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
setwd("E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/V2/DATA_ANA_1")

#initial setting
index_x <- list("XL_ps_imp","XL_ai_imp","XL_lsi_imp","XL_ai_gre","XL_lsi_gre","XB1_mean_1","XB1_mean_2","XB1_ratio","dem","slope","ndvi")
index_y <- list("XY_rcd", "XY_rci", "XY_crci")    #to_be_set
row_1 <- length(index_x)
col_1 <- length(index_y)

cor_data_f <- function(f_order){
  cat('order:', f_order)
  
  #input data and select rows and columns
  f_data_1 <- read.csv(paste0("ppa_2301_ana_s", f_order, ".csv")); 
  f_data_1b = f_data_1[, c(2,3,4,9,12,16,18,21,25,36,41,42,45,47,48,49,50,51,52,53)]  #to_be_set_key
  f_data_1c <- f_data_1b[f_data_1b$XY_rci != 0, ] #to_be_set
  f_data_2 <- f_data_1c[-c(31,32,33), ] #to_be_set_key
  
  #plot data
  #plot(f_data_2)
  chart.Correlation(f_data_2, method = "pearson", pch = 19, col = "blue", tl.cex = 1.2)
  #hist(f_data_2$rx_crci)  #to_be_set
  
  #correlation analysis
  f_cor_1 <- cor(f_data_2)

  #set empty lists
  f_model_formula <- list()
  f_model_list <- list()
  f_model_vif <- list()
  f_model_sum_list <- list()
  f_model_anova_list <- list()
  f_res_list <- list()
  
  #set empty arrays
  f_lm_cor <- matrix(0, nrow = row_1, ncol = col_1)
  f_lm_slope <- matrix(0, nrow = row_1, ncol = col_1)
  f_lm_p <- matrix(0, nrow = row_1, ncol = col_1)
  f_lm_r2 <- matrix(0, nrow = 3, ncol = col_1)
  #============================================================
  
  for (ii in 1: col_1){
    
    #model analysis
    f_model_formula[[ii]] <- as.formula(paste0(index_y[ii], " ~ ", paste(index_x, collapse = " + ")));
    f_model_list[[ii]] <- lm(f_model_formula[[ii]], data = f_data_2) #to_be_set
    f_model_vif[[ii]] <- vif(f_model_list[[ii]])
    
    #plot model
    par(mfrow = c(2,2))
    plot(f_model_list[[ii]])
    
    #model results
    f_model_sum_list[[ii]] <- summary(f_model_list[[ii]])
    f_model_anova_list[[ii]] <- anova(f_model_list[[ii]])
    
    for (jj in 1: row_1){
      f_lm_cor[jj,ii] <- f_cor_1[index_y[[ii]],index_x[[jj]]]
      f_lm_slope[jj,ii] <- f_model_list[[ii]]$coefficients[index_x[[jj]]]
      f_lm_p[jj,ii] <- f_model_sum_list[[ii]]$coefficients[index_x[[jj]], "Pr(>|t|)"]
    }
    
    f_lm_r2[1,ii] <- f_model_sum_list[[ii]]$r.squared
    f_lm_r2[2,ii] <- f_model_sum_list[[ii]]$adj.r.squared
    f_lm_r2[3,ii] <- f_model_sum_list[[ii]]$fstatistic[1]
    
    
  
    write.csv(f_model_vif[[ii]], file = paste0('RES/2301_lm_vif_s',f_order, '_buf', index_y[[ii]],'.csv'), row.names = TRUE)
  }
  
  colnames(f_lm_cor) <- index_y
  rownames(f_lm_cor) <- index_x
  colnames(f_lm_slope) <- index_y
  rownames(f_lm_slope) <- index_x  
  colnames(f_lm_p) <- index_y
  rownames(f_lm_p) <- index_x  
  
  f_res_list[["lm_cor"]] <- f_lm_cor
  f_res_list[["lm_slope"]] <- f_lm_slope
  f_res_list[["lm_p"]] <- f_lm_p
  f_res_list[["lm_r"]] <- f_lm_r2

  f_res_list[["model_list"]] <- f_model_list
  f_res_list[["model_vif"]] <- f_model_vif
  f_res_list[["model_formula"]] <- f_model_formula
  f_res_list[["model_sum_list"]] <- f_model_sum_list
  f_res_list[["model_anova_list"]] <- f_model_anova_list
  
  write.csv(f_lm_cor, file = paste0('RES/2301_lm_cor_s', f_order, '.csv'), row.names = TRUE)
  write.csv(f_lm_slope, file = paste0('RES/2301_lm_slope_s', f_order, '.csv'), row.names = TRUE)
  write.csv(f_lm_p, file = paste0('RES/2301_lm_p_s', f_order, '.csv'), row.names = TRUE)
  write.csv(f_lm_r2, file = paste0('RES/2301_lm_r2_s', f_order, '.csv'), row.names = TRUE)
  
  return(f_res_list)
}
#==========================

res_list <- list()
order_set <- c('4')  #to_be_set_key
res_list[[order_set]] <- cor_data_f(order_set)

