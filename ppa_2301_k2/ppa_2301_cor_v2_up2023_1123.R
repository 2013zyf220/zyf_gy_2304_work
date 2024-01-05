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
setwd("E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/shp/3/res")

#initial setting
#index_x <- list("XL_ps_imp","XL_co_imp","XL_ai_imp","XL_lsi_imp","XL_ps_gre","XL_co_gre","XL_ai_gre","XL_lsi_gre", "XB1b_mean_1","XB1b_mean_2","XB1b_std_1","XB1b_ratio","XT_DEM_MEA","XT_SLOPE_M","XN_NDVI_ME", "XA_RIVERW")
index_x <- list("XL_ps_imp","XL_co_imp","XL_ai_imp","XL_lsi_imp","XL_co_gre","XL_lsi_gre", "XB1b_mean_1","XB1b_mean_2","XB1b_ratio","XT_DEM_MEA","XT_SLOPE_M","XN_NDVI_ME", "XA_RIVERW")
index_y <- list("XY_rcd", "XY_rci", "XY_crci")    #to_be_set
row_1 <- length(index_x)
col_1 <- length(index_y)

cor_data_f <- function(f_order, f_buffer){
  cat('order:', f_order)
  cat('buffer:', f_buffer)
  #set empty lists
  f_model_list <- list()
  f_model_sum_list <- list()
  f_model_anova_list <- list()
  f_res_list <- list()
  f_model_formula <- list()
  f_model_vif <- list()
  #input data
  f_data_1 <- read.csv(paste0('ppa_2301_ana_s', f_order, '_buf', f_buffer, '.csv')) 
  f_data_1b = f_data_1[, c(5,6,8,12,14,15,17,21,45,46,47,49,69,71,76,77,78,79,90)]  #to_be_set_key
  f_data_1c <- f_data_1b[f_data_1b$XY_rci != 0, ] #to_be_set
  f_data_2 <- f_data_1c #to_be_set (another option：f_data_2 <- f_data_1c[-c(37,174), ])

  
  f_data_out_1 <- f_data_2[,c(1,6)]  #to_be_set
  
  #plot data
  #plot(f_data_2)
  chart.Correlation(f_data_2, method = "pearson", pch = 19, col = "blue", tl.cex = 1.2)
  #hist(f_data_2$rx_crci)  #to_be_set
  
  #correlation analysis
  f_cor_1 <- cor(f_data_2)
  
  #model analysis
  
  for(kk in 1: col_1){
    f_model_formula[[kk]] <- as.formula(paste0(index_y[kk], " ~ ", paste(index_x, collapse = " + ")));
    f_model_list[[kk]] <- lm(f_model_formula[[kk]], data = f_data_2) #to_be_set
    f_model_vif[[kk]] <- vif(f_model_list[[kk]])
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
      f_lm_p[jj,ii] <- f_model_sum_list[[ii]]$coefficients[index_x[[jj]], "Pr(>|t|)"]
    }

    f_lm_r2[1,ii] <- f_model_sum_list[[ii]]$r.squared
    f_lm_r2[2,ii] <- f_model_sum_list[[ii]]$adj.r.squared
    f_lm_r2[3,ii] <- f_model_sum_list[[ii]]$fstatistic[1]
    
    f_res_list[[ii]] <- list()
    f_res_list[[ii]][["model_sum_list"]] <- f_model_sum_list[[ii]]
    f_res_list[[ii]][["model_anova_list"]] <- f_model_anova_list[[ii]]
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
  f_res_list[["out_1"]] <- f_data_out_1
  f_res_list[["model_formula"]] <- f_model_formula
  f_res_list[["model_sum_list"]] <- f_model_sum_list
  f_res_list[["model_list"]] <- f_model_list
  f_res_list[["model_vif"]] <- f_model_vif
  write.csv(f_lm_cor, file = paste0('2301_lm_cor_s',f_order, '_buf', f_buffer, '.csv'), row.names = TRUE)
  write.csv(f_lm_slope, file = paste0('2301_lm_slope_s',f_order, '_buf', f_buffer, '.csv'), row.names = TRUE)
  write.csv(f_lm_p, file = paste0('2301_lm_p_s',f_order, '_buf', f_buffer, '.csv'), row.names = TRUE)
  write.csv(f_lm_r2, file = paste0('2301_lm_r2_s',f_order, '_buf', f_buffer, '.csv'), row.names = TRUE)
  
  return(f_res_list)
}
#==========================

res_list <- list()
orders <- c(5)  #to_be_set_key
buffers <- c(1000)  #to_be_set_key

ii <- 0
for(c_order in orders){
  ii <- ii + 1
  jj <- 0
  res_list[[c_order]] <- list()
  for(c_buffer in buffers){
    jj <- jj + 1
    res_list[[c_order]][[c_buffer]] <- cor_data_f(c_order, c_buffer) #to_be_set_key
  }
}



#==========================
order_1 <- 4 #to_be_set
buffer_1 <- 1000 #to_be_set
cor_exa_1 <- res_list[[c_order]][[buffer_1]][['out_1']] #to_be_set
