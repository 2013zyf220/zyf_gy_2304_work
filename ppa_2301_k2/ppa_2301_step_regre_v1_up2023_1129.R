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
setwd("E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/shp/3")

index_x <- list("XA_ANGLE_2", "XL_ps_imp", "XL_co_imp", "XL_ai_imp", "XL_ps_gre", "XL_pd", "XL_lsi", "XB1a_mean_1", "XT_DEM_MEA", "XT_SLOPE_M", "XT_DIS_CEN", "XN_NDVI_ME")    #to_be_set
index_y <- list("XY_rcd", "XY_rci", "XY_crci")    #to_be_set
index_y_set <- "XY_rci" #to_be_set
row_1 <- length(index_x)
col_1 <- length(index_y)

sel_x_1 <- c(37,174)  #to_be_set
sel_y_1 <- c(2,5,6,8,9,17,18,21,49,51,53,56,57,58,59)  #to_be_set

step_reg_f <- function(f_index_y_set, f_order, f_buffer, f_x_sel, f_y_sel){
  f_data_1 <- read.csv(paste0('ppa_2301_ana_s', f_order, '_buf', f_buffer, '.csv')) 
  f_data_1b <- f_data_1[, sel_y_1]    #to_be_set
  f_data_1c <- f_data_1b[f_data_1b$XY_rci != 0, ] #to_be_set
  f_data_2 <- f_data_1c[-sel_x_1, ] #to_be_set
  
  f_model_formula <- as.formula(paste0(f_index_y_set, " ~ ", paste(index_x, collapse = " + ")));
  f_model_list <- lm(f_model_formula, data = f_data_2) #to_be_set
  f_sum_model_list <- summary(f_model_list)
  
  f_step_model <- step(f_model_list)
  f_drop1_model <- drop1(f_model_list)
  
  f_pred_1 <- predict(f_step_model, newdata = f_data_2)  # Make predictions on the training data
  f_resi_1 <- (f_data_2[[f_index_y_set]] - f_pred_1)^2  # Calculate rmse
  f_rmse_1 <- sqrt(mean(f_resi_1))
  f_r2_1 <- f_sum_model_list$r.squared
  f_r2adj_1 <- f_sum_model_list$adj.r.squared
  f_equ_1 <- f_step_model$call
  
  f_relimp_1 <- calc.relimp(f_step_model, type = "lmg")
  
  f_res_list <- list()
  f_res_list[['r2']] <- f_r2_1
  f_res_list[['rmse']] <- f_rmse_1
  f_res_list[['r2adj']] <- f_r2adj_1
  f_res_list[['equ']] <- f_equ_1
  f_res_list[['relimp']] <- f_relimp_1
  return(f_res_list)
}

#============================================================
res_list <- list()

orders <- c(1,2,3,4) #to_be_set
buffers <- c(200,400,600,800,1000) #to_be_set

r2_out <- matrix(0, nrow = length(orders), ncol = length(buffers))
r2adj_out <- matrix(0, nrow = length(orders), ncol = length(buffers))
rmse_out <- matrix(0, nrow = length(orders), ncol = length(buffers))

ii <- 0
for(c_order in orders){
  ii <- ii + 1
  jj <- 0
  res_list[[ii]] <- list()
  for(c_buffer in buffers){
    jj <- jj + 1
    res_list[[ii]][[jj]] <- step_reg_f(index_y_set, c_order, c_buffer, sel_x_1, sel_y_1) #to_be_set
    r2_out[ii,jj] <- res_list[[ii]][[jj]][['r2']]
    r2adj_out[ii,jj] <- res_list[[ii]][[jj]][['r2adj']]
    rmse_out[ii,jj] <- res_list[[ii]][[jj]][['rmse']]
  }
}

write.csv(r2_out, file = paste0('shp/3/step_ana_r2_', index_y_set, '.csv'), row.names = FALSE)
write.csv(r2adj_out, file = paste0('shp/3/step_ana_r2adj_', index_y_set, '.csv'), row.names = FALSE)
write.csv(rmse_out, file = paste0('shp/3/step_ana_r2adj_', index_y_set, '.csv'), row.names = FALSE)