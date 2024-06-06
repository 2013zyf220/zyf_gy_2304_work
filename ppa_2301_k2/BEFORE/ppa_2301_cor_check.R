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
#===========================================================

lm_f <- function(f_order, f_buffer, f_x_orders, f_y_orders, f_rows_remove){
  f_data_a1 <- read.csv(paste0('ppa_2301_ana_s', f_order, '_buf', f_buffer, '.csv')) 
  f_data_a2 <- f_data_a1[-f_rows_remove,]
  f_col_names <- colnames(f_data_a2)
  
  f_rows_res <- length(f_x_orders)
  f_cols_res <- length(f_y_orders)
  f_r2_res <- matrix(0, nrow = f_rows_res, ncol = f_cols_res)
  
  ii <- 0
  for(fc_x_order in f_x_orders){
    ii <- ii + 1
    jj <- 0
    for(fc_y_order in f_y_orders){
      jj <- jj + 1
      fc_data_a3 <- f_data_a2[,c(fc_x_order, 76 + fc_y_order)]
      fc_index_x <- f_col_names[fc_x_order]
      fc_model_formula <- as.formula(paste0(index_y[fc_y_order], " ~ ", fc_index_x))
      fc_model_list <- lm(fc_model_formula, data = fc_data_a3) #to_be_set
      fc_model_list_sum <- summary(fc_model_list)
      f_r2_res[ii,jj] <- fc_model_list_sum$r.squared
    }
  }
  
  colnames(f_r2_res) <- index_y
  rownames(f_r2_res) <- f_col_names[f_x_orders]
  write.csv(f_r2_res, file = paste0("2301_check_r2_s", f_order, "_buf", f_buffer,".csv"), row.names = TRUE)

  f_res_list <- list()
  f_res_list[['r2_res']] <- f_r2_res
}


#===========================================================================

index_y <- c("XY_rcd","XY_rci","XY_crci")
x_orders <- c(2:76,88) #to_be_set
y_orders <- c(1,2,3) #to_be_set
rows_remove <- 43:52 #to_be_set

order_1 <- c(1,2,3,4) #to_be_set
buffer_1 <- c(200,400,500,600,800,1000) #to_be_set
r2_res <- list()
kk <- 0
for(c_order in order_1){
  kk <- kk + 1
  mm <- 0
  r2_res[[kk]] <- list()
  for(c_buffer in buffer_1){
    mm <- mm + 1
    r2_res[[kk]][[mm]] <- lm_f(c_order, c_buffer, x_orders, y_orders, rows_remove)
  }
}