library(MASS)
library(gbm)
library(randomForest)
library(skimr)
library(DataExplorer)
library(tidyverse)
library(caret)
library(pROC)
library(ggplot2)
library(pdp)

setwd("E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/V2/DATA_ANA_1")

#==============================================================

#get matrix values based on column name
col_values_f <- function(f_mat, f_col_name){
  if (f_col_name %in% colnames(f_mat)){
    f_col_index <- which(colnames(f_mat) == f_col_name)  # Find the column index
    f_col_values <- f_mat[, f_col_index]  # Extract the column values
    return(f_col_values)
  }else{
    cat("Column name not found in the matrix.")
    return(NULL)
  }
}

#==============================================================

brt_f1 <- function(f_order, f_index_y, f_col_1d2, f_col_2d3, f_rows_remove){
  #输入数据&行列筛选
  f_data_1 <- read.csv(paste0("ppa_2301_ana_s", f_order, ".csv")); 
  f_data_1b <- f_data_1[, f_col_1d2];
  if(length(f_rows_remove) == 0){
    f_data_2 <- f_data_1b
  }else{
    f_data_2 <- f_data_1b[-f_rows_remove,]
  }
  
  #数据概述
  skim(f_data_2) #鸟瞰数据
  plot_missing(f_data_2) #数据缺失状况
  
  #得到因变量的值
  f_data_2y <- col_values_f(f_data_2, f_index_y)
  hist(f_data_2y, breaks = 50) #to_be_set #对因变量做直方图 
  
  #求列名
  f_col_name_2 <- colnames(f_data_2) #get the column names of f_data_2
  f_col_name_3 <- f_col_name_2[f_col_2d3]
  
  #分类数据
  set.seed(1)
  f_trains <- createDataPartition(y = f_data_2y, p = 0.75, list = F); #to_be_set #基于因变量拆分
  f_train_data <- f_data_2[f_trains, ]
  f_test_data <- f_data_2[-f_trains, ]
  f_train_data_y <- col_values_f(f_train_data, f_index_y)
  f_test_data_y <- col_values_f(f_test_data, f_index_y)
  hist(f_train_data_y, breaks = 50) #to_be_set
  hist(f_test_data_y, breaks = 50) #to_be_set
  
  #构建公式
  f_form_reg <- as.formula(paste0(f_index_y," ~ ", paste(f_col_name_3, collapse = " + ")))  #as.formula(): 将字符串转换成公式
  f_form_reg
  
  f_res_list <- list();
  f_res_list[["trains"]] <- f_trains
  f_res_list[["data_2"]] <- f_data_2
  f_res_list[["data_2y"]] <- f_data_2y
  f_res_list[["col_name_2"]] <- f_col_name_2
  f_res_list[["col_name_3"]] <- f_col_name_3
  f_res_list[["train_data"]] <- f_train_data
  f_res_list[["test_data"]] <- f_test_data
  f_res_list[["train_data_y"]] <- f_train_data_y
  f_res_list[["test_data_y"]] <- f_test_data_y
  f_res_list[["form_reg"]] <- f_form_reg
  
  return(f_res_list)
}

#=================================================================

brt_f2 <- function(f_form_reg, f_data){
  set.seed(2)
  f_fit_1 <- gbm(f_form_reg, data = f_data, verbose = TRUE, shrinkage = 0.01, interaction.depth = 3, 
                 n.minobsinnode = 5, n.trees = 5000, cv.folds = 10)
  
  f_res_list <- list();
  f_res_list[["fit_1"]] <- f_fit_1
  f_res_list[["sum_fit_1"]] <- summary(f_fit_1)
  f_res_list[["perf_gbm"]]  <- gbm.perf(f_fit_1, method = "cv")
  return(f_res_list)
}

#=================================================================

brt_pred_f <- function(f_fit_1, f_data, f_perf_gbm1, f_data_y){
  f_pred_1 <- predict(f_fit_1, f_data, f_perf_gbm1) #生成预测数据
  plot(f_data_y, f_pred_1, main = "Test dataset", xlab = "original data", ylab = "Predicted data")
  abline(1, 1)
  
  f_lm <- lm(f_pred_1 ~ f_data_y)
  f_lm_sum <- summary(f_lm)
  
  f_r2_mat <- matrix(0, nrow = 1, ncol = 3)
  f_r2_mat[1,1] <- f_lm_sum$r.squared
  f_r2_mat[1,2] <- f_lm_sum$adj.r.squared
  f_residuals <- residuals(f_lm)
  f_r2_mat[1,3] <- sqrt(mean(f_residuals^2))
  f_res_list <- list();
  f_res_list[["mat"]] <- f_r2_mat
  f_res_list[["resi"]] <- f_residuals
  f_res_list[['pred']] <- f_pred_1
  f_res_list[['data']] <- f_data_y
  return(f_res_list)
}


#=================================================================

order_1 <- 5; #to_be_set_key
indexes_y <- list("XY_crci") #to_be_set_key
col_1d2 <- c(2,3,4,9,10,13,19,22,41,42,45,47,48,49,51,52,53); #to_be_set_key
col_2d3 <- c(4,5,6,7,8,9,10,11,13,14,15,16,17); #to_be_set_key
len_col_2d3 <- length(col_2d3);
rows_remove <- c(31,32,33); #to_be_set_key

brt_f1_res <- list()
brt_f2_res_2 <- list();
for(c_index_y in indexes_y){
  brt_f1_res[[c_index_y]] <- brt_f1(order_1, c_index_y, col_1d2, col_2d3, rows_remove)
  c_brt_f1_res <- brt_f1_res[[c_index_y]]
  brt_f2_res_2[[c_index_y]] <- brt_f2(c_brt_f1_res$form_reg, c_brt_f1_res$train_data)
  c_1 <- brt_f2_res_2[[c_index_y]]
  c_brt_f2_res_sum <- c_1$sum_fit_1
  
  c_brt_pred_train <- brt_pred_f(c_1$fit_1, c_brt_f1_res$train_data, c_1$perf_gbm,  c_brt_f1_res$train_data_y)
  c_brt_pred_test <- brt_pred_f(c_1$fit_1, c_brt_f1_res$test_data, c_1$perf_gbm,  c_brt_f1_res$test_data_y)
  
  write.csv(c_brt_f2_res_sum, file = paste0("RES/2301_brt_impor_1_", order_1,'_', c_index_y, ".csv"), row.names = FALSE)
  write.csv(c_brt_pred_train$mat, file = paste0("RES/2301_brt_pred_1_train_", order_1,'_', c_index_y, ".csv"), row.names = FALSE)
  write.csv(c_brt_pred_test$mat, file = paste0("RES/2301_brt_pred_1_test_", order_1,'_', c_index_y, ".csv"), row.names = FALSE)
  write.csv(c_brt_pred_test$pred, file = paste0("RES/2301_brt_pred_1_test_data1_", order_1,'_', c_index_y, ".csv"), row.names = FALSE)
  write.csv(c_brt_pred_test$data, file = paste0("RES/2301_brt_pred_1_test_data0_", order_1,'_', c_index_y, ".csv"), row.names = FALSE)
}


#====================================================================
index_y_1 <- indexes_y[[1]]; #to_be_set
pred_var_list <- c("XL_pd", "XL_ps_imp", "XL_ai_imp",  "XL_ps_gre", "XL_ai_gre", "XB1_mean_2", "XB1_mean_1", "XB1_ratio", "dem", "slope",  "length", "BEARING", "rotate") #to_be_set
partial_f <- function(f_order_var){
  f_pred_var <- pred_var_list[f_order_var]
  jpeg(paste0("RES/ppa_2301_brt_1b_", order_1,"_", index_y_1,'_', f_pred_var, ".jpg"))
  plot.gbm(brt_f2_res_2[[index_y_1]][["fit_1"]], i.var = f_order_var)
  dev.off()  
  
  f_partial_data <- partial(brt_f2_res_2[[index_y_1]][["fit_1"]], pred.var = f_pred_var, n.trees = 5000, train = brt_f1_res[[index_y_1]]$train_data) #to_be_set
  plot(f_partial_data)
  write.csv(f_partial_data, file = paste0("RES/2301_brt_partial_", order_1, "_", index_y_1, '_', f_pred_var, ".csv"), row.names = FALSE)
  return(f_partial_data)
}

partial_res <- list()
for(ii in 1:length(pred_var_list)){
  cat('partial:',ii)
  partial_res[[ii]] <- partial_f(ii)
}
#plot.gbm(brt_f2_res_2[[index_y_1]][["fit_1"]], i.var = c(1,5)) #生成第1个和第5个变量的偏依赖图



#====================================================================
#check
#check_index_y <- "XY_crci"
#check_brt_f1_res <- brt_f1(order_1, check_index_y, col_1d2, col_2d3, rows_remove)
#skim(check_brt_f1_res[['data_2']])
#plot_missing(check_brt_f1_res[['data_2']])
#check_brt_f1_res_data_2y <- check_brt_f1_res[['data_2y']]
#hist(check_brt_f1_res_data_2y, breaks = 50)
#check_brt_f1_res_trains <- check_brt_f1_res[['trains']]
#check_brt_f1_res_train_data <- check_brt_f1_res[['train_data']]
#check_brt_f1_res_train_data_y <- check_brt_f1_res[["train_data_y"]]

#check_brt_f2_fit_1 <- brt_f2_res_2[[check_index_y]][["fit_1"]]
#check_brt_f2_sum_fit_1 <- brt_f2_res_2[[check_index_y]][["sum_fit_1"]]
#check_brt_f2_perf_gbm <- brt_f2_res_2[[check_index_y]][["perf_gbm"]]
