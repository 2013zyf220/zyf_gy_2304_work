
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

#重点输出：非线性曲线&R2&重要性
setwd("E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/test01/shp/6")

#==============================================================

#get matrix values based on column name
col_values_f <- function(f_mat, f_col_name){
  if (f_col_name %in% colnames(f_mat)) {
    f_col_index <- which(colnames(f_mat) == f_col_name)  # Find the column index
    f_col_values <- f_mat[, f_col_index]  # Extract the column values
    return(f_col_values)
  } else {
    cat("Column name not found in the matrix.")
    return(NULL)
  }
}

#==============================================================

brt_f1 <- function(f_order, f_buffer, f_index_y, f_col_1d2, f_col_2d3, f_rows_remove){
  f_data_1 <- read.csv(paste0("res2/ppa_2301_ana_s", f_order, '_buf', f_buffer,".csv")); 
  f_data_1b <- f_data_1[, f_col_1d2];
  if(length(f_rows_remove) == 0){
    f_data_2 <- f_data_1b
  }else{
    f_data_2 <- f_data_1b[-f_rows_remove,]
  }
  
  skim(f_data_2) #鸟瞰数据
  plot_missing(f_data_2) #数据缺失状况
  
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

order_1 <- 4; #to_be_set_key
buffer_1s <- c('d01s4') #to_be_set_key
indexes_y <- list("XY_rci") #to_be_set_key

col_1d2 <- c(5,8,12,14,17,21,32,45,46,49,67,68,70,71,72,73,83); #to_be_set
col_2d3 <- c(1,2,4,5,7,8,9,10,11,12,17); #to_be_set

#col_1d2 <- c(2,5,8,12,14,17,21,32,45,46,49,67,68,70,71,72,73,83,85); #to_be_set
#col_2d3 <- c(1,2,3,5,6,8,9,10,11,12,13,18,19); #to_be_set
len_col_2d3 <- length(col_2d3);
rows_remove <- c(31,32,33); #to_be_set(another option: 37,174)
brt_f1_res <- list()
brt_f2_res_2 <- list();
ii <- 1;
for(c_buffer in buffer_1s){
  brt_f1_res[[ii]] <- list()
  brt_f2_res_2[[ii]] <- list();
  for(c_index_y in indexes_y){
    c_brt_f1_res <- brt_f1(order_1, c_buffer, c_index_y, col_1d2, col_2d3, rows_remove)
    c_1 <- brt_f2(c_brt_f1_res$form_reg, c_brt_f1_res$train_data)
    brt_f1_res[[ii]][[c_index_y]] <- c_brt_f1_res
    brt_f2_res_2[[ii]][[c_index_y]] <- c_1
    c_brt_f2_res_sum <- c_1$sum_fit_1
    
    c_brt_pred_train <- brt_pred_f(c_1$fit_1, c_brt_f1_res$train_data, c_1$perf_gbm,  c_brt_f1_res$train_data_y)
    c_brt_pred_test <- brt_pred_f(c_1$fit_1, c_brt_f1_res$test_data, c_1$perf_gbm,  c_brt_f1_res$test_data_y)
    
    write.csv(c_brt_pred_train$mat, file = paste0("res3/2301_brt_pred_1_train_", order_1,'_buf', c_buffer, c_index_y, ".csv"), row.names = FALSE)
    write.csv(c_brt_pred_test$mat, file = paste0("res3/2301_brt_pred_1_test_", order_1,'_buf', c_buffer, c_index_y, ".csv"), row.names = FALSE)
    write.csv(c_brt_f2_res_sum, file = paste0("res3/2301_brt_impor_1_", order_1,'_buf', c_buffer, c_index_y, ".csv"), row.names = FALSE)
    write.csv(c_brt_pred_test$pred, file = paste0("res3/2301_brt_pred_1_test_data1_", order_1,'_buf', c_buffer, c_index_y, ".csv"), row.names = FALSE)
    write.csv(c_brt_pred_test$data, file = paste0("res3/2301_brt_pred_1_test_data0_", order_1,'_buf', c_buffer, c_index_y, ".csv"), row.names = FALSE)
  }
  ii <- ii + 1;
}

#=================================================================
buffer_1e <- 'd01s5' #to_be_set
ii_1 <- 1; #to_be_set
index_y_1 <- indexes_y[[1]]; #to_be_set

#pred_var_list <- c("XL_ps_imp", "XL_ai_imp", "XL_lsi_imp", "XL_ps_gre", "XL_ai_gre", "XL_lsi_gre", "XB1b_mean_1", "XB1b_mean_2", "XB1b_ratio", "terrain", "slope", "ndvi", "XA_RIVERW")
pred_var_list <- c("XL_ps_imp", "XL_ai_imp",  "XL_ps_gre", "XL_ai_gre", "XL_pd", "XB1b_mean_1", "XB1b_mean_2", "XB1b_ratio", "terrain", "slope", "XA_RIVERW")
order_var <- 5 #to_be_set
pred_var <- pred_var_list[order_var]

jpeg(paste0("res3/ppa_2301_brt_1b_", order_1,'_buf', buffer_1e, "_", index_y_1,'_', order_var, ".jpg"))
plot.gbm(brt_f2_res_2[[ii_1]][[index_y_1]][["fit_1"]], i.var = order_var)
dev.off()  

#plot.gbm(brt_f2_res_2[[ii_1]][[index_y_1]][["fit_1"]], i.var = c(1,5)) #生成第1个和第5个变量的偏依赖图

partial_data <- partial(brt_f2_res_2[[ii_1]][[index_y_1]][["fit_1"]], pred.var = pred_var, n.trees = 5000,train = brt_f1_res[[ii_1]][[index_y_1]]$train_data) #to_be_set
plot(partial_data)
write.csv(partial_data, file = paste0("res3/2301_brt_partial_", order_1,'_buf', buffer_1e, index_y_1, '_', pred_var, ".csv"), row.names = FALSE)
