
library(MASS)
library(gbm)
library(randomForest)
library(skimr)
library(DataExplorer)
library(tidyverse)
library(caret)
library(pROC)

#重点输出：非线性曲线&R2&重要性
setwd("E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/shp/outputs3")

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

brt_f1 <- function(f_year, f_index_y, f_col_1d2, f_col_2d3){
  f_data_1 <- read.csv(paste0("E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/shp/outputs2/2301_river_6_", f_year, ".csv")); 
  f_data_2 <- f_data_1[, f_col_1d2];
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
  
  f_r2_mat <- matrix(0, nrow = 1, ncol = 2)
  f_r2_mat[1,1] <- f_lm_sum$r.squared
  f_r2_mat[1,2] <- f_lm_sum$adj.r.squared
  
  f_res_list <- list();
  f_res_list[["mat"]] <- f_r2_mat
  return(f_res_list)
}

#=================================================================

year_s <- 2021; #to_be_set
year_e <- 2021; #to_be_set
indexes_y <- list("rx_rci") #to_be_set
col_1d2 <- c(3,7,16,18,20,22,33,34,35,48); #to_be_set
col_2d3 <- c(1,2,3,4,5,6,10); #to_be_set
len_col_2d3 <- length(col_2d3);

brt_f2_res_2 <- list();
ii <- 1;
for(c_year in year_s: year_e){
  brt_f2_res_2[[ii]] <- list();
  for(c_index_y in indexes_y){
    c_brt_f1_res <- brt_f1(c_year, c_index_y, col_1d2, col_2d3)
    c_1 <- brt_f2(c_brt_f1_res$form_reg, c_brt_f1_res$train_data)
    brt_f2_res_2[[ii]][[c_index_y]] <- c_1
    c_brt_f2_res_sum <- c_1$sum_fit_1

    c_brt_pred_train <- brt_pred_f(c_1$fit_1, c_brt_f1_res$train_data, c_1$perf_gbm,  c_brt_f1_res$train_data_y)
    c_brt_pred_test <- brt_pred_f(c_1$fit_1, c_brt_f1_res$test_data, c_1$perf_gbm,  c_brt_f1_res$test_data_y)
    
    write.csv(c_brt_pred_train$mat, file = paste0("2301_brt_pred_1_train_", c_year, "_", c_index_y, ".csv"), row.names = FALSE)
    write.csv(c_brt_pred_test$mat, file = paste0("2301_brt_pred_1_test_", c_year, "_", c_index_y, ".csv"), row.names = FALSE)
    write.csv(c_brt_f2_res_sum, file = paste0("2301_brt_impor_1_", c_year, "_", c_index_y, ".csv"), row.names = FALSE)
  }
  ii <- ii + 1;
}

#=================================================================

year_1 <- 2021; #to_be_set
ii_1 <- 1; #to_be_set
index_y_1 <- indexes_y[[1]]; #to_be_set

jpeg(paste0("ppa_2301_brt_1b_", year_1, "_", 1,".jpg"))
plot.gbm(brt_f2_res_2[[ii_1]][[index_y_1]][["fit_1"]], i.var = 1)
dev.off()  

#plot.gbm(brt_f2_res_2[[ii_1]][[index_y_1]][["fit_1"]], i.var = c(1,5)) #生成第1个和第5个变量的偏依赖图
