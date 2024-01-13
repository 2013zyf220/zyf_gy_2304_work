library(randomForest)
library(skimr)
library(DataExplorer)
library(tidyverse)
library(caret)
library(pROC)

#重点输出：非线性曲线&R2&重要性
setwd("E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/shp/4/res2")

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

rf_f1 <- function(f_order, f_buffer, f_index_y, f_col_1d2, f_col_2d3, f_rows_remove){
  f_data_1 <- read.csv(paste0("ppa_2301_ana_s", f_order, '_buf', f_buffer,".csv"));
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

#==============================================================

#modelling
rf_f2 <- function(f_form_reg, f_data, f_ntree, f_mtry){
  set.seed(2)
  f_fit_rf_reg <- randomForest(f_form_reg, data = f_data, ntree = f_ntree, mtry = f_mtry, importance = T, na.action = na.pass)
  #ntree:决策树棵树，比较大就可以. mtry: 每个节点可供选择的变量数目，2到10之间就可以
  
  f_fit_rf_reg #模型概要
  plot(f_fit_rf_reg, main = "树的棵树与袋外MSE") # ntree参数与error之间的关系图示
  return(f_fit_rf_reg)
}

#==============================================================

rf_impor_f <- function(f_fit_rf_reg){
  f_impor_1 <- importance(f_fit_rf_reg) #变量重要性
  f_impor_2 <- f_impor_1[,"%IncMSE"]
  # 变量重要性图示，默认最多显示30个变量
  varImpPlot(f_fit_rf_reg, main = "随机森林变量重要性", type = 1); #to_be_set
  return(f_impor_2)
}

#==============================================================

rf_pred1 <- function(f_fit_rf_reg, f_data, f_data_y){
  f_pred  <- predict(object = f_fit_rf_reg, newdata = f_data) # 训练集预测结果
  defaultSummary(data = data.frame(obs = f_data_y, pred = f_pred)) # 训练集预测误差指标
  
  plot(x = f_data_y, y = f_pred, xlab = "Actual", ylab = "Prediction", 
       main = "随机森林—实际值与预测值比较", sub = "训练集") # 图示训练集预测结果
  
  f_lm <- lm(f_pred  ~ f_data_y)
  f_lm_sum <- summary(f_lm)
  
  abline(f_lm, col = "blue", lwd = 2.5, lty = "solid")
  abline(a = 0, b = 1, col = "red", lwd = 2.5, lty = "dashed")
  legend("topleft", legend = c("Model", "Base"), col = c("blue", "red"), lwd = 2.5, lty = c("solid","dashed"))
  
  f_r2_mat <- matrix(0, nrow = 1, ncol = 2)
  f_r2_mat[1,1] <- f_lm_sum$r.squared;
  f_r2_mat[1,2] <- f_lm_sum$adj.r.squared;
  
  f_res_list <- list(); 
  f_res_list[["predict"]] <- f_pred;
  f_res_list[["mat"]] <- f_r2_mat;
  
  return(f_res_list)
}

#==============================================================
rf_pred2 <- function(f_train_pred, f_test_pred, f_train_data_y, f_test_data_y){
  f_pred_res <- data.frame(obs = c(f_train_data_y, f_test_data_y), pred = c(f_train_pred , f_test_pred),
                         group = c(rep("Train", length(f_train_pred)), rep("Test", length(f_test_pred))))
  
  ggplot(f_pred_res, aes(x = obs, y = pred, fill = group, colour = group)) +
    geom_point(shape = 21, size = 3) +
    geom_smooth(method = "lm", se = F, linewidth = 1.2) + 
    geom_abline(intercept = 0, slope = 1, linewidth = 1.2) +
    labs(fill = NULL, colour = NULL) +
    theme(legend.position = "bottom")
  
  f_all_lm <- lm(pred ~ obs, data = f_pred_res)
  f_all_lm_sum <- summary(f_all_lm)
  
  f_r2_mat <- matrix(0, nrow = 1, ncol = 2)
  f_r2_mat[1,1] <- f_all_lm_sum$r.squared
  f_r2_mat[1,2] <- f_all_lm_sum$adj.r.squared
  
  f_res_list <- list();
  f_res_list[["mat"]] <- f_r2_mat

  return(f_res_list)
}
#==============================================================

mar_eff <- function(f_x, f_y){
  len_data <- length(f_x)
  f_me <- rep(0, len_data)
  for(ii in 2:len_data){
    f_me[ii] <- (f_y[ii] - f_y[ii - 1])/(f_x[ii] - f_x[ii - 1])
  }
  plot(f_me, type = "o")
}

#==============================================================
order_1 <- 4; #to_be_set_key
buffer_1s <- c(200,400,600,800,1000); #to_be_set_key
col_1d2 <- c(5,6,8,12,14,15,17,21,45,46,47,49,69,71,77,78,79,90); #to_be_set
col_2d3 <- c(1,2,3,4,6,8,9,10,12,13,14,18); #to_be_set

indexes_y <- list("XY_rci"); #to_be_set
rows_remove <- c(); #to_be_set
len_indexes_y <- length(indexes_y)
len_col_2d3 <- length(col_2d3);

for(c_buffer in buffer_1s){
  for(c_index_y in indexes_y){
    c_rf_f1_res <- rf_f1(order_1, c_buffer, c_index_y, col_1d2, col_2d3, rows_remove) #to_be_set
    c_rf_f2_res <- rf_f2(c_rf_f1_res$form_reg, c_rf_f1_res$train_data, 500, 6); #to_be_set
    c_rf_impor_res <- rf_impor_f(c_rf_f2_res)
    
    for(ii in 1: len_col_2d3){
      jpeg(paste0("ppa_2301_rf_1_s", order_1, "_buf", c_buffer, "_y_" , c_index_y, "_", ii,".jpg"), width = 800, height = 600, quality = 100)  # Adjust width, height, and quality as needed
      c_pp_data <- partialPlot(x = c_rf_f2_res, pred.data = c_rf_f1_res$train_data, x.var = c_rf_f1_res$col_name_3[ii])  #to_be_set
      mar_eff(c_pp_data$x, c_pp_data$y)
      dev.off()  # Close the jpeg device
    }
    
    jpeg(paste0("ppa_2301_rf_1_s", order_1, "_buf", c_buffer, "_y_" , c_index_y,".jpg"), width = 800, height = 600, quality = 100)  # Adjust width, height, and quality as needed
    par(mfrow = c(4, 4))
    for(ii in 1: len_col_2d3){
      partialPlot(x = c_rf_f2_res, pred.data = c_rf_f1_res$train_data, x.var = c_rf_f1_res$col_name_3[ii])  #to_be_set
    }
    dev.off()  # Close the jpeg device
    
    jpeg(paste0("ppa_2301_rf_2_", order_1, "_buf", c_buffer, "_y_" , c_index_y, ".jpg"), width = 800, height = 600, quality = 100)
    par(mfrow = c(4, 4))
    for(ii in 1: len_col_2d3){
      plot(c_rf_f1_res$train_data_y ~ col_values_f(c_rf_f1_res$train_data, c_rf_f1_res$col_name_3[ii])) #对应散点图
    }
    dev.off()  # Close the jpeg device
  
    c_rf_pred_train <- rf_pred1(c_rf_f2_res, c_rf_f1_res$train_data, c_rf_f1_res$train_data_y)
    c_rf_pred_test <- rf_pred1(c_rf_f2_res, c_rf_f1_res$test_data, c_rf_f1_res$test_data_y)
    c_rf_pred_all <- rf_pred2(c_rf_pred_train$predict, c_rf_pred_test$predict, c_rf_f1_res$train_data_y, c_rf_f1_res$test_data_y)
  
    write.csv(c_rf_pred_train[["mat"]], file = paste0("2301_rf_pred_1_train_s", order_1,'_buf', c_buffer, c_index_y, ".csv"), row.names = TRUE)
    write.csv(c_rf_pred_test[["mat"]], file = paste0("2301_rf_pred_1_test_s", order_1,'_buf', c_buffer, c_index_y, ".csv"), row.names = TRUE)
    write.csv(c_rf_pred_all[["mat"]], file = paste0("2301_rf_pred_2_all_s", order_1,'_buf', c_buffer, c_index_y, ".csv"), row.names = TRUE)
    write.csv(c_rf_impor_res, file = paste0("2301_rf_impor_1_s", order_1,'_buf', c_buffer, c_index_y, ".csv"), row.names = TRUE)
  }
}


