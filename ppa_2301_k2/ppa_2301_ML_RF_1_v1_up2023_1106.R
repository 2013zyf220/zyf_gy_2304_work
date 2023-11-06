library(randomForest)
library(skimr)
library(DataExplorer)
library(tidyverse)
library(caret)
library(pROC)

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/shp/outputs3')
#==============================================================

#get matrix values based on column name
col_values_f <- function(f_mat, f_col_name){
  if (f_col_name %in% colnames(f_mat)) {
    f_col_index <- which(colnames(f_mat) == f_col_name)  # Find the column index
    f_col_values <- f_mat[, f_col_index]  # Extract the column values
    return(f_col_values)
  } else {
    cat('Column name not found in the matrix.')
    return(NULL)
  }
}
#==============================================================
index_y_sel <- 'rx_rci'; #to_be_set
year <- 2021 ; #to_be_set
data_1 <- read.csv(paste0('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/shp/outputs2/2301_river_6_', year, '.csv'));
data_2 <- data_1[,c(3,7,16,18,20,22,33,34,35,48)] #to_be_set

skim(data_2) #鸟瞰数据
plot_missing(data_2) #数据缺失状况
col_name_2 <- colnames(data_2) #get the column names of data_2

col_2d3 <- c(1,2,3,4,5,6,10) #to_be_set
len_col_2d3 <- length(col_2d3)
col_name_3 <- col_name_2[col_2d3]
col_name_3

data_2y <- col_values_f(data_2, index_y_sel)
hist(data_2y, breaks = 50) #to_be_set #对因变量做直方图 

#=================================================================

#分类数据
set.seed(1)
trains <- createDataPartition(y = data_2y, p = 0.75, list = F); #to_be_set #基于因变量拆分
train_data <- data_2[trains, ]
test_data <- data_2[-trains, ]
train_data_y <- col_values_f(train_data, index_y_sel)
test_data_y <- col_values_f(test_data, index_y_sel)
hist(train_data_y, breaks = 50) #to_be_set
hist(test_data_y, breaks = 50) #to_be_set

#构建公式
form_reg <- as.formula(paste0(index_y_sel,' ~', paste(colnames(train_data)[col_2d3], collapse = ' + '))) #to_be_set #as.formula(): 将字符串转换成公式
form_reg

#modelling
set.seed(2)
fit_rf_reg <- randomForest(form_reg, data = train_data, ntree = 500, mtry = 6, importance = T, na.action = na.pass)
#ntree:决策树棵树，比较大就可以. mtry: 每个节点可供选择的变量数目，2到10之间就可以
fit_rf_reg #模型概要
plot(fit_rf_reg, main = '树的棵树与袋外MSE') # ntree参数与error之间的关系图示

# 变量重要性图示，默认最多显示30个变量
impor_1 <- importance(fit_rf_reg) #变量重要性
impor_2 <- impor_1[,'%IncMSE']
varImpPlot(fit_rf_reg, main = '随机森林变量重要性', type = 1) 

# 偏依赖图: 因变量随特定自变量变化的变化

jpeg(paste0('ppa_2301_rf_1_', year, '.jpg'), width = 800, height = 600, quality = 100)  # Adjust width, height, and quality as needed
par(mfrow = c(3, 3))
for(ii in 1: len_col_2d3){
  partialPlot(x = fit_rf_reg, pred.data = train_data, x.var = col_name_3[ii])  #to_be_set
}
dev.off()  # Close the jpeg device

#确保pred.data数据为data.frame, x.var: 某个自变量名称
jpeg(paste0('ppa_2301_rf_2_', year, '.jpg'), width = 800, height = 600, quality = 100) 
par(mfrow = c(3, 3))
for(ii in 1: len_col_2d3){
  plot(train_data_y ~ col_values_f(train_data, col_name_3[ii])) #对应散点图
}
dev.off()
#======================================
#预测

train_pred  <- predict(object = fit_rf_reg, newdata = train_data) # 训练集预测结果
defaultSummary(data = data.frame(obs = train_data_y, pred = train_pred)) # 训练集预测误差指标

plot(x = train_data_y, y = train_pred , xlab = 'Actual', ylab = 'Prediction', 
     main = '随机森林—实际值与预测值比较', sub = '训练集') # 图示训练集预测结果

train_lm <- lm(train_pred  ~ train_data_y)
abline(train_lm, col = 'blue', lwd = 2.5, lty = 'solid')
abline(a = 0, b = 1, col = 'red', lwd = 2.5, lty = 'dashed')
legend('topleft', legend = c('Model', 'Base'), col = c('blue', 'red'), lwd = 2.5, lty = c('solid','dashed'))

train_lm_sum <- summary(train_lm)
train_lm_sum$r.squared
train_lm_sum$adj.r.squared
#======================================
#训练集预测结果的说服力不足，因而需要测试集预测

test_pred <- predict(fit_rf_reg, newdata = test_data)  #测试集预测结果
defaultSummary(data.frame(obs = test_data_y, pred = test_pred))  #测试集预测误差指标

plot(x = test_data_y, y = test_pred, xlab = 'Actual', ylab = 'Prediction', 
     main = '随机森林——实际值与预测值比较', sub = '测试集') # 图示测试集预测结果
test_lm <- lm(test_pred ~ test_data_y)
abline(test_lm, col = 'blue', lwd = 2.5, lty = 'solid')
abline(a = 0, b = 1, col = 'red', lwd = 2.5, lty = 'dashed')
legend('topleft', legend = c('Model', 'Base'), col = c('blue', 'red'), lwd = 2.5, lty = c('solid','dashed'))

test_lm_sum <- summary(test_lm)
test_lm_sum$r.squared
test_lm_sum$adj.r.squared
#=================================

pred_res <- data.frame(obs = c(train_data_y, test_data_y), pred = c(train_pred , test_pred),
                         group = c(rep('Train', length(train_pred )), rep('Test', length(test_pred))))

ggplot(pred_res, aes(x = obs, y = pred, fill = group, colour = group)) +
  geom_point(shape = 21, size = 3) +
  geom_smooth(method = 'lm', se = F, size = 1.2) + 
  geom_abline(intercept = 0, slope = 1, size = 1.2) +
  labs(fill = NULL, colour = NULL) +
  theme(legend.position = 'bottom')

all_lm <- lm(pred ~ obs, data = pred_res)
all_lm_sum <- summary(all_lm)
all_lm_sum$r.squared
all_lm_sum$adj.r.squared

