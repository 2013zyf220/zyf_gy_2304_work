library(randomForest)
library(skimr)
library(DataExplorer)
library(tidyverse)
library(caret)
library(pROC)

f_order = 3 #to_be_set
f_buffer= 1000 #to_be_set
col_1d2 <- c(2,5,6,12,14,15,30,32,33,34,46,47,48,49,51,70,71,72,77,78,79,80,89); #to_be_set
col_2d3 <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,23); #to_be_set

setwd("E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/shp/3")
data_1 <- read.csv(paste0("ppa_2301_anax_s", f_order, '_buf', f_buffer,".csv"));
data_1b <- data_1[, col_1d2];
data_2 <- data_1b
#data_2 <-  read.csv('E:/zyf_gy/zyf_gy_2304_work/tool_codes_k1/R_lesson_machine_learning_2023a/randon_forest_2/data-Boston4Reg.csv')
skim(data_2) #鸟瞰数据
plot_missing(data_2) #数据缺失状况

trains <- createDataPartition(y = data_2$XY_crci, p = 0.75, list = F); #基于因变量拆分
traindata <- data_2[trains, ]
testdata <- data_2[-trains, ]
hist(traindata$XY_crci, breaks = 50)
hist(testdata$XY_crci, breaks = 50)
colnames(data_2) #get the column names of boston

form_reg <- as.formula(paste0('XY_crci ~', paste(colnames(traindata)[1:13], collapse = ' + '))) #as.formula(): 将字符串转换成公式
form_reg
fit_rf_reg <- randomForest(form_reg, data = traindata, ntree = 500, mtry = 6, importance = T) 
fit_rf_reg
importance(fit_rf_reg) #变量重要性

testpred <- predict(fit_rf_reg, newdata = testdata)  #测试集预测结果
defaultSummary(data.frame(obs = testdata$XY_crci, pred = testpred))  #测试集预测误差指标
