
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
setwd("E:/zyf_gn/zyf_gn_2301_data/ppa_2302_k2/DATA_PRO_1")

data_1 <- read_excel(paste0('RH_TP_NO3_1.xlsx'));
adj_1 <- 13347

adj_2 <- adj_1 + 720 - 1
data_2 <- data_1[adj_1: adj_2,]
data_2

#================
last_row <- tail(data_2, n = 1)
first_row <- data_2[1, ]
# 获取最后一行的第2列信息
info_last <- last_row[, 2]
print(first_row[,2])
print(info_last)