library(MASS)
library(skimr)
library(DataExplorer)
library(tidyverse)
library(caret)
library(pROC)
library(ggplot2)
library(readxl)

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2302_k2')

#==============================================

load('ARCGIS/ANA1_data_1_paras.RData')
load('ARCGIS/ANA1_data_1_TIME.RData')
load('ARCGIS/ANA1_data_1_TP.RData')
load('ARCGIS/ANA1_data_1_RH.RData')
load('ARCGIS/ANA1_data_2_TP.RData')
load('ARCGIS/ANA1_data_2_RH.RData')
load('ARCGIS/ANA1_data_2_TP_MAT.RData')
load('ARCGIS/ANA1_data_2_RH_MAT.RData')
load('ARCGIS/ANA1_data_2_TP_NOR_MAT.RData')
load('ARCGIS/ANA1_data_2_RH_NOR_MAT.RData')

times_set <- data_1_paras[['times_set']]
strs_co <- data_1_paras[['strs_co']]
strs_mo <- data_1_paras[['strs_mo']]
days_set <- data_1_paras[['days_set']]
days_nor <- data_1_paras[['days_nor']]

len_times_set <- length(times_set)
len_strs_co <- length(strs_co)
len_strs_mo <- length(strs_mo)
len_days_set <- length(days_set)
len_days_nor <- length(days_nor)

index_1 <- read.csv('ARCGIS/index_1m_df.csv')[1:300,]
ele_2 <- read.csv('ARCGIS/ele_2_table.csv')
ele_2b <- matrix(ele_2, nrow = 50 * 6, ncol = 1)
index_1$ele_2 <- ele_2b
#==============================================

data_2_TP_MEAN <- list()
data_2_TP_MEANA <- matrix(0, nrow = 50 * 6, ncol = len_times_set)
data_2_TP_MEAN2 <- matrix(0, nrow = 50, ncol = len_times_set)
for(ii in times_set){
  data_2_TP_MEAN[[ii]] <- matrix(0, nrow = 50, ncol = len_strs_mo)
  for(jj in strs_mo){
    data_2_TP_MEAN[[ii]][ ,jj] <- rowMeans(data_2_TP_MAT[[ii]][[jj]])
  }
  data_2_TP_MEAN2[ ,ii] <- rowMeans(data_2_TP_MEAN[[ii]])
  data_2_TP_MEANA[ ,ii] <- matrix(data_2_TP_MEAN[[ii]], nrow = 50 * 6, ncol = 1)
}

#==============================================


dis_1 <- seq(10, 500, 10) #to_be_set
dis_2 <- rep(dis_1, 6)
plot(dis_2, data_2_TP_MEANA[,3], main = "Scatter Plot", xlab = "X", ylab = "Y")

model_1 <- list()
for(ii in 1: len_times_set){
  model_1[[ii]] <- summary(lm(data_2_TP_MEANA[,ii] ~ poly(dis_2, 3, raw = TRUE)))
}

model_1_coe_1 <- model_1[[1]]$coefficients[1,1]
model_1_coe_2 <- model_1[[1]]$coefficients[2,1]
model_1_coe_3 <- model_1[[1]]$coefficients[3,1]
model_1_coe_4 <- model_1[[1]]$coefficients[4,1]

rce_f  <- function(f_1, f_2, f_3){
  f_rcd <- (-2 * f_2 - sqrt(4 * f_2^2 - 12 * f_1 * f_3))/(6 * f_1)
  f_rci <- f_1 * f_rcd^3 + f_2 * f_rcd^2 +  f_3 * f_rcd
  f_res <- list()
  f_res[['rcd']] <- f_rcd
  f_res[['rci']] <- f_rci
  return(f_res)
}

index_1$TP_1


#三次回归
#相关分析
index_1[['TP_times_1']] <- data_2_TP_MEANA[,1]
index_1[['TP_times_2']] <- data_2_TP_MEANA[,2]
index_1[['TP_times_3']] <- data_2_TP_MEANA[,3]

#==========

cname_index_1 <- colnames(index_1)
cname_index_2 <- cname_index_1[c(1,3,4)]
lm(Y ~ X1 + X2 + X3, data = data)
