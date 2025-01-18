library(MASS)
library(skimr)
library(DataExplorer)
library(tidyverse)
library(caret)
library(pROC)
library(ggplot2)
library(readxl)
library(gridExtra)

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2302_k2/ARCGIS')

#分2个距离段进行线性相关分析（对气象变量）
#=====================================
#up2024 1126 14:55

version_set <- 13   #to_be_set
STRS_names2 <- list()
STRS_names2[[1]] <- c('STR1_TP','STR2_TP','STR3_TP','STR4_TP','STR5_TP','STR6_TP')
STRS_names2[[2]] <- c('STR1_RH','STR2_RH','STR3_RH','STR4_RH','STR5_RH','STR6_RH')
STRS_names2[[3]] <- c('STR1_DI','STR2_DI','STR3_DI','STR4_DI','STR5_DI','STR6_DI')

times_set <- c(2,3) #to_be_set
varis <- c('TP','RH','DI') #to_be_set
len_varis <- length(varis)

range_1 <- c('DO1:DT51','DV1:EA51','EC1:EH51')

index_1b <- read.csv('RES4/index_1b.csv')
indep_1 <- c('SVF', 'XL_ps_bld', 'XL_ps_veg','bh_4_mean','dis','str_wid')
index_2 <- index_1b[, indep_1] #to_be_set
len_indep_1 <- length(indep_1)

#=====================================
#up2024 1126 15:16

data_1 <- list()
data_1m <- list()
for(ii in times_set){
  data_1[[ii]] <- list()
  data_1m[[ii]] <- list()
  for(jj in 1:len_varis){
    c_vari <- varis[jj]
    data_1[[ii]][[c_vari]] <- read_excel(paste0('RES3/PREPARE/PROCESS_2/V', version_set, '/REVISE2d1_Fig_z2_df_ORI_time', ii, '_V', version_set, '.xlsx'), sheet = 'Sheet3', range = range_1[[jj]])
    data_1m[[ii]][[c_vari]] <- as.vector(as.matrix(data_1[[ii]][[c_vari]]))
    c_name <- paste0('time', ii, '_', c_vari)
    index_2[[c_name]] <- data_1m[[ii]][[c_vari]]
  }
}

#=====================================
#up2024 1126 15:22

dis_sum_1 <- c()
dis_sum_2 <- c()

for (c_start in seq(1, 251, by = 50)){
  dis_sum_1 <- c(dis_sum_1, c_start:(c_start + 24))
}

for (c_start in seq(26, 276, by = 50)){
  dis_sum_2 <- c(dis_sum_2, c_start:(c_start + 24))
}

dis_sum <- list()
dis_sum[[1]] <- dis_sum_1
dis_sum[[2]] <- dis_sum_2

index_2_d1 <- index_2[dis_sum_1,]
index_2_d2 <- index_2[dis_sum_2,]

#=====================================
#up2024 1127 00:38

lm_f1 <- function(f_time, f_vari, f_dis, f_indep){
  f_name <- paste0('time', f_time, '_', f_vari)
  f_dis2 <- dis_sum[[f_dis]]
  f_data_1 <- index_2[f_dis2,]
  f_lm_model <- lm(as.formula(paste(f_name, ' ~ ', f_indep)), data = f_data_1) 
  
  f_res <- list()
  f_res[['R2']] <- summary(f_lm_model)$r.squared  
  
  return(f_res)
}

#=====================================
#up2024 1127 00:50

res_r2_1 <- list()
for(c_time in times_set){
  res_r2_1[[c_time]] <- list()
  for(ii in 1: len_varis){
    c_vari <- varis[ii]
    res_r2_1[[c_time]][[ii]] <- matrix(0, nrow = 2, ncol = len_indep_1)
    for(c_dis in 1:2){
      for(c_indepn in 1: len_indep_1){
        c_indep <- indep_1[c_indepn]
        #cat(c_time, c_vari, c_dis, c_indep, '\n')
        res_r2_1[[c_time]][[ii]][c_dis, c_indepn] <- lm_f1(c_time, c_vari, c_dis, c_indep)[['R2']]
      }
    }
  }
}


