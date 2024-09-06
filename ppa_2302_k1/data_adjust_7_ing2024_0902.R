library(MASS)
library(skimr)
library(DataExplorer)
library(tidyverse)
library(caret)
library(pROC)
library(ggplot2)
library(readxl)

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2302_k2/ARCGIS/RES3')

#===========================================

len_sites <- 50
len_stros_mo <- 6
len_c1 <- len_sites *  len_stros_mo
len_c2 <- len_c1 + 1


vari_set <- 'TP'
time_set <- 3
data_5c <- as.matrix(read.csv(paste0('Fig_z2_df_ORI_', vari_set, '_time', time_set, '_ADJB5c.csv')))

data_0 <- as.matrix(read.csv(paste0('recb_1_', vari_set, '_', time_set, '_df.csv')))
data_1 <- data_0[len_c2, ]

data_6 <- matrix(0, nrow = 300, ncol = 6)
for(mm in 1:300){
  data_6[mm,] <- data_1 + data_5c[mm,]
}

write.csv(data_6,paste0('Fig_z2_df_ORI_', vari_set, '_time', time_set, '_ADJB6.csv') )