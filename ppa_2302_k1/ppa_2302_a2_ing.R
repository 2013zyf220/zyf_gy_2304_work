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

rce_f  <- function(f_1, f_2, f_3){
  f_rcd <- (-2 * f_2 - sqrt(4 * f_2^2 - 12 * f_1 * f_3))/(6 * f_1)
  f_rci <- f_1 * f_rcd^3 + f_2 * f_rcd^2 +  f_3 * f_rcd
  f_res <- list()
  f_res[['rcd']] <- f_rcd
  f_res[['rci']] <- f_rci
  return(f_res)
}

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

len_sites <- 50 #to_be_set

index_1 <- read.csv('ARCGIS/index_1m_df.csv')[1:300,]
ele_2 <- read.csv('ARCGIS/ele_2_table.csv')
ele_2b <-as.matrix(ele_2)
ele_2c <- matrix(ele_2b, nrow = 50 * 6, ncol = 1)
index_1$ele_2 <- ele_2c

#==============================================

data_2_TP_MEAN <- list()
data_2_TP_MEAN2 <- matrix(0, nrow = len_sites, ncol = len_times_set)
data_2_TP_MEAN3 <- matrix(0, nrow = len_sites * len_strs_mo, ncol = len_times_set)
for(ii in times_set){
  data_2_TP_MEAN[[ii]] <- matrix(0, nrow = len_sites, ncol = len_strs_mo)
  for(jj in strs_mo){
    data_2_TP_MEAN[[ii]][ ,jj] <- rowMeans(data_2_TP_MAT[[ii]][[jj]])
  }
  data_2_TP_MEAN2[ ,ii] <- rowMeans(data_2_TP_MEAN[[ii]])
  data_2_TP_MEAN3[ ,ii] <- as.vector(data_2_TP_MEAN[[ii]])
}

#==============================================

dis_1 <- seq(0, 490, 10) #to_be_set
dis_2 <- rep(dis_1, len_strs_mo)

#plot(dis_2, data_2_TP_MEAN3[ ,1], main = 'Scatter Plot', xlab = 'X', ylab = 'Y') #to_be_set
plot(dis_1, data_2_TP_MEAN2[ ,1], main = 'Scatter Plot', xlab = 'X', ylab = 'Y') #to_be_set
#==============================================

modela_1 <- list()
modela_2 <- list()
modela_coe <- matrix(0, nrow = 4, ncol = len_times_set)
modela_rci_1 <- rep(0, len_times_set)
modela_rcd_1 <- rep(0, len_times_set)
for(ii in 1: len_times_set){
  modela_1[[ii]] <- lm(data_2_TP_MEAN3[,ii] ~ poly(dis_2, 3, raw = TRUE))
  modela_2[[ii]] <- summary(modela_1[[ii]])
  modela_coe[1,ii] <- modela_2[[ii]]$coefficients[1,1]
  modela_coe[2,ii] <- modela_2[[ii]]$coefficients[2,1]
  modela_coe[3,ii] <- modela_2[[ii]]$coefficients[3,1]
  modela_coe[4,ii] <- modela_2[[ii]]$coefficients[4,1]
  modela_rci_1[ii] <- rce_f(modela_coe[2,ii], modela_coe[3,ii], modela_coe[4,ii])[['rci']]
  modela_rcd_1[ii] <- rce_f(modela_coe[2,ii], modela_coe[3,ii], modela_coe[4,ii])[['rcd']]
}

#==============================================

cname_index_1 <- colnames(index_1)
cname_index_2 <- cname_index_1[c(1,3,4)] #to_be_set
cname_index_3 <- paste(cname_index_2, collapse = ' + ')

for(ii in 1: len_times_set){
  c_1 <- paste0('TP_times_', ii)
  index_1[[c_1]] <- data_2_TP_MEAN3[,ii]
}

modelb_for <- list()
for(ii in 1: len_times_set){
  c_1 <- paste0('TP_times_', ii)
  modelb_for[[ii]] <- lm(as.formula(paste0(c_1, ' ~ ', cname_index_3)), data = index_1)
}

#==============================================
data_2_TP_MEAN2_df <- as.data.frame(data_2_TP_MEAN2)
data_2_TP_MEAN3_df <- as.data.frame(data_2_TP_MEAN3)
colnames(data_2_TP_MEAN2_df) <- times_set
colnames(data_2_TP_MEAN3_df) <- times_set
write.csv(data_2_TP_MEAN2_df, "data_2_TP_MEAN2.csv", row.names = FALSE)
write.csv(data_2_TP_MEAN3_df, "data_2_TP_MEAN3.csv", row.names = FALSE)

data_2_TP_MEAN_df <- list()
for(ii in times_set){
  data_2_TP_MEAN_df[[ii]] <- as.data.frame(data_2_TP_MEAN[[ii]])
  colnames(data_2_TP_MEAN_df[[ii]]) <- times_set
  write.csv(data_2_TP_MEAN_df[[ii]], "data_2_TP_MEAN_times_", ii, ".csv", row.names = FALSE)
}

write.csv(dis_1, "dis_1.csv", row.names = FALSE)
write.csv(dis_2, "dis_2.csv", row.names = FALSE)
