library(MASS)
library(skimr)
library(DataExplorer)
library(tidyverse)
library(caret)
library(pROC)
library(ggplot2)
library(readxl)

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2302_k2/ARCGIS')

#============================================
#up2024_0709_07:00

str2rou_f <- function(f_str){
  f_rou <- ceiling(f_str/2)
  return(f_rou)
}

#============================================
#up2024_0709_07:00

times_set <- c(1,2,3) #to_be_set_key
days_ori <- c(1,2,3,4,5,6)  #to_be_set_key
days_nor <- c(1,2,3,5)  #to_be_set_key(number inside days_ori)
days_hot <- c(4,6)  #to_be_set_key(number inside days_ori)
strs_co <- c(1,2,3,4,5,6,7,8)  #to_be_set_key
strs_mo <- c(1,2,3,4,5,6)  #to_be_set_key
varis_1 <- c('Date', 'Time', 'TA','RH','TG','WBGT')  #to_be_set_key
varis_2 <- c('WS')  #to_be_set_key('WS','TA','RH')

len_days_ori <- length(days_ori)
len_days_nor <- length(days_nor)
len_days_hot <- length(days_hot)
len_strs_co <- length(strs_co)
len_strs_mo <- length(strs_mo)
len_varis_1 <- length(varis_1)
len_sites <- 50 #to_be_set

days_ori_name <- c('day1', 'day2', 'day3', 'day4', 'day5', 'day6')

#============================================
#up2024_0709_07:06

cal_globe_1f <- function(f_vari, f_time, f_str, f_day){
  f_rou <- str2rou_f(f_str)
  f_data_1 <- read.csv(paste0('ORI_GLOBE/GLOBE_NO', f_rou, '.csv'))
  if(f_str%%2 == 1){
    f_s1 <- (f_day - 1) * 360 + (f_time - 1) * 120 + 1
    f_s2 <- f_s1 + len_sites - 1
    f_data_2 <- f_data_1[[f_vari]][f_s1: f_s2]
  }else{
    f_s1 <- (f_day - 1) * 360 + (f_time - 1) * 120 + 61
    f_s2 <- f_s1 + len_sites - 1
    f_data_2 <- rev(f_data_1[[f_vari]][f_s1: f_s2])
  }
  
  return(f_data_2)
}

#============================================
#up2024_0709_07:21

cal_globe_2f <- function(f_vari){
  f_data_1 <- list()
  f_data_1_df <- list()
  
  for(ii in times_set){
    f_data_1[[ii]] <- matrix(0, nrow = len_sites * len_strs_co, ncol = len_days_ori)
    for(jj in strs_co){
      f_s <- (jj - 1) * len_sites + 1
      f_e <- jj * len_sites
      for(kk in days_ori){
        f_data_1[[ii]][f_s:f_e,kk] <- cal_globe_1f(f_vari, ii, jj, kk)
      }
    }
    f_data_1_df[[ii]] <- as.data.frame(f_data_1[[ii]])
    colnames(f_data_1_df[[ii]]) <- days_ori_name
    write.csv(f_data_1_df[[ii]], paste0('RES1/datag_1_', f_vari, '_time', ii,'.csv'), row.names = FALSE)
  }
  return(f_data_1_df)
}

#=========================================
#up2024_0709_07:21

cal_globe_res_1 <- list()
for(c_vari in varis_1){
  cal_globe_res_1[[c_vari]] <- cal_globe_2f(c_vari)
}


