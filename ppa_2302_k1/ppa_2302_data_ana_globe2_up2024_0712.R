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
#up2024_0712_18:01

vari_1 <- 'WS' #to_be_set
times_set <- c(1,2,3) #to_be_set
len_days_ori <- 6 #to_be_set
len_strs_co <- 8 #to_be_set
len_sites <- 50 #to_be_set
days_ori_name <- c('day1', 'day2', 'day3', 'day4', 'day5', 'day6') #to_be_set

#============================================
#up2024_0712_18:00

interp_f <- function(f_time){
  f_data_res <- matrix(0, nrow = len_sites * len_strs_co, ncol = len_days_ori)
  for(kk in 1:len_days_ori){
    cat('interp_f', f_time, kk, '\n') #to_be_set
    f_day <- days_ori_name[kk]
    f_data_1 <- read.csv(paste0('RES1/dataw_1_1', vari_1, '_time', f_time, '.csv'))
    f_data_2 <- f_data_1[[f_day]]
    f_loc_na <- which(is.na(f_data_2))  # 找到NA值的位置
    f_loc_nna <- which(!is.na(f_data_2))  # 找到非NA值的位置
    if(length(f_loc_nna) < 2){
      f_data_res[kk] <- f_data_2
      print('ALL NA!\n')
    }else{
      f_data_res[kk] <- approx(f_loc_nna, f_data_2[f_loc_nna], xout = f_loc_na)$y 
    }
  }
  f_data_res_df <- as.data.frame(f_data_res)
  colnames(f_data_res_df) <- days_ori_name
  write.csv(f_data_res_df, paste0('RES1/dataw2_1_1', vari_1, '_time', f_time, '.csv'))
  return(f_data_res_df)
}

#============================================
#up2024_0712_18:00

interp_res <- list()
for(ii in times_set){
  interp_res[[ii]] <- interp_f(ii)
}
