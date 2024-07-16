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
#up2024_0716_09:00

setas <- c('recg','recw')  #to_be_set_key

varis <- list()
varis[['recg']] <- c('TA','RH','TG','WBGT') #to_be_set
varis[['recw']] <- c('WS') #to_be_set
vari_set_2 <- 1 #to_be_set
times_set <- c(1,2,3) #to_be_set
len_days_ori <- 6 #to_be_set
len_strs_co <- 8 #to_be_set
len_sites <- 50 #to_be_set
days_ori_name <- c('day1', 'day2', 'day3', 'day4', 'day5', 'day6') #to_be_set

#============================================
#up2024_0716_09:00

interp_f <- function(f_time, f_vari, f_seta){
  if(f_seta == 'recw'){
    f_data_1 <- read.csv(paste0('RES2/recw_1_', vari_set_2, f_vari, '_time', f_time, '.csv'))
  }else if(f_seta == 'recg'){
    f_data_1 <- read.csv(paste0('RES2/recg_1_', f_vari, '_time', f_time, '.csv'))
    f_data_1[f_data_1 == -9999] <- NA
  }else{
    print('ERROR')
  }
  f_data_res_2 <- matrix(0, nrow = len_sites * len_strs_co, ncol = len_days_ori)
  for(kk in 1:len_days_ori){
    cat('interp_f', f_time, '_', kk, '\n') #to_be_set
    f_day <- days_ori_name[kk]
    f_data_2 <- f_data_1[[f_day]]
    f_loc_na <- which(is.na(f_data_2))  # 找到NA值的位置
    f_loc_nna <- which(!is.na(f_data_2))  # 找到非NA值的位置
    
    if(length(f_loc_nna) < 2){
      f_data_res_2[,kk] <- f_data_2
      print('ALL NA!\n')
    }else{
      f_data_res_1 <- approx(f_loc_nna, f_data_2[f_loc_nna], xout = f_loc_na)$y 
      f_data_res_2[,kk] <- f_data_2
      f_data_res_2[,kk][f_loc_na] <- f_data_res_1
    }
  }
  f_data_res_2b <- round(f_data_res_2, 2) #to_be_set
  f_data_res_df <- as.data.frame(f_data_res_2b)
  colnames(f_data_res_df) <- days_ori_name
  if(f_seta == 'recw'){
    write.csv(f_data_res_df, paste0('RES2/recw2_1_', vari_set_2, f_vari, '_time', f_time, '.csv'), row.names = FALSE)
  }else if(f_seta == 'recg'){
    write.csv(f_data_res_df, paste0('RES2/recg2_1_', f_vari, '_time', f_time, '.csv'), row.names = FALSE)
  }else{
    print('ERROR')
  }
  return(f_data_res_df)
}

#============================================
#up2024_0716_09:00

interp_res <- list()
for(c_seta in setas){
  interp_res[[c_seta]] <- list()
  for(c_vari in varis[[c_seta]]){
    interp_res[[c_seta]][[c_vari]] <- list()
    for(ii in times_set){
      interp_res[[c_seta]][[c_vari]][[ii]] <- interp_f(ii, c_vari, c_seta)
    }
  }
}

