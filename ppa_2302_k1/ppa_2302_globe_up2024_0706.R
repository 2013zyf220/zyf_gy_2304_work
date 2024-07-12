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
varis_2 <- c('RH')  #to_be_set_key('WS','TA','RH')

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
  f_data_1 <- read.csv(paste0('RES1/GLOBE_NO', f_rou, '.csv'))
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

#======================================================================
#======================================================================
#up2024_0709_07:21

numw_site <- c(5,6,7,9)  #to_be_set_key
days_all <- c('2023/8/15', '2023/8/16','2023/8/17','2023/8/20','2023/8/22','2023/8/28')  #to_be_set
time_sa_all <- c('10:00:00', '15:00:00', '20:00:00')  #to_be_set
time_sb_all <- c('11:00:00', '16:00:00', '21:00:00')  #to_be_set
time_ea_all <- c('10:50:00', '15:50:00', '20:50:00')  #to_be_set
time_eb_all <- c('11:50:00', '16:50:00', '21:50:00')  #to_be_set

#======================================================================
#up2024_0709_08:21

get_dataw_1f <- function(f_str, f_day, f_time){
  f_rou <- str2rou_f(f_str)
  f_rou2 <- numw_site[f_rou]
  f_data_2 <- read.csv(paste0('RES1/WEATHER_285999', f_rou2, '.csv'))
  
  f_time_x1 <- f_data_2$FORMATTED.DATE_TIME
  f_time_len <- length(f_time_x1)
  f_time_x2 <- c()
  for(mm in 1: f_time_len){
    if(mm %% 5000 == 0){
      cat(mm, '\n')
    }
    f_time_x2 <- append(f_time_x2, ymd_hms(paste0(f_time_x1[mm],':00')))
  }
  
  f_data_2$time_x2 <- f_time_x2
  
  f_day_2 <- days_all[f_day]
  if(f_str %% 2 == 1){
    f_time_s1 <- time_sa_all[f_time]
    f_time_e1 <- time_ea_all[f_time]    
  }else{
    f_time_s1 <- time_sb_all[f_time]
    f_time_e1 <- time_eb_all[f_time]     
  }

  f_time_s2 <- paste(f_day_2, ' ', f_time_s1, sep = '')
  f_time_e2 <- paste(f_day_2, ' ', f_time_e1, sep = '')
  f_time_s3 <- ymd_hms(f_time_s2)
  f_time_e3 <- ymd_hms(f_time_e2)
  #====================
  
  f_data_3 <- subset(f_data_2, time_x2 >= f_time_s3 & time_x2 < f_time_e3)
  f_min_seq_1 <- seq(from = f_time_s3, to = f_time_e3, by = "min")
  f_min_seq_len <- length(f_min_seq_1) - 1
  
  #====================
  f_data_4 <- list()
  f_data_ws1 <- list()
  f_data_ws1_1 <- rep(0, f_min_seq_len)
  f_data_ws1_2 <- rep(0, f_min_seq_len)
  f_data_ta1 <- list()
  f_data_ta1_1 <- rep(0, f_min_seq_len)
  f_data_ta1_2 <- rep(0, f_min_seq_len)
  f_data_rh1 <- list()
  f_data_rh1_1 <- rep(0, f_min_seq_len)
  f_data_rh1_2 <- rep(0, f_min_seq_len)
  for(pp in 1: f_min_seq_len){
    #cat('pp:', pp, '\n')
    f_data_4[[pp]] <- subset(f_data_2, time_x2 >= f_min_seq_1[pp] & time_x2 < f_min_seq_1[pp + 1])
    f_data_ws1[[pp]] <- as.numeric(f_data_4[[pp]]$Wind.Speed) 
    f_data_ta1[[pp]] <- as.numeric(f_data_4[[pp]]$Temperature) 
    f_data_rh1[[pp]] <- as.numeric(f_data_4[[pp]]$Relative.Humidity) 
    if(length(f_data_ws1[[pp]]) > 0){
      f_data_ws1_1[pp] <- mean(f_data_ws1[[pp]])
      f_data_ws1_2[pp] <- f_data_ws1[[pp]][1]  
      f_data_ta1_1[pp] <- mean(f_data_ta1[[pp]])
      f_data_ta1_2[pp] <- f_data_ta1[[pp]][1]  
      f_data_rh1_1[pp] <- mean(f_data_rh1[[pp]])
      f_data_rh1_2[pp] <- f_data_rh1[[pp]][1]  
    }else{
      f_data_ws1_1[pp] <- NA
      f_data_ws1_2[pp] <- NA
      f_data_ta1_1[pp] <- NA
      f_data_ta1_2[pp] <- NA
      f_data_rh1_1[pp] <- NA
      f_data_rh1_2[pp] <- NA
    }
  }
 
  f_res <- list() 
  f_res[['ws1']] <- f_data_ws1
  f_res[['ws1_1']] <- f_data_ws1_1
  f_res[['ws1_2']] <- f_data_ws1_2
  f_res[['ta1']] <- f_data_ta1
  f_res[['ta1_1']] <- f_data_ta1_1
  f_res[['ta1_2']] <- f_data_ta1_2
  f_res[['rh1']] <- f_data_rh1
  f_res[['rh1_1']] <- f_data_rh1_1
  f_res[['rh1_2']] <- f_data_rh1_2
  return(f_res)
}

#============================================
#up2024_0711_08:03

ws_vari <- c('ws1_1','ws1_2')
ta_vari <- c('ta1_1','ta1_2')
rh_vari <- c('rh1_1','rh1_2')

#============================================
#up2024_0709_08:21

get_dataw_2f <- function(f_vari){
  f_vari_set <- 1 #to_be_set
  if(f_vari == 'WS'){
    f_vari1 <- ws_vari[f_vari_set]
  }else if(f_vari == 'TA'){
    f_vari1 <- ta_vari[f_vari_set]
  }else if(f_vari == 'RH'){
    f_vari1 <- rh_vari[f_vari_set]
  }else{
    print('ERROR')
  }
  
  f_d1_vari <- list()
  f_d1_vari_df <- list()  
  for(ii in times_set){
    f_d1_vari[[ii]] <- matrix(0, nrow = len_sites * len_strs_co, ncol = len_days_ori)
    for(jj in strs_co){
      f_s <- (jj - 1) * len_sites + 1
      f_e <- jj * len_sites
      for(kk in days_ori){
        print(Sys.time())
        cat('get_dataw_2f:', ii, jj, kk, '\n')
        if(jj %% 2 == 1){
          f_d1_vari[[ii]][f_s:f_e,kk] <- get_dataw_1f(jj, kk, ii)[[f_vari1]]
        }else{
          f_d1_vari[[ii]][f_s:f_e,kk] <- rev(get_dataw_1f(jj, kk, ii)[[f_vari1]])
        }
      }
    }
    
    f_d1_vari_df[[ii]] <- as.data.frame(f_d1_vari[[ii]])
    colnames(f_d1_vari_df[[ii]]) <- days_ori_name
    write.csv(f_d1_vari_df[[ii]], paste0('RES1/dataw_1_', f_vari_set, f_vari, '_time', ii,'.csv'), row.names = FALSE)
  }
  return(f_d1_vari_df)
}

#============================================
#up2024_0709_08:21

dataw_1 <- list()
for(c_vari in varis_2){
  dataw_1[[c_vari]] <- get_dataw_2f(c_vari)
}



