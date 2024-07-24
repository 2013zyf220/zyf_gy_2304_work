library(MASS)
library(skimr)
library(DataExplorer)
library(tidyverse)
library(caret)
library(pROC)
library(ggplot2)
library(readxl)

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2302_k2/ARCGIS')

#==================
#up2024_0712_22:03
paras <- c('rec', 'recg2', 'recw2') #to_be_set_key

len_sites <- 50 #to_be_set
len_strs_co <- 8  #to_be_set
len_days_ori <- 6 #to_be_set
times_set <- c(1,2,3) #to_be_set

#==================
#up2024_0712_22:03

date_1 <- c('15.8.2023', '16.8.2023', '17.8.2023', '20.8.2023', '22.8.2023', '28.8.2023')  #to_be_set
date_2 <- list()
date_3 <- c()

for(mm in 1:len_days_ori)
  date_2[[mm]] <- rep(date_1[mm], len_sites * len_strs_co) #to_be_set
  date_3 <- append(date_3, date_2[[mm]])
  
#==================
#up2024_0712_22:05

time_a <- c('10:00','15:00','20:00')
time_b <- c('11:00','16:00','21:00')

get_time_f <- function(f_time){
  f_time_a2 <- rep(time_a[f_time], len_sites)
  f_time_b2 <- rep(time_b[f_time], len_sites)
  f_time_3 <- c(f_time_a2, f_time_b2)
  f_time_4 <- rep(f_time_3, len_strs_co * len_days_ori * 0.5)
  return(f_time_4)
}

time_sum <- list()
for(ii in times_set){
  time_sum[[ii]] <- get_time_f(ii)
}

#==================
#up2024_0712_22:06

lon_1 <- round(106 + 35/60, 2)  #to_be_set
lat_1 <- round(29 + 43/60, 2) #to_be_set
ele_1 <- 432  #to_be_set
timez_1 <- 8.0 #to_be_set

lon_2 <- rep(lon_1, len_sites * len_strs_co * len_days_ori)
lat_2 <- rep(lat_1, len_sites * len_strs_co * len_days_ori)
ele_2 <- rep(ele_1, len_sites * len_strs_co * len_days_ori)
timez_2 <- rep(timez_1, len_sites * len_strs_co * len_days_ori)

#==================
#up2024_0712_22:06

time2_1 <- list()
time2_2 <- list()
for(ii in times_set){
  time2_1[[ii]] <- as.matrix(read.csv(paste0('RES2/rec_1_TIME_time', ii, '.csv')))
  time2_2[[ii]] <- as.vector(time2_1[[ii]])
  write.csv(time2_2[[ii]], paste0('RES2/rayman_time2_1_time', ii, '.csv'))
}

#==================
#up2024_0724_11:00

get_tp_f <- function(f_para){
  f_tp_1 <- list()
  f_tp_2 <- list()
  for(ii in times_set){
    f_tp_1[[ii]] <- as.matrix(read.csv(paste0('RES2/', f_para, '_1_TP_time', ii, '.csv'))) #to_be_set_key
    f_tp_2[[ii]] <- as.vector(f_tp_1[[ii]])
  }
  return(f_tp_2)
}

tp_data <- list()
for(c_para in paras){
  tp_data[[c_para]] <- get_tp_f(c_para)
}

#==================
#up2024_0724_11:00

get_rh_f <- function(f_para){
  f_rh_1 <- list()
  f_rh_2 <- list()
  for(ii in times_set){
    f_rh_1[[ii]] <- as.matrix(read.csv(paste0('RES2/', f_para, '_1_RH_time', ii, '.csv'))) #to_be_set_key
    f_rh_2[[ii]] <- as.vector(f_rh_1[[ii]])
  }
  return(f_rh_2)
}

rh_data <- list()
for(c_para in paras){
  rh_data[[c_para]] <- get_rh_f(c_para)
}


#==================
#up2024_0724_11:00

get_ws_f <- function(f_para){
  f_ws_1 <- list()
  f_ws_2 <- list()
  for(ii in times_set){
    f_ws_1[[ii]] <- as.matrix(read.csv(paste0('RES2/', f_para, '_1_1WS_time', ii, '.csv'))) #to_be_set_key
    f_ws_2[[ii]] <- as.vector(f_ws_1[[ii]])
  }
  return(f_ws_2)
}

ws_data <- list()
for(c_para in paras){
  ws_data[[c_para]] <- get_ws_f(c_para)
}


#==================
#up2024_0712_22:06

could_1 <- 0.0 #to_be_set
could_2 <- rep(could_1, len_sites * len_strs_co * len_days_ori)

#===============================
#up2024_0724_11:00

col_names_set <- c('DATE', 'TIME', 'LON', 'LAT', 'ELE', 'TIMEZONE', 'TP', 'RH', 'WS', 'CLOUD') #to_be_set

rayman_f1 <- function(f_para_tr, f_para_ws, f_time){
  f_rayman_1 <- matrix(0, nrow = len_sites * len_strs_co * len_days_ori, ncol = 10) #to_be_set
  f_rayman_1[,1] <- date_3
  f_rayman_1[,2] <- time_sum[[f_time]]
  f_rayman_1[,3] <- lon_2
  f_rayman_1[,4] <- lat_2
  f_rayman_1[,5] <- ele_2
  f_rayman_1[,6] <- timez_2
  f_rayman_1[,7] <- tp_data[[f_para_tr]][[f_time]]
  f_rayman_1[,8] <- rh_data[[f_para_tr]][[f_time]]
  f_rayman_1[,9] <- ws_data[[f_para_ws]][[f_time]]
  f_rayman_1[,10] <- could_2
  f_rayman_1_df <- as.data.frame(f_rayman_1)
  colnames(f_rayman_1_df) <- col_names_set
  write.csv(f_rayman_1_df, paste0('RAYMAN/rayman_1_time', ii, '_', f_para_tr, '_', f_para_ws, '.csv'), row.names = FALSE)
  write.table(f_rayman_1_df, file = paste0('RAYMAN/rayman_1_time', ii, '_', f_para_tr, '_', f_para_ws, '.txt'), sep = ' ', row.names = FALSE, col.names = TRUE, quote = FALSE)
}

#===============================
#up2024_0724_11:00

rayman_res <- list()
for(c_para_tr in paras){
  rayman_res[[c_para_tr]] <- list()
  for(c_para_ws in paras){
    rayman_res[[c_para_tr]][[c_para_ws]] <- list()
    for(ii in times_set){
      rayman_res[[c_para_tr]][[c_para_ws]][[ii]] <- rayman_f1(c_para_tr, c_para_ws, ii)
    }
  }
}
