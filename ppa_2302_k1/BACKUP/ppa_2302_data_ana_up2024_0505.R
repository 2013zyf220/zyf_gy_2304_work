library(MASS)
library(skimr)
library(DataExplorer)
library(tidyverse)
library(caret)
library(pROC)
library(ggplot2)
library(readxl)

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2302_k2')

#========================================
adj_para_1 <- read_csv('DATA_PRO_1/adj_para_1.csv', skip = 0)
TP_adj_2 <- read_csv('ARCGIS/TP_adj_2.csv', skip = 0)

#add columns for 'TP_adj_2'
TP_adj_ref <- TP_adj_2$rou4[1] #to_be_set
TP_adj_ref_2 <- rep(TP_adj_ref, 50)
TP_adj_2$rou7 <- TP_adj_ref_2
TP_adj_2$rou8 <- TP_adj_ref_2

#====================
#up2024_0505_09:55_s

str_to_rou_f <- function(f_str){
  if(f_str == 1||f_str == 2){
    f_rou <- 1
  }else if(f_str == 3||f_str == 4){
    f_rou <- 2
  }else if(f_str == 5||f_str == 6){
    f_rou <- 3
  }else if(f_str == 7||f_str == 8){
    f_rou <- 4
  }else{
    f_rou <- NA
  }
  return(f_rou)
}

#up2024_0505_09:55_e
#====================
#up2024_0505_09:55_s

adj_para_f <- function(f_rou, f_time_2){
  if(f_rou == 1){
    f_y1 <- adj_para_1$NO1
  }else if(f_rou == 2){
    f_y1 <- adj_para_1$NO2
  }else if(f_rou == 3){
    f_y1 <- adj_para_1$NO3
  }else if(f_rou == 4){
    f_y1 <- adj_para_1$NO4
  }else{
    print('ERROR')
  }
  f_y2 <- f_y1[f_time_2]
  return(f_y2)
}

#up2024_0505_09:55_e
#====================
#up2024_0505_09:55_s

data_pro1_f <- function(f_str, f_time_1, f_day){
  f_rou <- str_to_rou_f(f_str)
  f_data_1 <- read_excel(paste0('DATA_PRO_1/RH_TP_NO', f_rou, '_1.xlsx'))
  
  f_time_2 <- f_time_1 + (f_day - 1) * 3
  f_adj_1 <- adj_para_f(f_rou, f_time_2)
  f_adj_2 <- f_adj_1 + 720 - 1
  
  f_data_2 <- f_data_1[f_adj_1: f_adj_2,]
  f_data_3 <- f_data_2[c(seq(1, nrow(f_data_2), 6)), ]
  if(f_str %% 2 == 1){
    f_data_4 <- f_data_3[1:50, ]
  }else{
    f_data_4 <- rev(f_data_3[61:110, ])
  }
  f_res <- list()
  f_res[['data_2']] <- f_data_2
  f_res[['data_3']] <- f_data_3
  f_res[['data_4']] <- f_data_4
  return(f_res)
}

#up2024_0505_09:55_e
#====================
#up2024_0505_09:55_s

times_set <- c(1,2,3) #to_be_set_key
len_times <- length(times_set)

strs_co <- c(1,2,3,4,5,6,7,8)  #to_be_set_key
strs_mo <- c(1,2,3,4,5,6)  #to_be_set_key
len_strs_co <- length(strs_co)
len_strs_mo <- length(strs_mo)

days_set <- c(1,2,3)  #to_be_set_key
days_nor <- c(1,2)  #to_be_set_key(number inside days_set)
days_hot <- c(2,3)  #to_be_set_key(number inside days_set)
len_days <- length(days_set)
len_days_nor <- length(days_nor)
len_days_hot <- length(days_hot)

data_1_paras <- list()
data_1_paras[['times_set']] <- times_set
data_1_paras[['strs_co']] <- strs_co
data_1_paras[['strs_mo']] <- strs_mo
data_1_paras[['days_set']] <- days_set
data_1_paras[['days_nor']] <- days_nor
data_1_paras[['days_hot']] <- days_hot

#up2024_0505_09:55_e
cat('=========================step1!=========================\n')
#up2024_0505_09:55_s

data_TP_set <- 0 #to_be_set
data_1 <- list()
data_1_TIME <- list()
data_1_TP <- list()
data_1_RH <- list()

for(ii in times_set){
  cat('times:', ii, '\n')
  data_1[[ii]] <- list()
  data_1_TIME[[ii]] <- list()
  data_1_TP[[ii]] <- list()
  data_1_RH[[ii]] <- list()
  for(jj in strs_co){
    data_1[[ii]][[jj]] <- list()
    data_1_TIME[[ii]][[jj]] <- list()
    data_1_TP[[ii]][[jj]] <- list()
    data_1_RH[[ii]][[jj]] <- list()
    for(kk in days_set){
      data_1[[ii]][[jj]][[kk]] <- data_pro1_f(jj, ii, kk)$data_4
      data_1_TIME[[ii]][[jj]][[kk]] <- data_1[[ii]][[jj]][[kk]]$TIME
      data_1_RH[[ii]][[jj]][[kk]] <- data_1[[ii]][[jj]][[kk]]$RH  
      if(data_TP_set == 1){
        data_1_TP[[ii]][[jj]][[kk]] <- data_1[[ii]][[jj]][[kk]]$TP + TP_adj_2[[jj]]
      }else{
        data_1_TP[[ii]][[jj]][[kk]] <- data_1[[ii]][[jj]][[kk]]$TP
      }
    }
  }
}

#up2024_0505_09:55_e
cat('\n=========================step2!=========================\n')
#up2024_0505_09:55_s

data_2_TP <- list()
data_2_RH <- list()
data_2_TP_MAT <- list()
data_2_RH_MAT <- list()
data_2_TP_NOR_MAT <- list()
data_2_RH_NOR_MAT <- list()
data_2_TP_HOT_MAT <- list()
data_2_RH_HOT_MAT <- list()
for(ii in times_set){
  data_2_TP[[ii]] <- list()
  data_2_RH[[ii]] <- list()
  data_2_TP_MAT[[ii]] <- list()
  data_2_RH_MAT[[ii]] <- list()
  data_2_TP_NOR_MAT[[ii]] <- list()
  data_2_RH_NOR_MAT[[ii]] <- list()
  data_2_TP_HOT_MAT[[ii]] <- list()
  data_2_RH_HOT_MAT[[ii]] <- list()
  for(jj in strs_mo){
    data_2_TP[[ii]][[jj]] <- list()
    data_2_RH[[ii]][[jj]] <- list()
    data_2_TP_MAT[[ii]][[jj]] <- matrix(0, nrow = 50, ncol = len_days)
    data_2_RH_MAT[[ii]][[jj]] <- matrix(0, nrow = 50, ncol = len_days)
    for(kk in days_set){
      if(jj %% 2 == 1){
        c_ref_TP <- data_1_TP[[ii]][[7]][[kk]]
        c_ref_RH <- data_1_RH[[ii]][[7]][[kk]]        
      }else{
        c_ref_TP <- data_1_TP[[ii]][[8]][[kk]]
        c_ref_RH <- data_1_RH[[ii]][[8]][[kk]]  
      }
      data_2_TP[[ii]][[jj]][[kk]] <- data_1_TP[[ii]][[jj]][[kk]] - c_ref_TP
      data_2_RH[[ii]][[jj]][[kk]] <- data_1_RH[[ii]][[jj]][[kk]] - c_ref_RH
      data_2_TP_MAT[[ii]][[jj]][,kk] <- data_2_TP[[ii]][[jj]][[kk]]
      data_2_RH_MAT[[ii]][[jj]][,kk] <- data_2_RH[[ii]][[jj]][[kk]] 
    }
    data_2_TP_NOR_MAT[[ii]][[jj]] <- data_2_TP_MAT[[ii]][[jj]][, days_nor]
    data_2_RH_NOR_MAT[[ii]][[jj]] <- data_2_RH_MAT[[ii]][[jj]][, days_nor]
    data_2_TP_HOT_MAT[[ii]][[jj]] <- data_2_TP_MAT[[ii]][[jj]][, days_hot]
    data_2_RH_HOT_MAT[[ii]][[jj]] <- data_2_RH_MAT[[ii]][[jj]][, days_hot]
  }
}

#up2024_0505_09:55_e
#=======================================================
#up2024_0505_09:55_s

save(data_1_TIME, file = 'ARCGIS/ANA1_data_1_TIME.RData')
save(data_1_TP, file = 'ARCGIS/ANA1_data_1_TP.RData')
save(data_1_RH, file = 'ARCGIS/ANA1_data_1_RH.RData')
save(data_2_TP, file = 'ARCGIS/ANA1_data_2_TP.RData')
save(data_2_RH, file = 'ARCGIS/ANA1_data_2_RH.RData')
save(data_2_TP_MAT, file = 'ARCGIS/ANA1_data_2_TP_MAT.RData')
save(data_2_RH_MAT, file = 'ARCGIS/ANA1_data_2_RH_MAT.RData')
save(data_2_TP_NOR_MAT, file = 'ARCGIS/ANA1_data_2_TP_NOR_MAT.RData')
save(data_2_RH_NOR_MAT, file = 'ARCGIS/ANA1_data_2_RH_NOR_MAT.RData')
save(data_2_TP_HOT_MAT, file = 'ARCGIS/ANA1_data_2_TP_HOT_MAT.RData')
save(data_2_RH_HOT_MAT, file = 'ARCGIS/ANA1_data_2_RH_HOT_MAT.RData')
save(data_1_paras, file = 'ARCGIS/ANA1_data_1_paras.RData')

#up2024_0505_09:55_e
#=======================================================