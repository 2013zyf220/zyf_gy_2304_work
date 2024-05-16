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
#up2024_0515_17:00_s

adj_para_1 <- read_csv('DATA_PRO_1/adj_para_1.csv', skip = 0)
TP_adj_2 <- read_csv('ARCGIS/TP_adj_2.csv', skip = 0)

#add columns for 'TP_adj_2'
TP_adj_ref <- TP_adj_2$rou4[1] #to_be_set
TP_adj_ref_2 <- rep(TP_adj_ref, 50)
TP_adj_2$rou7 <- TP_adj_ref_2
TP_adj_2$rou8 <- TP_adj_ref_2

#up2024_0515_17:00_e
#====================
#up2024_0515_17:00_s

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

#up2024_0515_17:00_e
#====================
#up2024_0515_17:00_s

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

#up2024_0515_17:00_e
#====================
#up2024_0515_17:00_s

data_pro1_f <- function(f_time_1, f_str, f_day){
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

#up2024_0515_17:00_e
#====================
#up2024_0515_17:00_s

times_set <- c(1,2,3) #to_be_set_key
len_times_set <- length(times_set)

strs_co <- c(1,2,3,4,5,6,7,8)  #to_be_set_key
strs_mo <- c(1,2,3,4,5,6)  #to_be_set_key
len_strs_co <- length(strs_co)
len_strs_mo <- length(strs_mo)

days_ori <- c(1,2,3,4,5,6)  #to_be_set_key
days_nor <- c(1,2,3,5)  #to_be_set_key(number inside days_ori)
days_hot <- c(4,6)  #to_be_set_key(number inside days_ori)
len_days_ori <- length(days_ori)
len_days_nor <- length(days_nor)
len_days_hot <- length(days_hot)

VARIS <- c('TP','RH','DI','HI','HR')  #to_be_set
SUBS1 <- list(NOR = days_nor, HOT = days_hot)  #to_be_set
SUBS1_NAME <- c('NOR','HOT')  #to_be_set
len_VARIS <- length(VARIS)
len_SUBS1 <- length(SUBS1)
len_sites <- 50  #to_be_set

data_1_paras <- list()
data_1_paras[['times_set']] <- times_set
data_1_paras[['strs_co']] <- strs_co
data_1_paras[['strs_mo']] <- strs_mo
data_1_paras[['days_ori']] <- days_ori
data_1_paras[['days_nor']] <- days_nor
data_1_paras[['days_hot']] <- days_hot
data_1_paras[['VARIS']] <- VARIS
data_1_paras[['SUBS1']] <- SUBS1
data_1_paras[['SUBS1_NAME']] <- SUBS1_NAME

#up2024_0515_17:00_e
#==============================================
#up2024_0515_17:00_s

WEA_STA_TIME_1 <- read.csv('ARCGIS/WEA_STA_1.csv')$TIME
WEA_STA_PR_1 <- read.csv('ARCGIS/WEA_STA_1.csv')$PRESSURE
WEA_ORDER_1 <- matrix(0, nrow = len_times_set, ncol = len_days_ori)
WEA_STA_PR_2 <- matrix(0, nrow = len_times_set, ncol = len_days_ori)
WEA_STA_TIME_2 <- list()

for(ii in times_set){
  WEA_STA_TIME_2[[ii]] <- list()
  for(jj in days_ori){
    WEA_ORDER_1[ii,jj] <- (jj - 1) * 24 + ii * 5 + 5
    WEA_STA_PR_2[ii,jj] <- WEA_STA_PR_1[WEA_ORDER_1[ii,jj]]
    WEA_STA_TIME_2[[ii]][[jj]] <- WEA_STA_TIME_1[WEA_ORDER_1[ii,jj]]
  }
}

#up2024_0515_17:00_e
cat('==============step1================\n')
#up2024_0515_17:00_s

data_1_ori <- list()
for(ii in times_set){
  cat('times:', ii, '\n')
  data_1_ori[[ii]] <- list()
  for(jj in strs_co){
    data_1_ori[[ii]][[jj]] <- list()
    for(kk in days_ori){
      data_1_ori[[ii]][[jj]][[kk]] <- data_pro1_f(ii, jj, kk)$data_4
    }
  }
}

#up2024_0515_17:00_e
#========================================
#up2024_0515_17:00_s

d1_VARI_f <- function(f_VARI){
  f_d1_VARI <- list()
  for(ii in times_set){
    f_d1_VARI[[ii]] <- list()
    for(jj in strs_co){
      f_d1_VARI[[ii]][[jj]] <- matrix(0, nrow = len_sites, ncol = len_days_ori)
      for(kk in days_ori){
        f_d1_VARI[[ii]][[jj]][,kk] <- data_1_ori[[ii]][[jj]][[kk]][[f_VARI]]
      }
    }
  }
  return(f_d1_VARI)
}

#up2024_0515_17:00_e
#=============================================
#up2024_0515_17:00_s

data_1_TIME <- d1_VARI_f('TIME')
data_1_TP1 <- d1_VARI_f('TP')
data_1_RH <- d1_VARI_f('RH')

#up2024_0515_17:00_e
#=============================================
#up2024_0515_17:00_s

data_1_TP2 <- list()
for(ii in times_set){
  data_1_TP2[[ii]] <- list()
  for(jj in strs_co){
    data_1_TP2[[ii]][[jj]] <- matrix(0, nrow = len_sites, ncol = len_days_ori)
    for(kk in days_ori){
        data_1_TP2[[ii]][[jj]][,kk] <- data_1_ori[[ii]][[jj]][[kk]][['TP']] + TP_adj_2[[jj]]
    }
  }
}

data_1_TP <- data_1_TP2 #to_be_set(data_1_TP1/data_1_TP2)

#up2024_0515_17:00_e
cat('==============step2================\n')
#up2024_0515_17:00_s

DI_f <- function(f_TP,f_RH){
  f_DI <- f_TP - 0.55 * (1 - 0.01 * f_RH) * (f_TP - 14.5)
  return(f_DI)
}

#refer to: Yin, Zhengtong, Zhixin Liu, Xuan Liu, Wenfeng Zheng, and Lirong Yin. 
#"Urban heat islands and their effects on thermal comfort in the US: New York and New Jersey." Ecological Indicators 154 (2023): 110765.
#up2024_0515_17:00_e
#====================================
#up2024_0515_17:00_s

HI_f <- function(f_TP,f_RH){
  f_HI <- -8.784695 + 1.61139411 * f_TP + 2.338549 * f_RH - 0.14611605 * f_TP * f_RH - 1.2308094 * 10^-2 * f_TP^2
  - 1.6424828 * 10^-2 * f_RH^2 + 2.2211732 * 10^-3 * f_TP^2 * f_RH + 7.2546 * 10^-4 * f_TP * f_RH^2 - 3.582 * 10^-6 * f_TP^2 * f_RH^2
  
  return(f_HI)
}

#refer to: Yao, Lingye, David J. Sailor, Xiaoshan Yang, Genyu Xu, and Lihua Zhao. 
#"Are water bodies effective for urban heat mitigation? Evidence from field studies of urban lakes in two humid subtropical cities." Building and Environment 245 (2023): 110860.
#up2024_0515_17:00_e
#====================================
#up2024_0515_17:00_s

HR_f <- function(f_TP,f_RH,f_pa){
  f_TR <- 1 - 373.15/(273.15 + f_TP)
  f_es <- 101.325 * exp(13.3185 * f_TR - 1.9760 * f_TR^2 - 0.6445 * f_TR^3 - 0.1299 * f_TR^4)
  f_ea <- f_es * f_RH / 100
  f_HR <- 622 * f_ea/(f_pa - f_ea)
  return(f_HR)
}

#refer to: Yao, Lingye, David J. Sailor, Xiaoshan Yang, Genyu Xu, and Lihua Zhao. 
#"Are water bodies effective for urban heat mitigation? Evidence from field studies of urban lakes in two humid subtropical cities." Building and Environment 245 (2023): 110860.
#up2024_0515_17:00_e
#==============================================
#up2024_0515_17:00_s

data_1_DI <- list()
data_1_HI <- list()
data_1_HR <- list()
for(ii in times_set){
  data_1_DI[[ii]] <- list()
  data_1_HI[[ii]] <- list()
  data_1_HR[[ii]] <- list()
  for(jj in strs_co){
    data_1_DI[[ii]][[jj]] <- matrix(0, nrow = len_sites, ncol = len_days_ori)
    data_1_HI[[ii]][[jj]] <- matrix(0, nrow = len_sites, ncol = len_days_ori)
    data_1_HR[[ii]][[jj]] <- matrix(0, nrow = len_sites, ncol = len_days_ori)
    for(kk in days_ori){
      for(mm in 1: len_sites){
        data_1_DI[[ii]][[jj]][mm,kk] <- DI_f(data_1_TP[[ii]][[jj]][mm,kk], data_1_RH[[ii]][[jj]][mm,kk])
        data_1_HI[[ii]][[jj]][mm,kk] <- HI_f(data_1_TP[[ii]][[jj]][mm,kk], data_1_RH[[ii]][[jj]][mm,kk])
        data_1_HR[[ii]][[jj]][mm,kk] <- HR_f(data_1_TP[[ii]][[jj]][mm,kk], data_1_RH[[ii]][[jj]][mm,kk], WEA_STA_PR_2[ii,kk])
      }
    }
  }
}

data_1 <- list()
data_1[['TP']] <- data_1_TP
data_1[['RH']] <- data_1_RH
data_1[['DI']] <- data_1_DI
data_1[['HI']] <- data_1_HI
data_1[['HR']] <- data_1_HR

#up2024_0515_17:00_e
cat('==============step3================\n')
#up2024_0515_17:00_s

d2_VARI_f <- function(f_VARI){
  f_d1_VARI <- data_1[[f_VARI]]
  f_d2_VARI <- list()
  for(ii in times_set){
    f_d2_VARI[[ii]] <- list()
    for(jj in strs_mo){
      f_d2_VARI[[ii]][[jj]] <- matrix(0, nrow = len_sites, ncol = len_days_ori)
      for(kk in days_ori){
        if(jj %% 2 == 1){
          f_REF_1 <- f_d1_VARI[[ii]][[7]][,kk]
        }else{
          f_REF_1 <- f_d1_VARI[[ii]][[8]][,kk]
        }
        f_d2_VARI[[ii]][[jj]][,kk] <- f_d1_VARI[[ii]][[jj]][,kk] - f_REF_1
      }
    }
  }
  return(f_d2_VARI)
}

#up2024_0515_17:00_e
#=============================================
#up2024_0515_17:00_s

data_2_ORI <- list()
for(c_VARI in VARIS){
  data_2_ORI[[c_VARI]] <- d2_VARI_f(c_VARI)
}

#up2024_0515_17:00_e
#=============================================
#up2024_0515_17:00_s

d2_VARI_SUB_f <- function(f_VARI, f_SUB){
  f_DATA_1 <- data_2_ORI[[f_VARI]]
  f_DATA_2 <- list()
  for(ii in times_set){
    f_DATA_2[[ii]] <- list()
    for(jj in strs_mo){
      f_DATA_2[[ii]][[jj]] <- f_DATA_1[[ii]][[jj]][, f_SUB]
    }
  }
  return(f_DATA_2)
}

#up2024_0515_17:00_e
#=============================================
#up2024_0515_17:00_s

data_2 <- list() 
data_2[['ORI']] <- data_2_ORI
for(ii in 1:len_SUBS1){
  c_SUB1_NAME <- SUBS1_NAME[ii]
  c_SUB1 <- SUBS1[[c_SUB1_NAME]]
  data_2[[c_SUB1_NAME]] <- list()
  for(c_VARI in VARIS){
    data_2[[c_SUB1_NAME]][[c_VARI]] <- d2_VARI_SUB_f(c_VARI, c_SUB1)
  }
}


#up2024_0515_17:00_e
#=======================================================
#up2024_0515_17:00_s

save(data_1_paras, file = 'ARCGIS/RES1/ANA1_data_1_paras.RData')
save(data_1, file = 'ARCGIS/RES1/ANA1_data_1.RData')
save(data_2, file = 'ARCGIS/RES1/ANA1_data_2.RData')

#up2024_0515_17:00_e
#=======================================================