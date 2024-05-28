library(MASS)
library(skimr)
library(DataExplorer)
library(tidyverse)
library(caret)
library(pROC)
library(ggplot2)
library(readxl)

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2302_k2')

cat('==============step 1: preparation================\n')
#up2024_0528_15:00
#initial setting and input data

len_sites <- 50  #to_be_set

adj_para_1 <- read_csv('DATA_PRO_1/adj_para_1.csv', skip = 0)
TP_adj_2 <- read_csv('ARCGIS/TP_adj_2.csv', skip = 0)

#========================================
#up2024_0528_10:30
#set temperature adjustment based on elevation

TP_adj_ref <- TP_adj_2$str4[1] #to_be_set
TP_adj_ref_2 <- rep(TP_adj_ref, len_sites)
TP_adj_2$str7 <- TP_adj_ref_2
TP_adj_2$str8 <- TP_adj_ref_2

#====================
#up2024_0528_10:30
#define function: from street to route(8s4r)

str2rou_f <- function(f_str){
  f_rou <- ceiling(f_str/2)
  return(f_rou)
}

#====================
#up2024_0528_10:30
#define function: set start time order of each data set

adj_para_f <- function(f_rou, f_time_2){
  f_name <- paste0('NO', f_rou)
  f_res <- adj_para_1[[f_name]][f_time_2]
  return(f_res)
}

#====================
#up2024_0528_10:30
#set parameters

times_set <- c(1,2,3) #to_be_set_key
strs_co <- c(1,2,3,4,5,6,7,8)  #to_be_set_key
strs_mo <- c(1,2,3,4,5,6)  #to_be_set_key
days_ori <- c(1,2,3,4,5,6)  #to_be_set_key
days_nor <- c(1,2,3,5)  #to_be_set_key(number inside days_ori)
days_hot <- c(4,6)  #to_be_set_key(number inside days_ori)

len_times_set <- length(times_set)
len_strs_co <- length(strs_co)
len_strs_mo <- length(strs_mo)
len_days_ori <- length(days_ori)
len_days_nor <- length(days_nor)
len_days_hot <- length(days_hot)

varis <- c('TP','RH','DI','HI','HR')  #to_be_set
subs1 <- list(NOR = days_nor, HOT = days_hot)  #to_be_set
subs1_name <- c('NOR','HOT')  #to_be_set
len_varis <- length(varis)
len_subs1 <- length(subs1)

strs_mo_name <- c('str1','str2','str3','str4','str5','str6')  #to_be_set
strs_co_name <- c('str1','str2','str3','str4','str5','str6','str7','str8')  #to_be_set
days_ori_name <- c('day1','day2','day3','day4','day5','day6') #to_be_set

data_1_paras <- list()
data_1_paras[['times_set']] <- times_set
data_1_paras[['strs_co']] <- strs_co
data_1_paras[['strs_mo']] <- strs_mo
data_1_paras[['days_ori']] <- days_ori
data_1_paras[['days_nor']] <- days_nor
data_1_paras[['days_hot']] <- days_hot
data_1_paras[['varis']] <- varis
data_1_paras[['subs1']] <- subs1
data_1_paras[['subs1_name']] <- subs1_name

#==============================================
#up2024_0528_10:30
#get data of air pressure

WEA_STA_PR_1 <- read.csv('ARCGIS/WEA_STA_1.csv')$PRESSURE
WEA_STA_TIME_1 <- read.csv('ARCGIS/WEA_STA_1.csv')$TIME
WEA_ORDER_1a <- matrix(0, nrow = len_times_set, ncol = len_days_ori)
WEA_ORDER_1b <- matrix(0, nrow = len_times_set, ncol = len_days_ori)
WEA_STA_PR_2a <- matrix(0, nrow = len_times_set, ncol = len_days_ori)
WEA_STA_PR_2b <- matrix(0, nrow = len_times_set, ncol = len_days_ori)
WEA_STA_TIME_2a <- list()
WEA_STA_TIME_2b <- list()

for(ii in times_set){
  WEA_STA_TIME_2a[[ii]] <- list()
  WEA_STA_TIME_2b[[ii]] <- list()
  for(jj in days_ori){
    WEA_ORDER_1a[ii,jj] <- (jj - 1) * 24 + ii * 5 + 5
    WEA_ORDER_1b[ii,jj] <- (jj - 1) * 24 + ii * 5 + 6
    WEA_STA_PR_2a[ii,jj] <- WEA_STA_PR_1[WEA_ORDER_1a[ii,jj]]
    WEA_STA_PR_2b[ii,jj] <- WEA_STA_PR_1[WEA_ORDER_1b[ii,jj]]
    WEA_STA_TIME_2a[[ii]][[jj]] <- WEA_STA_TIME_1[WEA_ORDER_1a[ii,jj]]
    WEA_STA_TIME_2b[[ii]][[jj]] <- WEA_STA_TIME_1[WEA_ORDER_1b[ii,jj]]
  }
}

cat('==============step 2: get basic data================\n')
#up2024_0528_10:30
#define function: get basic data(TP/RH)

data_1_f <- function(f_time_1, f_str, f_day){
  f_rou <- str2rou_f(f_str)
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
  f_res[['data_4']] <- f_data_4 #final data set
  return(f_res)
}

#==================================================
#up2024_0528_10:30
#get basic data(all variables together)

data_1_ori <- list()
for(ii in times_set){
  cat('times:', ii, '\n')
  data_1_ori[[ii]] <- list()
  for(jj in strs_co){
    data_1_ori[[ii]][[jj]] <- list()
    for(kk in days_ori){
      data_1_ori[[ii]][[jj]][[kk]] <- data_1_f(ii, jj, kk)$data_4
    }
  }
}

#========================================
#up2024_0528_10:30
#define function: get basic data(for selected variable)

d1_vari_f <- function(f_vari){
  f_d1_vari <- list()
  for(ii in times_set){
    f_d1_vari[[ii]] <- list()
    for(jj in strs_co){
      f_d1_vari[[ii]][[jj]] <- matrix(0, nrow = len_sites, ncol = len_days_ori)
      for(kk in days_ori){
        f_d1_vari[[ii]][[jj]][,kk] <- data_1_ori[[ii]][[jj]][[kk]][[f_vari]]
      }
    }
  }
  return(f_d1_vari)
}

#=============================================
#up2024_0528_10:30
#get basic data(for TIME/TP(unadjusted)/RH)

data_1_TIME <- d1_vari_f('TIME')
data_1_TP1 <- d1_vari_f('TP')
data_1_RH <- d1_vari_f('RH')

#=============================================
#up2024_0528_10:30
#get basic data(for TP(adjusted)) and choose TP data

data_1_TP2 <- list()
for(ii in times_set){
  data_1_TP2[[ii]] <- list()
  for(jj in strs_co){
    c_str_name <- paste0('str',jj)
    data_1_TP2[[ii]][[jj]] <- matrix(0, nrow = len_sites, ncol = len_days_ori)
    for(kk in days_ori){
        data_1_TP2[[ii]][[jj]][,kk] <- data_1_ori[[ii]][[jj]][[kk]][['TP']] + TP_adj_2[[c_str_name]]
    }
  }
}

data_1_TP <- data_1_TP2 #to_be_set(data_1_TP1/data_1_TP2)

cat('==============step2: calculate heat indexes================\n')
#up2024_0528_10:30
#define function: calculate DI,HI and HR

#for DI, refer to: Yin, Zhengtong, Zhixin Liu, Xuan Liu, Wenfeng Zheng, and Lirong Yin. 
#"Urban heat islands and their effects on thermal comfort in the US: New York and New Jersey." Ecological Indicators 154 (2023): 110765.

#for HI, refer to: Yao, Lingye, David J. Sailor, Xiaoshan Yang, Genyu Xu, and Lihua Zhao. 
#"Are water bodies effective for urban heat mitigation? Evidence from field studies of urban lakes in two humid subtropical cities." Building and Environment 245 (2023): 110860.

#for HR, refer to: Yao, Lingye, David J. Sailor, Xiaoshan Yang, Genyu Xu, and Lihua Zhao. 
#"Are water bodies effective for urban heat mitigation? Evidence from field studies of urban lakes in two humid subtropical cities." Building and Environment 245 (2023): 110860.

DI_f <- function(f_TP,f_RH){
  f_DI <- f_TP - 0.55 * (1 - 0.01 * f_RH) * (f_TP - 14.5)
  return(f_DI)
}

HI_f <- function(f_TP,f_RH){
  f_HI <- -8.784695 + 1.61139411 * f_TP + 2.338549 * f_RH - 0.14611605 * f_TP * f_RH - 1.2308094 * 10^-2 * f_TP^2
  - 1.6424828 * 10^-2 * f_RH^2 + 2.2211732 * 10^-3 * f_TP^2 * f_RH + 7.2546 * 10^-4 * f_TP * f_RH^2 - 3.582 * 10^-6 * f_TP^2 * f_RH^2
  
  return(f_HI)
}

HR_f <- function(f_TP,f_RH,f_pa){
  f_TR <- 1 - 373.15/(273.15 + f_TP)
  f_es <- 101.325 * exp(13.3185 * f_TR - 1.9760 * f_TR^2 - 0.6445 * f_TR^3 - 0.1299 * f_TR^4)
  f_ea <- f_es * f_RH / 100
  f_HR <- 622 * f_ea/(f_pa - f_ea)
  return(f_HR)
}

#==============================================
#up2024_0528_10:30
#calculate heat indexes

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
      if(jj %% 2 == 1){
        c_pa <- WEA_STA_PR_2a[ii,kk]
      }else{
        c_pa <- WEA_STA_PR_2b[ii,kk]
      }
      for(mm in 1: len_sites){
        data_1_DI[[ii]][[jj]][mm,kk] <- DI_f(data_1_TP[[ii]][[jj]][mm,kk], data_1_RH[[ii]][[jj]][mm,kk])
        data_1_HI[[ii]][[jj]][mm,kk] <- HI_f(data_1_TP[[ii]][[jj]][mm,kk], data_1_RH[[ii]][[jj]][mm,kk])
        data_1_HR[[ii]][[jj]][mm,kk] <- HR_f(data_1_TP[[ii]][[jj]][mm,kk], data_1_RH[[ii]][[jj]][mm,kk], c_pa)
      }
    }
  }
}

#==============================================
#up2024_0528_10:30
#sumary of all data sets into data_1

data_1 <- list()
data_1[['TP']] <- data_1_TP
data_1[['RH']] <- data_1_RH
data_1[['DI']] <- data_1_DI
data_1[['HI']] <- data_1_HI
data_1[['HR']] <- data_1_HR

cat('==============step3.1: change relative to the reference(all days)================\n')
#up2024_0528_10:30
#define function: calculate change relative to the reference

d2_vari_f <- function(f_vari){
  f_d1_vari <- data_1[[f_vari]]
  f_d2_vari <- list()
  for(ii in times_set){
    f_d2_vari[[ii]] <- list()
    for(jj in strs_mo){
      f_d2_vari[[ii]][[jj]] <- matrix(0, nrow = len_sites, ncol = len_days_ori)
      for(kk in days_ori){
        if(jj %% 2 == 1){
          f_ref_1 <- f_d1_vari[[ii]][[7]][,kk]
        }else{
          f_ref_1 <- f_d1_vari[[ii]][[8]][,kk]
        }
        f_d2_vari[[ii]][[jj]][,kk] <- f_d1_vari[[ii]][[jj]][,kk] - f_ref_1
      }
    }
  }
  return(f_d2_vari)
}

#=============================================
#up2024_0528_10:30
#calculate change relative to the reference(all days)

data_2_ori <- list()
for(c_vari in varis){
  data_2_ori[[c_vari]] <- d2_vari_f(c_vari)
}

cat('==============step3.2: change relative to the reference(selected days)================\n')
#up2024_0528_10:30
#define function: change relative to the reference(selected days)

d2_vari_sub_f <- function(f_sub, f_vari){
  f_data_1 <- data_2_ori[[f_vari]]
  f_data_2 <- list()
  for(ii in times_set){
    f_data_2[[ii]] <- list()
    for(jj in strs_mo){
      f_data_2[[ii]][[jj]] <- f_data_1[[ii]][[jj]][, f_sub]
    }
  }
  return(f_data_2)
}

#=============================================
#up2024_0528_10:30
#change relative to the reference(selected days)

data_2 <- list() 
data_2[['ORI']] <- data_2_ori
for(ii in 1:len_subs1){
  c_subs1_name <- subs1_name[ii]
  c_subs1 <- subs1[[c_subs1_name]]
  data_2[[c_subs1_name]] <- list()
  for(c_vari in varis){
    data_2[[c_subs1_name]][[c_vari]] <- d2_vari_sub_f(c_subs1, c_vari)
  }
}

cat('==============step4: export files================\n')
#up2024_0528_10:30

data_1_csv <- list()
data_1_csv_df <- list()
for(c_vari in varis){
  data_1_csv[[c_vari]] <- list()
  data_1_csv_df[[c_vari]] <- list() 
  for(ii in times_set){
    data_1_csv[[c_vari]][[ii]] <- matrix(0, nrow = len_sites * len_strs_co, ncol = len_days_ori)
    for(kk in days_ori){
      for(jj in strs_co){
        c_1 <- (jj - 1) * len_sites + 1
        c_2 <- jj * len_sites
        data_1_csv[[c_vari]][[ii]][c_1:c_2,kk] <- data_1[[c_vari]][[ii]][[jj]][,kk]
      }
    }
    data_1_csv_df[[c_vari]][[ii]] <- as.data.frame(data_1_csv[[c_vari]][[ii]])
    colnames(data_1_csv_df[[c_vari]][[ii]]) <- days_ori_name
    write.csv(data_1_csv_df[[c_vari]][[ii]], paste0('ARCGIS/RES1/datac_1_', c_vari, '_', ii,'_df.csv'), row.names = FALSE)
  }
}

#=======================================================
#up2024_0528_10:30

data_2_csv <- list()
data_2_csv_df <- list()
for(c_vari in varis){
  data_2_csv[[c_vari]] <- list()
  data_2_csv_df[[c_vari]] <- list()
  for(ii in times_set){
    data_2_csv[[c_vari]][[ii]] <- matrix(0, nrow = len_sites * len_strs_mo, ncol = len_days_ori)
    for(kk in days_ori){
      for(jj in strs_mo){
        c_1 <- (jj - 1) * len_sites + 1
        c_2 <- jj * len_sites
        data_2_csv[[c_vari]][[ii]][c_1:c_2,kk] <- data_2$ORI[[c_vari]][[ii]][[jj]][,kk]
      }
    }
    data_2_csv_df[[c_vari]][[ii]] <- as.data.frame(data_2_csv[[c_vari]][[ii]])
    colnames(data_2_csv_df[[c_vari]][[ii]]) <- days_ori_name
    write.csv(data_2_csv_df[[c_vari]][[ii]], paste0('ARCGIS/RES1/datac_2_', c_vari, '_', ii,'_df.csv'), row.names = FALSE)
  }
}

#=======================================================
#up2024_0528_10:30
#save data

save(data_1_paras, file = 'ARCGIS/RES1/ANA1_data_1_paras.RData')
