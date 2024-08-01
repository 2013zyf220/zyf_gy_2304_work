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
#up2024_0531_15:30
#initial setting and input data

len_sites <- 50  #to_be_set
load('ARCGIS/RES1/ANA1_data_1_paras.RData')

times_set <- data_1_paras[['times_set']]
strs_co <- data_1_paras[['strs_co']]
strs_mo <- data_1_paras[['strs_mo']]
days_ori <- data_1_paras[['days_ori']]
days_nor <- data_1_paras[['days_nor']]
days_hot <- data_1_paras[['days_hot']]
varis <- data_1_paras[['varis']]
subs1 <- data_1_paras[['subs1']]
subs1_name <- data_1_paras[['subs1_name']]
subs <- data_1_paras[['subs']]
subs_name <- data_1_paras[['subs_name']]
days_ori_name <- data_1_paras[['days_ori_name']]

len_times_set <- length(times_set)
len_strs_co <- length(strs_co)
len_strs_mo <- length(strs_mo)
len_days_ori <- length(days_ori)
len_days_nor <- length(days_nor)
len_days_hot <- length(days_hot)
len_varis <- length(varis)
len_subs1 <- length(subs1)

#========================================
#up2024_0531_15:30
#set temperature adjustment based on elevation

TP_adj_2 <- read_csv('ARCGIS/TP_adj_2.csv', skip = 0)

TP_adj_ref <- TP_adj_2$str4[1] #to_be_set
TP_adj_ref_2 <- rep(TP_adj_ref, len_sites)
TP_adj_2$str7 <- TP_adj_ref_2
TP_adj_2$str8 <- TP_adj_ref_2

#==============================================
#up2024_0531_15:30
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

#==================================
#up2024_0723_22:30

varis2g_f <- function(f_vari){
  if(f_vari == 'TPG'){
    f_vari2 <- 'TP'
  }else if(f_vari == 'RHG'){
    f_vari2 <- 'RH'
  }else{
    f_vari2 <- f_vari
  }
  return(f_vari2)
}

varis2w_f <- function(f_vari){
  if(f_vari == 'TPW'){
    f_vari2 <- 'TP'
  }else if(f_vari == 'RHW'){
    f_vari2 <- 'RH'
  }else{
    f_vari2 <- f_vari
  }
  return(f_vari2)
}

cat('==============step 2: get basic data================\n')
#up2024_0531_15:30
#get original data of meteorological variables
recw_seta <- 1   #to_be_set

varis2 <- c('TIME','TP','RH')   #to_be_set
varis2w <- c('TPW', 'RHW','WS')   #to_be_set_key
varis2g <- c('TPG', 'RHG', 'TG', 'WBGT') #to_be_set_key
varis3 <- c(varis2, varis2w, varis2g) #to_be_set
varis4 <-  setdiff(varis3, 'TIME')
varis5 <- c(varis4, 'DI', 'HI', 'HR', 'PET', 'UTCI') #to_be_set

data_1_ori <- list()
for(c_vari in varis2){
  data_1_ori[[c_vari]] <- list()
  for(ii in times_set){
    data_1_ori[[c_vari]][[ii]] <- read.csv(paste0('ARCGIS/RES2/rec_1_', c_vari, '_time', ii,'.csv'))
  }
}

for(c_vari in varis2g){
  c_vari2 <- varis2g_f(c_vari)
  data_1_ori[[c_vari]] <- list()
  for(ii in times_set){
    data_1_ori[[c_vari]][[ii]] <- read.csv(paste0('ARCGIS/RES2/recg_1_', c_vari2, '_time', ii,'.csv'))
  }
}

for(c_vari in varis2w){
  c_vari2 <- varis2w_f(c_vari)
  data_1_ori[[c_vari]] <- list()
  for(ii in times_set){
    data_1_ori[[c_vari]][[ii]] <- read.csv(paste0('ARCGIS/RES2/recw2_1_', c_vari2, recw_seta, '_time', ii,'.csv'))
  }
}

#======================================
#up2024_0531_15:30
#adjust data form of 'data_1_ori'
#adjust TP data based on elevation 

data_1_ori2 <- list()
for(c_vari in varis3){
  data_1_ori2[[c_vari]] <- list()
  for(ii in times_set){
    data_1_ori2[[c_vari]][[ii]] <- list()
    for(jj in strs_co){
      c_str_name <- paste0('str',jj)
      data_1_ori2[[c_vari]][[ii]][[jj]] <- matrix(0, nrow = len_sites, ncol = len_days_ori)
      c_s <- (jj - 1) * len_sites + 1
      c_e <- jj * len_sites
      for(kk in days_ori){
        if(c_vari == 'TP'){
          #data_1_ori2[[c_vari]][[ii]][[jj]][,kk] <- data_1_ori[[c_vari]][[ii]][c_s:c_e,kk] + TP_adj_2[[c_str_name]] #to_be_set
          data_1_ori2[[c_vari]][[ii]][[jj]][,kk] <- data_1_ori[[c_vari]][[ii]][c_s:c_e,kk]
        }else{
          data_1_ori2[[c_vari]][[ii]][[jj]][,kk] <- data_1_ori[[c_vari]][[ii]][c_s:c_e,kk]
        }
      }
    }
  }
}

#======================================
#up2024_0716_08:30

data_1_TP <- data_1_ori2$TP
data_1_RH <- data_1_ori2$RH
data_1_TPG <- data_1_ori2$TPG
data_1_RHG <- data_1_ori2$RHG
data_1_TPW <- data_1_ori2$TPW
data_1_RHW <- data_1_ori2$RHW
data_1_TIME <- data_1_ori2$TIME
data_1_WS <- data_1_ori2$WS
data_1_TG <- data_1_ori2$TG
data_1_WBGT <- data_1_ori2$WBGT

cat('==============step2: calculate heat indexes================\n')
#up2024_0531_15:30
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
#up2024_0531_15:30
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
#up2024_0716_08:27

data_1_ray1 <- list()
data_1_ray2 <- list()
data_1_PET <- list()
data_1_UTCI <- list()

for(ii in times_set){
  data_1_ray1[[ii]] <- read.table(paste0('ARCGIS/RAYMAN/rayman_1_time', ii, '_rec_out.dat'), header = FALSE, skip = 5)
  data_1_ray2[[ii]] <- as.matrix(data_1_ray1[[ii]])
  c_pet_1 <- data_1_ray1[[ii]][,18] #to_be_set
  c_utci_1 <- data_1_ray1[[ii]][,19] #to_be_set
  c_pet_2 <- matrix(c_pet_1, nrow = len_sites * len_strs_co, ncol = len_days_ori, byrow = FALSE)
  c_utci_2 <- matrix(c_utci_1, nrow = len_sites * len_strs_co, ncol = len_days_ori, byrow = FALSE)
  data_1_PET[[ii]] <- list()
  data_1_UTCI[[ii]] <- list()
  for(jj in strs_co){
    fc_1 <- (jj - 1) * len_sites + 1
    fc_2 <- jj * len_sites
    data_1_PET[[ii]][[jj]] <- matrix(0, nrow = len_sites, ncol = len_days_ori)
    data_1_UTCI[[ii]][[jj]] <- matrix(0, nrow = len_sites, ncol = len_days_ori)
    for(kk in days_ori){
      data_1_PET[[ii]][[jj]][,kk] <- c_pet_2[fc_1:fc_2,kk]
      data_1_UTCI[[ii]][[jj]][,kk] <- c_utci_2[fc_1:fc_2,kk]
    }
  }
  write.csv(c_pet_2, paste0('ARCGIS/RES3/rayman_pet_out_', ii, '.csv'))
  write.csv(c_utci_2, paste0('ARCGIS/RES3/rayman_utci_out_', ii, '.csv'))
  write.csv(data_1_ray1[[ii]], paste0('ARCGIS/RES3/rayman0_out_', ii, '.csv'))
}

#==============================================
#up2024_0716_08:30
#sumary of all data sets into data_1

data_1 <- list()
data_1[['TP']] <- data_1_TP
data_1[['RH']] <- data_1_RH
data_1[['TPG']] <- data_1_TPG
data_1[['RHG']] <- data_1_RHG
data_1[['TPW']] <- data_1_TPW
data_1[['RHW']] <- data_1_RHW
data_1[['WS']] <- data_1_WS
data_1[['TG']] <- data_1_TG
data_1[['WBGT']] <- data_1_WBGT
data_1[['DI']] <- data_1_DI
data_1[['HI']] <- data_1_HI
data_1[['HR']] <- data_1_HR
data_1[['PET']] <- data_1_PET
data_1[['UTCI']] <- data_1_UTCI

cat('==============step3.1: change relative to the reference(all days)================\n')
#up2024_0528_16:00
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
#up2024_0528_16:00
#calculate change relative to the reference(all days)

data_2_ori <- list()
for(c_vari in varis5){
  data_2_ori[[c_vari]] <- d2_vari_f(c_vari)
}

cat('==============step3.2: change relative to the reference(selected days)================\n')
#up2024_0528_16:00
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
#up2024_0528_16:00
#change relative to the reference(selected days)

data_2 <- list() 
data_2[['ORI']] <- data_2_ori
for(c_subs1_name in subs1_name){
  c_subs1 <- subs1[[c_subs1_name]]
  data_2[[c_subs1_name]] <- list()
  for(c_vari in varis5){
    data_2[[c_subs1_name]][[c_vari]] <- d2_vari_sub_f(c_subs1, c_vari)
  }
}

cat('==============step4: export files================\n')
#up2024_0528_16:00

data_1_csv <- list()
data_1_csv_df <- list()
for(c_vari in varis5){
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
    write.csv(data_1_csv_df[[c_vari]][[ii]], paste0('ARCGIS/RES3/recc_1_', c_vari, '_', ii,'_df.csv'), row.names = FALSE)
  }
}

#=======================================================
#up2024_0528_16:00

data_2_csv <- list()
data_2_csv_df <- list()
for(c_vari in varis5){
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
    write.csv(data_2_csv_df[[c_vari]][[ii]], paste0('ARCGIS/RES3/recc_2_', c_vari, '_', ii,'_df.csv'), row.names = FALSE)
  }
}
