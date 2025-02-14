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
#up2024_0531_15:15
#initial setting and input data

len_sites <- 50  #to_be_set
adj_para_1 <- read_csv('DATA_PRO_1/adj_para_1.csv', skip = 0)

#====================
#up2024_0531_15:15
#define function: from street to route(8s4r)

str2rou_f <- function(f_str){
  f_rou <- ceiling(f_str/2)
  return(f_rou)
}

#====================
#up2024_0531_15:15
#define function: set start time order of each data set

adj_para_f <- function(f_rou, f_time_2){
  f_name <- paste0('NO', f_rou)
  f_res <- adj_para_1[[f_name]][f_time_2]
  return(f_res)
}

#====================
#up2024_0531_15:15
#set parameters

times_set <- c(1,2,3) #to_be_set_key
strs_co <- c(1,2,3,4,5,6,7,8)  #to_be_set_key
strs_mo <- c(1,2,3,4,5,6)  #to_be_set_key
days_ori <- c(1,2,3,4,5,6)  #to_be_set_key
days_nor <- c(1,2,3,5)  #to_be_set_key(number inside days_ori)
days_hot <- c(4,6)  #to_be_set_key(number inside days_ori)

varis <- c('TP','RH','DI','HI','HR','WS','TG','WBGT','PET','UTCI')  #to_be_set
subs1 <- list(NOR = days_nor, HOT = days_hot)  #to_be_set
subs1_name <- c('NOR','HOT')  #to_be_set
subs <- c(ORI = days_ori, subs1)
subs_name <- c('ORI', subs1_name)

strs_co_name <- c('str1','str2','str3','str4','str5','str6','str7','str8')  #to_be_set
strs_mo_name <- c('str1','str2','str3','str4','str5','str6')  #to_be_sets
days_ori_name <- c('day1','day2','day3','day4','day5','day6') #to_be_set

len_times_set <- length(times_set)
len_strs_co <- length(strs_co)
len_strs_mo <- length(strs_mo)
len_days_ori <- length(days_ori)
len_days_nor <- length(days_nor)
len_days_hot <- length(days_hot)
len_varis <- length(varis)
len_subs1 <- length(subs1)

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
data_1_paras[['subs']] <- subs
data_1_paras[['subs_name']] <- subs_name
data_1_paras[['strs_co_name']] <- strs_co_name
data_1_paras[['strs_mo_name']] <- strs_mo_name
data_1_paras[['days_ori_name']] <- days_ori_name

cat('==============step 2: get basic data================\n')
#up2024_0531_15:15
#define function: get basic data(all variables)

data_1_f <- function(f_time_1, f_str, f_day){
  f_rou <- str2rou_f(f_str)
  f_data_1 <- read_excel(paste0('DATA_PRO_1/RH_TP_NO', f_rou, '_1.xlsx'))
  
  f_time_2 <- f_time_1 + (f_day - 1) * 3
  f_adj_1 <- adj_para_f(f_rou, f_time_2)
  f_adj_2 <- f_adj_1 + 720 - 1
  
  f_data_2 <- f_data_1[f_adj_1: f_adj_2,]
  f_data_3 <- f_data_2[seq(1, nrow(f_data_2), 6), ]
  if(f_str %% 2 == 1){
    f_data_4 <- f_data_3[1:50, ]
    f_add <- 0
  }else{
    f_data_4 <- f_data_3[61:110, ]
    f_add <- 360
  }
  
  f_data_5 <- list()
  for(pp in 1:50){
    fc_1 <- (pp - 1) * 6 + 1 + f_add
    fc_2 <- pp * 6 + f_add
    f_data_5[[pp]] <- f_data_2[fc_1: fc_2,]
  }
  
  f_res <- list()
  f_res[['data_2']] <- f_data_2 #data at start represents data of the minute
  f_res[['data_3']] <- f_data_3
  f_res[['data_4']] <- f_data_4
  f_res[['data_5']] <- f_data_5 #average data represents data of the minute
  return(f_res)
}

#========================================
#up2024_0531_15:15
#define function: get basic data(for selected variable) and export file

d1_vari_f <- function(f_vari){
  f_d1_vari <- list()
  f_d2_vari <- list()
  f_d1_vari_df <- list()
  f_d2_vari_df <- list()
  for(ii in times_set){
    f_d1_vari[[ii]] <- matrix(0, nrow = len_sites * len_strs_co, ncol = len_days_ori)
    f_d2_vari[[ii]] <- matrix(0, nrow = len_sites * len_strs_co, ncol = len_days_ori)
    for(jj in strs_co){
      f_s <- (jj - 1) * len_sites + 1
      f_e <- jj * len_sites
      for(kk in days_ori){
        cat('d1_vari_f', ii, jj, kk, '\n')
        f_res_1 <- data_1_f(ii, jj, kk)$data_4[[f_vari]]
        f_res_2 <- rep(0, 50)
        if(f_vari == 'TIME'){
          for(pp in 1: 50){
            f_res_2[pp] <- data_1_f(ii, jj, kk)$data_5[[pp]][[f_vari]][1]
          }         
        }else{
          for(pp in 1: 50){
            f_res_2[pp] <- mean(data_1_f(ii, jj, kk)$data_5[[pp]][[f_vari]])
          }         
        }
        if(jj %% 2 == 1){
          f_d1_vari[[ii]][f_s:f_e,kk] <- f_res_1
          f_d2_vari[[ii]][f_s:f_e,kk] <- f_res_2
        }else{
          f_d1_vari[[ii]][f_s:f_e,kk] <- rev(f_res_1)
          f_d2_vari[[ii]][f_s:f_e,kk] <- rev(f_res_2)
        }
      }
    }
    f_d1_vari_df[[ii]] <- as.data.frame(f_d1_vari[[ii]])
    f_d2_vari_df[[ii]] <- as.data.frame(f_d2_vari[[ii]])
    colnames(f_d1_vari_df[[ii]]) <- days_ori_name
    colnames(f_d2_vari_df[[ii]]) <- days_ori_name
    write.csv(f_d1_vari_df[[ii]], paste0('ARCGIS/RES1/data_1_', f_vari, '_time', ii,'.csv'), row.names = FALSE)
    write.csv(f_d2_vari_df[[ii]], paste0('ARCGIS/RES1/datab_1_', f_vari, '_time', ii,'.csv'), row.names = FALSE)
  }
  
  f_res <- list()
  f_res[['data_1']] <- f_d1_vari_df
  f_res[['data_2']] <- f_d2_vari_df
  return(f_res)
}

#=============================================
#up2024_0531_15:15

d1_vari_TP <- d1_vari_f('TP')
d1_vari_RH <- d1_vari_f('RH')
d1_vari_TIME <- d1_vari_f('TIME')

#=============================================
#up2024_0531_15:15

save(data_1_paras, file = 'ARCGIS/RES1/ANA1_data_1_paras.RData')
