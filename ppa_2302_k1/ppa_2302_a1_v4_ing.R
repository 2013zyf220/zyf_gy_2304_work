library(MASS)
library(skimr)
library(DataExplorer)
library(tidyverse)
library(caret)
library(pROC)
library(ggplot2)
library(pdp)
library(readxl)

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2302_k2')
adj_para_1 <- read_excel('DATA_PRO_1/adj_paras_1.xlsx', skip = 0);
#====================
#up2024_0430_20:20_s

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

#up2024_0430_20:20_e
#====================
#up2024_0430_20:20_s

adj_para_f <- function(f_rou, f_time_1){
  if(f_rou == 1){
    f_y1 <- adj_para_1$NO1
  }else if(f_rou == 2){
    f_y1 <- adj_para_1$NO2
  }else if(f_rou == 3){
    f_y1 <- adj_para_1$NO3
  }else if(f_rou == 4){
    f_y1 <- adj_para_1$NO4
  }else{
    print("ERROR")
  }
  f_y2 <- f_y1[f_time_1]
  return(f_y2)
}

#up2024_0430_20:20_e
#====================
#up2024_0430_22:20_s

data_pro1_f <- function(f_str, f_time_1, f_day){
  f_rou <- str_to_rou_f(f_str)
  f_data_1 <- read_excel(paste0('DATA_PRO_1/RH_TP_NO', f_rou, '_1.xlsx'))
  
  f_time_2 <- f_time_1 + (f_day - 1) * 3
  f_adj_1 <- adj_para_f(f_rou, f_time_2)
  f_adj_2 <- f_adj_1 + 720 - 1
  
  f_data_2 <- f_data_1[f_adj_1: f_adj_2,]
  f_data_3 <- f_data_2[c(seq(1, nrow(f_data_2), by = 6)), ]
  if(f_str%%2 == 1){
    f_data_4 <- f_data_3[1:50, ]
  }else{
    f_data_4 <- rev(f_data_3[51:100, ])
  }
  f_res <- list()
  f_res[['data_2']] <- f_data_2
  f_res[['data_3']] <- f_data_3
  f_res[['data_4']] <- f_data_4
  return(f_res)
}

#up2024_0430_22:20_e
#====================
#up2024_0430_22:27_s

times_set <- c(1,2,3) #to_be_set_key
len_times <- length(times_set)

strs_co <- c(1,2,3,4,5,6,7,8)  #to_be_set_key
strs_mo <- c(1,2,3,4,5,6)  #to_be_set_key

days_set <- c(1,2,3)  #to_be_set_key
days_nor <- c(1,2)  #to_be_set_key
len_days <- length(days_set)
len_days_nor <- length(days_nor)

#up2024_0430_22:27_e
cat('=========================step1!=========================\n')
#up2024_0430_22:39_s

data_str_1 <- list()
data_str_1_TIME <- list()
data_str_1_TP <- list()
data_str_1_RH <- list()

for(ii in times_set){
  cat('times:', ii, '\n')
  data_str_1[[ii]] <- list()
  data_str_1_TIME[[ii]] <- list()
  data_str_1_TP[[ii]] <- list()
  data_str_1_RH[[ii]] <- list()
  for(jj in strs_co){
    data_str_1[[ii]][[jj]] <- list()
    data_str_1_TIME[[ii]][[jj]] <- list()
    data_str_1_TP[[ii]][[jj]] <- list()
    data_str_1_RH[[ii]][[jj]] <- list()
    for(kk in days_set){
      #cat('day:', jj, '\n')
      data_str_1[[ii]][[jj]][[kk]] <- data_pro1_f(jj, ii, kk)$data_4
      data_str_1_TIME[[ii]][[jj]][[kk]] <- data_str_1[[ii]][[jj]][[kk]]$TIME
      data_str_1_TP[[ii]][[jj]][[kk]] <- data_str_1[[ii]][[jj]][[kk]]$TP + TP_adj_2[[ii]]
      data_str_1_RH[[ii]][[jj]][[kk]] <- data_str_1[[ii]][[jj]][[kk]]$RH      
    }
  }
}

#up2024_0430_22:39_e
cat('\n=========================step2!=========================\n')
#up2024_0430_22:47_s

data_str_2_TP <- list()
data_str_2_RH <- list()
data_str_2_TP_MAT <- list()
data_str_2_RH_MAT <- list()
data_str_2_TP_MAT2 <- list()
data_str_2_RH_MAT2 <- list()
data_str_2_TP_NOR_MAT <- list()
data_str_2_RH_NOR_MAT <- list()
for(ii in times_set){
  data_str_2_TP[[ii]] <- list()
  data_str_2_RH[[ii]] <- list()
  data_str_2_TP_MAT[[ii]] <- list()
  data_str_2_RH_MAT[[ii]] <- list()
  data_str_2_TP_MAT2[[ii]] <- list()
  data_str_2_RH_MAT2[[ii]] <- list()
  data_str_2_TP_NOR_MAT[[ii]] <- list()
  data_str_2_RH_NOR_MAT[[ii]] <- list()
  for(jj in strs_mo){
    cat('street:',jj, '\n')
    data_str_2_TP[[ii]][[jj]] <- list()
    data_str_2_RH[[ii]][[jj]] <- list()
    data_str_2_TP_MAT[[ii]][[jj]] <- matrix(0, nrow = 50, ncol = len_days)
    data_str_2_RH_MAT[[ii]][[jj]] <- matrix(0, nrow = 50, ncol = len_days)
    data_str_2_TP_NOR_MAT[[ii]][[jj]] <- matrix(0, nrow = 50, ncol = len_days_nor)
    data_str_2_RH_NOR_MAT[[ii]][[jj]] <- matrix(0, nrow = 50, ncol = len_days_nor)
    for(kk in days_set){
      if(ii%%2 == 1){
        c_ref_TP <- data_str_1_TP[[ii]][[7]][[kk]]
        c_ref_RH <- data_str_1_RH[[ii]][[7]][[kk]]        
      }else{
        c_ref_TP <- data_str_1_TP[[ii]][[8]][[kk]]
        c_ref_RH <- data_str_1_RH[[ii]][[8]][[kk]]  
      }
      data_str_2_TP[[ii]][[jj]][[kk]] <- data_str_1_TP[[ii]][[jj]][[kk]] - c_ref_TP
      data_str_2_RH[[ii]][[jj]][[kk]] <- data_str_1_RH[[ii]][[jj]][[kk]] - c_ref_RH
      data_str_2_TP_MAT[[ii]][[jj]][,kk] <- data_str_2_TP[[ii]][[jj]][[kk]]
      data_str_2_RH_MAT[[ii]][[jj]][,kk] <- data_str_2_RH[[ii]][[jj]][[kk]]     
    }
    data_str_2_TP_MAT2[[ii]][[jj]] <- c(data_str_2_TP_MAT[[ii]][[jj]])
    data_str_2_RH_MAT2[[ii]][[jj]] <- c(data_str_2_RH_MAT[[ii]][[jj]])
    data_str_2_TP_NOR_MAT[[ii]][[jj]] <- data_str_2_TP_MAT[[ii]][[jj]][, days_nor]
    data_str_2_RH_NOR_MAT[[ii]][[jj]] <- data_str_2_RH_MAT[[ii]][[jj]][, days_nor]
  }
}





cat('\n=========================step2!=========================\n')

data_str_2_TP_MEAN <- list()
data_str_2_TP_NOR_MEAN <- list()
for(ii in times_set){
  data_str_2_TP_MEAN[[ii]] <- list()
  data_str_2_TP_NOR_MEAN[[ii]] <- list()
  for(jj in strs_mo){
    data_str_2_TP_MEAN[[ii]][[jj]] <- rowMeans(data_str_2_TP_MAT[[ii]][[jj]])
    data_str_2_TP_NOR_MEAN[[ii]][[jj]] <- rowMeans(data_str_2_TP_NOR_MAT[[ii]][[jj]])    
  }
}
  

plot(data_str_2_TP_MEAN[[1]][[6]], type = "l", col = "black", lwd = 2, xlab = "Distance", ylab = "Temperature", main = "Simple Line Plot") #to_be_set
#up2024_0430_22:47_e
#====================
dis_1 <- 1:50
dis_2 <- rep(dis_1, times = len_days)

for(ii in times_set){
  plot(dis_2, data_str_2_TP_MAT2[[1]], main="Scatter Plot of X vs. Y", xlab="X values", ylab="Y values", pch=19, col="blue")
}

#====================
#====================


#======================
#求降温指标
#相关分析



