library(MASS)
library(skimr)
library(DataExplorer)
library(tidyverse)
library(caret)
library(pROC)
library(ggplot2)
library(readxl)
library(gridExtra)

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2302_k2/ARCGIS')

#=================================================================

#result 1: each variable, each time, each street, each day
#result 2: each variable, each time, streets together, each day
#result 3: each sub, each variable, each time, each street, days merged
#result 4: each sub, each variable, each time, streets together, days merged
#result 5: each sub, each variable, each time, each street, days together
#result 6: each sub, each variable, each time, streets together, days together

cat('========================step 1: basic setting==========================\n')
#up2024_0528_17:00
#add and set parameters

load('RES1/ANA1_data_1_paras.RData')

times_set <- data_1_paras[['times_set']]
strs_co <- data_1_paras[['strs_co']]
strs_mo <- data_1_paras[['strs_mo']]
days_ori <- data_1_paras[['days_ori']]
days_nor <- data_1_paras[['days_nor']]
days_hot <- data_1_paras[['days_hot']]
varis <- data_1_paras[['varis']]
subs1 <- data_1_paras[['subs1']]
subs1_name <- data_1_paras[['subs1_name']]

subs <- c(ORI = list(days_ori), subs1)
subs_name <- c('ORI', subs1_name)

days_ori_name <- c('day1','day2','day3','day4','day5','day6')  #to_be_set
days_nor_name <- c('day1','day2','day3','day5')  #to_be_set
days_hot_name <- c('day4','day6')  #to_be_set
times_set_name <- c('time1','time2','time3')

len_times_set <- length(times_set)
len_strs_co <- length(strs_co)
len_strs_mo <- length(strs_mo)
len_days_ori <- length(days_ori)
len_days_nor <- length(days_nor)
len_days_hot <- length(days_hot)
len_varis <- length(varis)
len_subs <- length(subs)
len_sites <- 50 #to_be_set
len_sites_2 <- 50 #to_be_set_seq(25/50)

ele_2a <- read.csv('ele_2_table.csv')
ele_2b <- as.matrix(ele_2a)
ele_2c <- matrix(ele_2b, nrow = len_sites * len_strs_mo, ncol = 1)

buf_set <- 100 #to_be_set
index_name1 <- paste0('index_1m_df3_buf', buf_set, '.csv')
index_1 <- read.csv(index_name1)[1:300,]
index_1$ele_2 <- ele_2c
index_2 <- index_1

indep_set <- 40 #to_be_set_key

#=================================================================
#up2024_0531_17:00
#set independent variables for regression

reg_se_sum <- list()
reg_se_sum[[1]] <- c(23,40,41)
reg_se_sum[[2]] <- c(12)

reg_set <- 1 #to_be_set_key
reg_se <- reg_se_sum[[reg_set]]

cname_index_1 <- colnames(index_1)
cname_index_2 <- cname_index_1[reg_se] #to_be_set_key
cname_index_3 <- paste(cname_index_2, collapse = ' + ')

#============================define functions=====================================
#up2024_0528_17:00
#define function: from subs_name(name of days) to subs(days)

days_trans_f <- function(f_1){
  if(f_1 == 'ORI'){
    f_2 <- days_ori
  }else if(f_1 == 'NOR'){
    f_2 <- days_nor
  }else if(f_1 == 'HOT'){
    f_2 <- days_hot
  }
  return(f_2)
}

#======================================================
#up2024_0528_17:00
#define distances

dis_1 <- seq(10, 500, 10) #to_be_set
dis_2 <- rep(dis_1, len_strs_mo)
dis_3 <- list()
dis_3[['ORI']] <- rep(dis_1, len_days_ori)
dis_3[['NOR']] <- rep(dis_1, len_days_nor)
dis_3[['HOT']] <- rep(dis_1, len_days_hot)

#==============================================
#up2024_0617_20:30

days_array_f <- function(f_days){
  f_array <- c()
  for (mm in f_days){
    f_s <- (mm - 1) * 50 + 1
    f_e <- mm * 50
    f_array <- c(f_array, f_s:f_e)
  }
  return(f_array)
}

days_array_list <- list()
for(c_sub_name in subs_name){
  days_array_list[[c_sub_name]] <- days_array_f(days_trans_f(c_sub_name))
}
#==============================================
#up2024_0617_20:30
#for analysis by distances

bydis_itv <- 10 #to_be_set
bydis_num <- len_sites/bydis_itv

bydis <- list()
for(mm in 1: bydis_num){
  c_res <- c()
  for(nn in 1: len_strs_mo){
    c_s <- (nn - 1) * 50 + (mm - 1) * 10 + 1
    c_e <- (nn - 1) * 50 + mm * 10
    c_res <- c(c_res, c_s:c_e)
  }
  bydis[[mm]] <- c_res
}

bydis2 <- list()
for(mm in 1: bydis_num){
  c_s <- (mm - 1) * 10 + 1
  c_e <- mm * 10
  bydis2[[mm]] <- c_s: c_e
}

cat('========================step 2: basic data(data2_2)==========================\n')
#up2024_0528_17:00
#get original data(substract by reference, all days)

data2_2_ori <- list()
for(c_vari in varis){
  data2_2_ori[[c_vari]] <- list()
  for(ii in times_set){
    data2_2_ori[[c_vari]][[ii]] <- as.matrix(read.csv(paste0('RES2/recb_2_', c_vari, '_', ii,'_df.csv'))) #to_be_set
  } 
}

#=========================================================
#up2024_0528_18:00
#get original data(substract by reference, selected days)

data2_2_nor <- list()
data2_2_hot <- list()
for(c_vari in varis){
  data2_2_nor[[c_vari]] <- list()
  data2_2_hot[[c_vari]] <- list()
  for(ii in times_set){
    data2_2_nor[[c_vari]][[ii]] <- data2_2_ori[[c_vari]][[ii]][ ,days_nor]
    data2_2_hot[[c_vari]][[ii]] <- data2_2_ori[[c_vari]][[ii]][ ,days_hot]
  }
}

data2_2 <- list()
data2_2[['ORI']] <- data2_2_ori
data2_2[['NOR']] <- data2_2_nor
data2_2[['HOT']] <- data2_2_hot

cat('========================step 3: calculate mean==========================\n')
#up2024_0528_18:00
#function: input weather data(averaged by day)

d2_mean_1f <- function(f_data, f_sub, f_vari){
  f_1 <- f_data[[f_sub]][[f_vari]]
  f_mean_1 <- matrix(0, nrow = len_sites * len_strs_mo, ncol = len_times_set)
  for(ii in times_set){
    f_mean_1[ ,ii] <- rowMeans(f_1[[ii]])
  }
  f_res <- list()
  f_res[['r1']] <- f_mean_1
  return(f_res)
}

#==============================================
#up2024_0528_18:00
#summarize data(averaged by day)
#size of data2_2_mean$NOR$TP: 300 * 3

data2_2_mean <- list()
for(c_sub_name in subs_name){
  data2_2_mean[[c_sub_name]] <- list()
  for(c_vari in varis){
    data2_2_mean[[c_sub_name]][[c_vari]] <- d2_mean_1f(data2_2,c_sub_name, c_vari)$r1
    c_data2_2_mean <- data2_2_mean[[c_sub_name]][[c_vari]]
    c_data2_2_mean_df <- as.data.frame(c_data2_2_mean)
    colnames(c_data2_2_mean_df) <- times_set_name
    write.csv(c_data2_2_mean_df, paste0('RES2/data2_2_mean_', c_sub_name, '_', c_vari,'.csv'), row.names = FALSE)
  }
}

cat('========================step 4: data for regression==========================\n')
#up2024_0528_18:00
#get index_1 by adding data2_2_mean(summarize data averaged by day)

for(c_sub_name in subs_name){
  for(c_vari in varis){
    for(ii in times_set){
      c_name <- paste0('mean1_', c_sub_name, '_', c_vari, '_times_', ii)
      index_1[[c_name]] <- data2_2_mean[[c_sub_name]][[c_vari]][,ii]
    }
  }
}

write.csv(index_1, paste0('RES2/index_1b.csv'), row.names = FALSE)

index_1_set_1 <- c(12,15)
index_1c <- index_1[index_1_set_1]
write.csv(index_1c, paste0('RES2/index_1c.csv'), row.names = FALSE)

#======================================================
#up2024_0528_18:00
#get index_2 by adding data2_2_ori(original data compared to reference)

for(c_vari in varis){
  for(ii in times_set){
    for(kk in days_ori){
      c_name <- paste0('ORI_', c_vari, '_time', ii, '_day', kk)
      index_2[[c_name]] <- data2_2_ori[[c_vari]][[ii]][,kk]
    }
  }
}

write.csv(index_2, paste0('RES2/index_2.csv'), row.names = FALSE)

#==============================================
#up2024_0709_13:50
#adjust distance intervals

seq_a <- seq(1,50,1) #to_be_set_seq(seq(1,49,2)/seq(1,50,1))
seq_b <- seq(1,300,1) #to_be_set_seq(seq(1,299,2)/seq(1,300,1))
seq_c <- list()
seq_c[['ORI']] <- seq(1,300,1) #to_be_set_seq(seq(1,299,2)/seq(1,300,1))
seq_c[['NOR']] <- seq(1,200,1) #to_be_set_seq(seq(1,199,2)/seq(1,200,1))
seq_c[['HOT']] <- seq(1,100,1) #to_be_set_seq(seq(1,99,2)/seq(1,100,1))
index_c1 <- index_1[seq_b, ]
index_c2 <- index_2[seq_b, ]

#==============================================
#up2024_0709_13:55
#separate index_1/index_2 by streets

index_1s <- list()
index_2s <- list()
for(jj in 1: len_strs_mo){
  c_1 <- (jj - 1) * len_sites + 1
  c_2 <- jj * len_sites
  index_1s[[jj]] <- index_1[c_1:c_2,]
  index_2s[[jj]] <- index_2[c_1:c_2,]
}


index_c1s <- list()
index_c2s <- list()
for(jj in 1: len_strs_mo){
  c_1 <- (jj - 1) * len_sites_2 + 1
  c_2 <- jj * len_sites_2
  cat('index_c:',c_1,c_2,'\n')
  index_c1s[[jj]] <- index_c1[c_1:c_2,]
  index_c2s[[jj]] <- index_c2[c_1:c_2,]
}

cat('========================step 5: rce calculation==========================\n')
#up2024_0528_18:00
#define function: calculate RCE indexes 1a

rce_f <- function(f_1, f_2, f_3){
  f_rcd <- (-2 * f_2 - sqrt(4 * f_2^2 - 12 * f_1 * f_3))/(6 * f_1)
  f_rci <- f_1 * f_rcd^3 + f_2 * f_rcd^2 +  f_3 * f_rcd
  f_res <- list()
  f_res[['rcd']] <- f_rcd
  f_res[['rci']] <- f_rci
  return(f_res)
}

#refer to: Park, C. Y., Lee, D. K., Asawa, T., Murakami, A., Kim, H. G., Lee, M. K., & Lee, H. S. (2019). 
#Influence of urban form on the cooling effect of a small urban river. Landscape and urban planning, 183, 26-35.

#==============================================
#up2024_0528_18:00
#define function: calculate RCE indexes 1b

rce_1_f <- function(f_1, f_2){
  f_rce_1 <- lm(f_1 ~ poly(f_2, 3, raw = TRUE))
  f_rce_2 <- summary(f_rce_1)
  f_rce_coe <- rep(0,4)
  f_rce_coe[1] <- f_rce_2$coefficients[1,1]
  f_rce_coe[2] <- f_rce_2$coefficients[2,1]
  f_rce_coe[3] <- f_rce_2$coefficients[3,1]
  f_rce_coe[4] <- f_rce_2$coefficients[4,1]
  f_rci_1 <- rce_f(f_rce_coe[2], f_rce_coe[3], f_rce_coe[4])$rci
  f_rcd_1 <- rce_f(f_rce_coe[2], f_rce_coe[3], f_rce_coe[4])$rcd
  
  f_res <- list()
  f_res[['model_1']] <- f_rce_1
  f_res[['model_2']] <- f_rce_2
  f_res[['model_coe']] <- f_rce_coe
  f_res[['model_rci_1']] <- f_rci_1
  f_res[['model_rcd_1']] <- f_rcd_1
  return(f_res)
}

#==============================================
#up2024_0709_13:55
#define function: calculate RCE indexes 2

rce_2_f <- function(f_1){
  f_interval <- 3 #to_be_set
  f_1_len <- length(f_1)
  f_1_len2 <- f_1_len - f_interval
  for(nn in 1: f_1_len2){
    c_1 <- nn + 1
    c_2 <- nn + f_interval
    if(f_1[nn] > max(f_1[c_1: c_2])){
      f_rcd <- nn
      f_rci <- f_1[f_rcd] - f_1[1]
      f_crci <- f_1[f_rcd] * f_rcd - sum(f_1[1:f_rcd])
      break
    }else if(nn == f_1_len2){
      f_rcd <- -9999 
      f_rci <- -9999 
      f_crci <- -9999
      print('ERROR')
    }else{}
  }
  
  f_res <- list()
  f_res[['RCD']] <- f_rcd
  f_res[['RCI']] <- f_rci
  f_res[['CRCI']] <- f_crci
  return(f_res)
}

#==============================================
#up2024_0617_20:30
#RCE calculation based on method 2(single day)

rce_rb1 <- list()
for(c_vari in varis){
  rce_rb1[[c_vari]] <- list()
  for(ii in times_set){
    rce_rb1[[c_vari]][[ii]] <- list()
    for(jj in strs_mo){
      c_1 <- (jj - 1) * len_sites + 1
      c_2 <- jj * len_sites
      rce_rb1[[c_vari]][[ii]][[jj]] <- list()
      for(kk in days_ori){
        rce_rb1[[c_vari]][[ii]][[jj]][[kk]] <- rce_2_f(data2_2_ori[[c_vari]][[ii]][c_1: c_2, kk][seq_a])
      }
    }
  }
}

#==============================================
#up2024_0617_20:30
#RCE calculation based on method 2(daily mean)

rce_rb2 <- list()
for(c_sub_name in subs_name){
  rce_rb2[[c_sub_name]] <- list()
  for(c_vari in varis){
    rce_rb2[[c_sub_name]][[c_vari]] <- list()
    for(ii in times_set){
      rce_rb2[[c_sub_name]][[c_vari]][[ii]] <- list()
      for(jj in strs_mo){
        c_1 <- (jj - 1) * len_sites + 1
        c_2 <- jj * len_sites
        rce_rb2[[c_sub_name]][[c_vari]][[ii]][[jj]] <- rce_2_f(data2_2_mean[[c_sub_name]][[c_vari]][c_1: c_2,ii][seq_a])
      }
    }
  }
}

#==============================================
#up2024_0528_22:00
#calculate rce indexes(method 1, each each variable, each time, each street, each day)

rce_r1 <- list()
for(c_vari in varis){
  rce_r1[[c_vari]] <- list()
    for(ii in times_set){
      rce_r1[[c_vari]][[ii]] <- list()
      for(jj in strs_mo){
        c_1 <- (jj - 1) * len_sites + 1
        c_2 <- jj * len_sites
        rce_r1[[c_vari]][[ii]][[jj]] <- list()        
        for(kk in days_ori){
          rce_r1[[c_vari]][[ii]][[jj]][[kk]] <- rce_1_f(data2_2_ori[[c_vari]][[ii]][c_1: c_2, kk][seq_a], dis_1[seq_a])
      }
    }
  }
}

#==============================================
#up2024_0529_10:00
#calculate rce indexes(method 1, each sub, each variable, each time, each street, days together)

data2_2v <- list()
rce_r2 <- list()
for(c_sub_name in subs_name){
  data2_2v[[c_sub_name]] <- list()
  rce_r2[[c_sub_name]] <- list()
  for(c_vari in varis){
    data2_2v[[c_sub_name]][[c_vari]] <- list()
    rce_r2[[c_sub_name]][[c_vari]] <- list()
    for(ii in times_set){
      data2_2v[[c_sub_name]][[c_vari]][[ii]] <- list()
      rce_r2[[c_sub_name]][[c_vari]][[ii]] <- list()
      for(jj in strs_mo){
        c_1 <- (jj - 1) * len_sites + 1
        c_2 <- jj * len_sites
        data2_2v[[c_sub_name]][[c_vari]][[ii]][[jj]] <- as.vector(data2_2[[c_sub_name]][[c_vari]][[ii]][c_1:c_2,])
        rce_r2[[c_sub_name]][[c_vari]][[ii]][[jj]] <- rce_1_f(data2_2v[[c_sub_name]][[c_vari]][[ii]][[jj]], dis_3[[c_sub_name]])
      }
    }
  }
}

#==============================================
#up2024_0529_10:00
#calculate rce indexes(method 1, days merged, streets together; each sub, each variable, each time)

rce_r3 <- list()
for(c_sub_name in subs_name){
  rce_r3[[c_sub_name]] <- list()
  for(c_vari in varis){
    rce_r3[[c_sub_name]][[c_vari]] <- list()
    for(ii in times_set){
      rce_r3[[c_sub_name]][[c_vari]][[ii]] <- rce_1_f(data2_2_mean[[c_sub_name]][[c_vari]][, ii], dis_2)
    }
  }
}

#=====================================================================
cat('========================step b1: regression_b==========================\n')
#ing2024_0721_20:26

regreb_6f <- function(f_sub, f_vari, f_time, f_bydis_num, f_reg_se){
  f_days <- days_trans_f(f_sub)
  f_order <- bydis[[f_bydis_num]]
  f_y_1 <- c()
  for(f_day in f_days){
    f_name <- paste0('ORI_', f_vari, '_time', f_time, '_day', f_day)
    f_y_1 <- c(f_y_1, index_2[[f_name]][f_order])
  }
  
  f_r2a <- rep(0, length(f_reg_se))
  f_x_2 <- matrix(0, nrow = bydis_itv * len_strs_mo * length(f_days), ncol = length(f_reg_se))
  f_model_1 <- list()
  for(mm in 1:length(f_reg_se)){
    c_reg <- f_reg_se[mm] 
    f_x_1 <- rep(index_2[, c_reg][f_order], length(f_days))
    f_model_1[[mm]] <- lm(f_y_1 ~ f_x_1)
    f_r2a[mm] <- summary(f_model_1[[mm]])$r.squared
    f_x_2[,mm] <- f_x_1
  }
  
  f_z_1 <- cbind(f_x_2, f_y_1)
  f_z_2 <- as.data.frame(f_z_1)
  f_colnames <- append(cname_index_2, 'y_regreb_6')
  colnames(f_z_2) <- f_colnames
  f_model_for <- as.formula(paste0('y_regreb_6', ' ~ ', cname_index_3)) 
  f_model_res <- lm(f_model_for, data = f_z_2)
  f_model_sum <- summary(f_model_res)
  f_r2b <- f_model_sum$r.squared
  
  f_res <- list()
  f_res[['days']] <- f_days
  f_res[['order']] <- f_order
  f_res[['model_sum']] <- f_model_sum
  f_res[['y_1']] <- f_y_1
  f_res[['x_2']] <- f_x_2
  f_res[['z_2']] <- f_z_2
  
  f_res[['r2a']] <- f_r2a
  f_res[['r2b']] <- f_r2b
  return(f_res)
}

#========================================
#ing

regreb_6f_sub <- 'ORI'

regreb_6f_r2 <- list()
for(c_vari in varis){
  regreb_6f_r2[[c_vari]] <- list()
  for(ii in times_set){
    regreb_6f_r2[[c_vari]][[ii]] <- matrix(0, nrow = length(reg_se), ncol = bydis_num)
    for(mm in 1: bydis_num){
      regreb_6f_r2[[c_vari]][[ii]][,mm] <- regreb_6f(regreb_6f_sub, c_vari, ii, mm, reg_se)$r2a
    }
    colnames(regreb_6f_r2[[c_vari]][[ii]]) <- cname_index_2
    write.csv(regreb_6f_r2[[c_vari]][[ii]], paste0('RES3/Fig_r2_', regreb_6f_sub, '_', c_vari, '_time', ii, '.csv'), row.names = FALSE)
  }
}
