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
#=================================================================
#up2024_0528_17:00

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

ele_2a <- read.csv('ele_2_table.csv')
ele_2b <- as.matrix(ele_2a)
ele_2c <- matrix(ele_2b, nrow = len_sites * len_strs_mo, ncol = 1)

index_1 <- read.csv('index_1m_df2.csv')[1:300,]
index_1$ele_2 <- ele_2c
index_2 <- index_1

#=================================================================
#up2024_0531_17:00

reg_se_sum <- list()
reg_se_sum[[1]] <- c(1,3,4)
reg_se_sum[[2]] <- c(12)

reg_set <- 1 #to_be_set_key
reg_se <- reg_se_sum[[reg_set]]
#=================================================================
#up2024_0528_17:00
#set independent variables for regression

cname_index_1 <- colnames(index_1)
cname_index_2 <- cname_index_1[reg_se] #to_be_set_key
cname_index_3 <- paste(cname_index_2, collapse = ' + ')

#============================define functions=====================================
#up2024_0528_17:00
#define function: from subs_name to subs

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

dis_1 <- seq(10, 500, 10) #to_be_set
dis_2 <- rep(dis_1, len_strs_mo)
dis_3 <- list()
dis_3[['ORI']] <- rep(dis_1, len_days_ori)
dis_3[['NOR']] <- rep(dis_1, len_days_nor)
dis_3[['HOT']] <- rep(dis_1, len_days_hot)

cat('========================step 1: basic data(data2_2)==========================\n')
#up2024_0528_17:00

data2_2_ori <- list()
for(c_vari in varis){
  data2_2_ori[[c_vari]] <- list()
  for(ii in times_set){
    data2_2_ori[[c_vari]][[ii]] <- as.matrix(read.csv(paste0('RES2/recb_2_', c_vari, '_', ii,'_df.csv'))) #to_be_set_key
  } 
}

#=========================================================
#up2024_0528_18:00

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

cat('========================step 2: calculate mean==========================\n')

#up2024_0528_18:00
#function: input weather data(mean)

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
#summarize data by averaging days

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

cat('========================step 3: data for regression==========================\n')
#up2024_0528_18:00
#get index_1

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
#get index_2

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
#up2024_0528_18:00
#separate index_1/index_2 by streets

index_1s <- list()
index_2s <- list()
for(ii in 1: len_strs_mo){
  c_1 <- (ii - 1) * len_sites + 1
  c_2 <- ii * len_sites
  index_1s[[ii]] <- index_1[c_1:c_2,]
  index_2s[[ii]] <- index_2[c_1:c_2,]
}

cat('========================step 4: rce calculation==========================\n')
#up2024_0528_18:00
#define function: calculate RCE indexes

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
#define function: calculate rce indexes

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
#up2024_0528_22:00
#calculate rce indexes(each each variable, each time, each street, each day)

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
          rce_r1[[c_vari]][[ii]][[jj]][[kk]] <- rce_1_f(data2_2_ori[[c_vari]][[ii]][c_1:c_2,kk], dis_1)
      }
    }
  }
}

#==============================================
#up2024_0529_10:00
#calculate rce indexes(each sub, each variable, each time, each street, days together)

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
#calculate rce indexes(days merged, streets together; each sub, each variable, each time)

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

cat('========================step 5: regression==========================\n')
#up2024_0529_10:00
#define function: regression(each variable, each time, each street, each day)

regre_1f <- function(f_vari, f_time, f_str, f_day){
  f_name <- paste0('ORI_', f_vari, '_time', f_time,'_day',f_day)
  f_model_for <- as.formula(paste0(f_name, ' ~ ', cname_index_3)) 
  f_model_res <- lm(f_model_for, data = index_2s[[f_str]])
  f_model_sum <- summary(f_model_res)
  f_r2 <- f_model_sum$r.squared
  f_r2_adj <- f_model_sum$adj.r.squared
  f_coe <- f_model_res$coefficients
  
  f_res <- list()
  f_res[['r2']] <- f_r2
  f_res[['r2_adj']] <- f_r2_adj
  f_res[['coe']] <- f_coe
  return(f_res)
}

#==============================================
#up2024_0529_10:00
#regression(each variable, each time, each street, each day)

regre_r1 <- list()
for(c_vari in varis){
  regre_r1[[c_vari]] <- list()
  for(ii in times_set){
    regre_r1[[c_vari]][[ii]] <- list()
    for(jj in strs_mo){
      regre_r1[[c_vari]][[ii]][[jj]] <- list()
      for(kk in days_ori){
        regre_r1[[c_vari]][[ii]][[jj]][[kk]] <- regre_1f(c_vari, ii, jj, kk)
      }
    }
  }
}

#==============================================
#up2024_0529_10:00
#define function: regression(each variable, each time, streets together, each day)

regre_2f <- function(f_vari, f_time, f_day){
  f_name <- paste0('ORI_', f_vari, '_time', f_time,'_day',f_day)
  f_model_for <- as.formula(paste0(f_name, ' ~ ', cname_index_3)) 
  f_model_res <- lm(f_model_for, data = index_2)
  f_model_sum <- summary(f_model_res)
  f_r2 <- f_model_sum$r.squared
  f_r2_adj <- f_model_sum$adj.r.squared
  f_coe <- f_model_res$coefficients
  
  f_res <- list()
  f_res[['r2']] <- f_r2
  f_res[['r2_adj']] <- f_r2_adj
  f_res[['coe']] <- f_coe
  return(f_res)
}

#==============================================
#up2024_0529_10:00
#regression for(each variable, each time, streets together, each day)

regre_r2 <- list()
for(c_vari in varis){
  regre_r2[[c_vari]] <- list()
  for(ii in times_set){
    regre_r2[[c_vari]][[ii]] <- list()
    for(kk in days_ori){
      regre_r2[[c_vari]][[ii]][[kk]] <- regre_2f(c_vari, ii, kk)
    }
  }
}

#==============================================
#up2024_0529_10:00
#define function: regression(each sub, each variable, each time, each street, days merged)

regre_3f <- function(f_sub_name, f_vari, f_time, f_str){
  f_name <- paste0('mean1_', f_sub_name, '_', f_vari, '_times_', f_time)
  f_model_for <- as.formula(paste0(f_name, ' ~ ', cname_index_3)) 
  f_model_res <- lm(f_model_for, data = index_1s[[f_str]])
  f_model_sum <- summary(f_model_res)
  f_r2 <- f_model_sum$r.squared
  f_r2_adj <- f_model_sum$adj.r.squared
  f_coe <- f_model_res$coefficients
  
  f_res <- list()
  f_res[['r2']] <- f_r2
  f_res[['r2_adj']] <- f_r2_adj
  f_res[['coe']] <- f_coe
  return(f_res)
}

#==============================================
#up2024_0529_10:00
#regression(each sub, each variable, each time, each street, days merged)

regre_r3 <- list()
for(c_sub_name in subs_name){
  regre_r3[[c_sub_name]] <- list()
  for(c_vari in varis){
    regre_r3[[c_sub_name]][[c_vari]] <- list()
    for(ii in times_set){
      regre_r3[[c_sub_name]][[c_vari]][[ii]] <- list()
      for(jj in strs_mo){
        regre_r3[[c_sub_name]][[c_vari]][[ii]][[jj]] <- regre_3f(c_sub_name, c_vari, ii, jj)
      }
    }
  }
}

#==============================================
#up2024_0529_10:00
#define function: regression based on 'index_1'(each sub, each variable, each time, each streets together, days merged)

regre_4f <- function(f_sub_name, f_vari, f_time){
  f_name <- paste0('mean1_', f_sub_name, '_', f_vari, '_times_', f_time)
  f_model_for <- as.formula(paste0(f_name, ' ~ ', cname_index_3)) 
  f_model_res <- lm(f_model_for, data = index_1)
  f_model_sum <- summary(f_model_res)
  f_r2 <- f_model_sum$r.squared
  f_r2_adj <- f_model_sum$adj.r.squared
  f_coe <- f_model_res$coefficients
  
  f_res <- list()
  f_res[['r2']] <- f_r2
  f_res[['r2_adj']] <- f_r2_adj
  f_res[['coe']] <- f_coe
  return(f_res)
}

#==============================================
#up2024_0529_10:00
#regression based on 'index_1'(each sub, each variable, each time, each streets together, days merged)

regre_r4 <- list()
for(c_sub_name in subs_name){
  regre_r4[[c_sub_name]] <- list()
  for(c_vari in varis){
    regre_r4[[c_sub_name]][[c_vari]] <- list()
    for(ii in times_set){
      regre_r4[[c_sub_name]][[c_vari]][[ii]] <- regre_4f(c_sub_name, c_vari, ii)
    }
  }
}

#==============================================
#up2024_0531_08:00
#define function: each sub, each variable, each time, each street, days together

regre_5f <- function(f_sub_name, f_vari, f_time, f_str){
  f_days <- days_trans_f(f_sub_name)
  f_y_1 <- c()
  for(f_day in f_days){
    f_name <- paste0('ORI_', f_vari, '_time', f_time,'_day',f_day)
    f_y_1 <- c(f_y_1, index_2s[[f_str]][[f_name]])
  }
  f_x_1 <- index_2s[[f_str]][, reg_se]
  if(length(reg_se) == 1){
    f_x_2 <- rep(f_x_1, length(f_days))
    f_z <- as.data.frame(cbind(f_x_2, f_y_1))
    colnames(f_z) <- c(colnames(index_2s[[f_str]])[reg_se], 'f_y_1')
  }else{
    f_x_2 <- do.call(rbind, rep(list(f_x_1), length(f_days)))
    f_z <- as.data.frame(cbind(f_x_2, f_y_1))     
  }
  f_model_for <- as.formula(paste0('f_y_1 ~ ', cname_index_3)) 
  f_model_res <- lm(f_model_for, data = f_z)
  f_model_sum <- summary(f_model_res)
  f_r2 <- f_model_sum$r.squared
  f_r2_adj <- f_model_sum$adj.r.squared
  f_coe <- f_model_res$coefficients
  
  f_res <- list()
  f_res[['r2']] <- f_r2
  f_res[['r2_adj']] <- f_r2_adj
  f_res[['coe']] <- f_coe
  return(f_res)
}

#==============================================
#up2024_0531_08:00

regre_r5 <- list()
for(c_sub_name in subs_name){
  regre_r5[[c_sub_name]] <- list()
  for(c_vari in varis){
    regre_r5[[c_sub_name]][[c_vari]] <- list()
    for(ii in times_set){
      regre_r5[[c_sub_name]][[c_vari]][[ii]] <- list()
      for(jj in strs_mo){
        regre_r5[[c_sub_name]][[c_vari]][[ii]][[jj]] <- regre_5f(c_sub_name, c_vari, ii, jj)
      }
    }
  }
}

#==============================================
#up2024_0531_08:00
#define function: each sub, each variable, each time, streets together, days together

regre_6f <- function(f_sub_name, f_vari, f_time){
  f_days <- days_trans_f(f_sub_name)
  f_y_1 <- c()
  
  for(f_day in f_days){
    f_name <- paste0('ORI_', f_vari, '_time', f_time,'_day',f_day)
    f_y_1 <- c(f_y_1, index_2[[f_name]])
  }
  f_x_1 <- index_2[, reg_se]
  if(length(reg_se) == 1){
    f_x_2 <- rep(f_x_1, length(f_days))
    f_z <- as.data.frame(cbind(f_x_2, f_y_1))
    colnames(f_z) <- c(colnames(index_2)[reg_se], 'f_y_1')
  }else{
    f_x_2 <- do.call(rbind, rep(list(f_x_1), length(f_days)))
    f_z <- as.data.frame(cbind(f_x_2, f_y_1))     
  }
  f_model_for <- as.formula(paste0('f_y_1 ~ ', cname_index_3)) 
  f_model_res <- lm(f_model_for, data = f_z)
  f_model_sum <- summary(f_model_res)
  f_r2 <- f_model_sum$r.squared
  f_r2_adj <- f_model_sum$adj.r.squared
  f_coe <- f_model_res$coefficients
  
  f_res <- list()
  f_res[['r2']] <- f_r2
  f_res[['r2_adj']] <- f_r2_adj
  f_res[['coe']] <- f_coe
  return(f_res)
}

#====================
#up2024_0531_08:00

regre_r6 <- list()
for(c_sub_name in subs_name){
  regre_r6[[c_sub_name]] <- list()
  for(c_vari in varis){
    regre_r6[[c_sub_name]][[c_vari]] <- list()
    for (ii in times_set){
      regre_r6[[c_sub_name]][[c_vari]][[ii]] <- regre_6f(c_sub_name, c_vari, ii)
    }
  }
}

cat('========================step 6: figures==========================\n')
#up2024_0529_10:00
#define function: fig(each variable, each time, each street, each day)

fig_1f <- function(f_vari, f_time){
  jpeg(paste0('FIG2/fig1_', f_vari, '_', f_time,'.jpg'), width = 1800, height = 1200)
  par(mfrow = c(len_strs_mo, len_days_ori)) 
  for(jj in strs_mo){
    for(kk in days_ori){
      f_1 <- (jj - 1) * len_sites + 1
      f_2 <- jj * len_sites
      f_y <- data2_2_ori[[f_vari]][[f_time]][f_1:f_2,kk]
      plot(dis_1, f_y, main = paste0('str: ', jj, '_day:', kk), 
           xlab = 'Distance (m)', ylab = paste0('d ', f_vari), pch = 19, col = 'skyblue')
      f_fit1 <- lm(f_y ~ poly(dis_1, 1, raw = TRUE))
      f_fit2 <- lm(f_y ~ poly(dis_1, 2, raw = TRUE))
      curve(predict(f_fit1, newdata = data.frame(dis_1 = x)), add = TRUE, col = 'red', lty = 2)
      #curve(predict(f_fit2, newdata = data.frame(dis_1 = x)), add = TRUE, col = 'red', lty = 2)
    }
  }
  dev.off() 
  f_res <- list()
  f_res[['fit1']] <- f_fit1
  f_res[['fit2']] <- f_fit2
  return(f_res)
}

#==============================================
#up2024_0529_10:00
#fig(each variable, each time, each street, each day)

fig_1_fit <- list()
for(c_vari in varis){
  fig_1_fit[[c_vari]] <- list()
  for(ii in times_set){
    fig_1_fit[[c_vari]][[ii]] <- fig_1f(c_vari, ii)
  }
}

#==============================================
#up2024_0530_17:00
#fig(each variable, each time, streets together, each day)

fig_2f <- function(f_vari){
  jpeg(paste0('FIG2/fig2_', f_vari, '.jpg'), width = 1800, height = 1200)
  par(mfrow = c(len_times_set, len_days_ori)) 
  for(ii in times_set){
    for(kk in days_ori){
      f_y <- data2_2_ori[[f_vari]][[ii]][,kk]
      plot(dis_2, f_y, main = paste0('time: ', ii, '_day:', kk), 
           xlab = 'Distance (m)', ylab = paste0('d ', f_vari), pch = 19, col = 'skyblue')
      f_fit1 <- lm(f_y ~ poly(dis_2, 1, raw = TRUE))
      f_fit2 <- lm(f_y ~ poly(dis_2, 2, raw = TRUE))
      curve(predict(f_fit1, newdata = data.frame(dis_2 = x)), add = TRUE, col = 'red', lty = 2)
      curve(predict(f_fit2, newdata = data.frame(dis_2 = x)), add = TRUE, col = 'red', lty = 2)
    }
  }
  dev.off() 
  f_res <- list()
  f_res[['fit1']] <- f_fit1
  f_res[['fit2']] <- f_fit2
  return(f_res)
}

#==============================================
#up2024_0530_17:00
#fig(each variable, each time, each street, each day)

fig_2_fit <- list()
for(c_vari in varis){
  fig_2_fit[[c_vari]] <- fig_2f(c_vari)
}

#==============================================
#up2024_0530_17:00
#each sub, each variable, each time, each street, days merged

fig_3f <- function(f_sub, f_vari){
  jpeg(paste0('FIG2/fig3_', f_sub, '_', f_vari,'.jpg'), width = 1800, height = 1200)
  par(mfrow = c(len_times_set, len_strs_mo)) 
  for(ii in times_set){
    for(jj in strs_mo){
      f_1 <- (jj - 1) * len_sites + 1
      f_2 <- jj * len_sites
      f_y <- data2_2_mean[[f_sub]][[f_vari]][f_1:f_2,ii]
      plot(dis_1, f_y, main = paste0('time: ', ii, '_str:', jj), 
           xlab = 'Distance (m)', ylab = paste0(f_sub, '_d ', f_vari), pch = 19, col = 'skyblue')
      f_fit1 <- lm(f_y ~ poly(dis_1, 1, raw = TRUE))
      f_fit2 <- lm(f_y ~ poly(dis_1, 2, raw = TRUE))
      curve(predict(f_fit1, newdata = data.frame(dis_1 = x)), add = TRUE, col = 'red', lty = 2)
      curve(predict(f_fit2, newdata = data.frame(dis_1 = x)), add = TRUE, col = 'red', lty = 2)
    }
  }
  dev.off() 
  f_res <- list()
  f_res[['fit1']] <- f_fit1
  f_res[['fit2']] <- f_fit2
  return(f_res)
}

#==============================================
#up2024_0530_17:00
#fig(each variable, each time, each street, each day)

fig_3_fit <- list()
for(c_sub_name in subs_name){
  fig_3_fit[[c_sub_name]] <- list()
  for(c_vari in varis){
    fig_3_fit[[c_sub_name]][[c_vari]] <- fig_3f(c_sub_name, c_vari)
  }
}

#==============================================
#up2024_0529_10:00
#each sub, each time, each variable, streets together, days merged

windowsFonts(calibri = windowsFont('Calibri'))

fig_4f <- function(f_vari){
  jpeg(paste0('FIG2/fig4_', f_vari,'.jpg'), width = 1500, height = 1500, res = 300)
  par(mfrow = c(len_subs, len_times_set), oma = c(3, 3, 3, 3), mar = c(2.8,2.8,2.8,2.8),  mgp = c(2, 0.5, 0)) 
  for(c_sub_name in subs_name){
    for(ii in times_set){
      f_y <- data2_2_mean[[c_sub_name]][[f_vari]][,ii]
      plot(dis_2, f_y, main = paste0('sub:', c_sub_name, '__time: ', ii), xlab = 'Distance (m)', ylab = paste0('d ', f_vari), 
           col = 'skyblue', family = 'calibri', cex.main = 0.5, cex.lab = 0.6, cex.axis = 0.6, pch = 19, cex = 0.2)
      f_fit1 <- lm(f_y ~ poly(dis_2, 1, raw = TRUE))
      f_fit2 <- lm(f_y ~ poly(dis_2, 2, raw = TRUE))
      curve(predict(f_fit1, newdata = data.frame(dis_2 = x)), add = TRUE, col = 'red', lty = 2)
      curve(predict(f_fit2, newdata = data.frame(dis_2 = x)), add = TRUE, col = 'green', lty = 2)
      #grid(nx = NULL, ny = NULL, col = 'gray', lty = 'dotted')    
    }
  }
  dev.off()
  f_res <- list()
  f_res[['fit1']] <- f_fit1
  f_res[['fit2']] <- f_fit2
  return(f_res)
}

#==============================================
#up2024_0529_10:00

fig_4_fit <- list()
for(c_vari in varis){
  fig_4_fit[[c_vari]] <- fig_4f(c_vari)
}

#==============================================
#up2024_0529_10:00
#each sub, each variable, each time, each street, days together

fig_5f <- function(f_sub, f_vari){
  jpeg(paste0('FIG2/fig5_', f_sub, '_', f_vari,'.jpg'), width = 1800, height = 1200)
  par(mfrow = c(len_times_set, len_strs_mo)) 
  for(ii in times_set){
    for(jj in strs_mo){
      f_1 <- (jj - 1) * len_sites + 1
      f_2 <- jj * len_sites
      f_x <- dis_3[[f_sub]]
      f_y <- as.vector(data2_2_ori[[f_vari]][[ii]][f_1:f_2,days_trans_f(f_sub)])
      plot(f_x, f_y, main = paste0('time:', ii, '_str: ', jj), 
           xlab = 'Distance (m)', ylab = paste0('d ', f_vari), pch = 19, col = 'skyblue')
      f_fit1 <- lm(f_y ~ poly(f_x, 1, raw = TRUE))
      f_fit2 <- lm(f_y ~ poly(f_x, 2, raw = TRUE))
      curve(predict(f_fit1, newdata = data.frame(f_x = x)), add = TRUE, col = 'red', lty = 2)
      curve(predict(f_fit2, newdata = data.frame(f_x = x)), add = TRUE, col = 'green', lty = 2)
    }
  }
  dev.off() 
  f_res <- list()
  f_res[['fit1']] <- f_fit1
  f_res[['fit2']] <- f_fit2
  return(f_res)
}

#==============================================
#up2024_0529_10:00

fig_5_fit <- list()
for(c_sub_name in subs_name){
  fig_5_fit[[c_sub_name]] <- list()
  for(c_vari in varis){
    fig_5_fit[[c_sub_name]][[c_vari]] <- fig_5f(c_sub_name, c_vari)
  }
}


#==============================================
#up2024_0530_17:00
#each sub, each variable, each time, streets together, days together

fig_6f <- function(f_vari){
  jpeg(paste0('FIG2/fig6_', f_vari,'.jpg'), width = 1800, height = 1200)
  par(mfrow = c(len_subs, len_varis)) 
  for(c_sub_name in subs_name){
    for(ii in times_set){
      f_x <- rep(dis_1, len_strs_mo * length(days_trans_f(c_sub_name)))
      f_y <- as.vector(data2_2_ori[[f_vari]][[ii]][,days_trans_f(c_sub_name)])
      plot(f_x, f_y, main = paste0('vari: ', c_vari), 
           xlab = 'Distance (m)', ylab = paste0('sub:', c_sub_name, '_time:', ii), pch = 19, col = 'skyblue')
      f_fit1 <- lm(f_y ~ poly(f_x, 1, raw = TRUE))
      f_fit2 <- lm(f_y ~ poly(f_x, 2, raw = TRUE))
      curve(predict(f_fit1, newdata = data.frame(f_x = x)), add = TRUE, col = 'red', lty = 2)
      curve(predict(f_fit2, newdata = data.frame(f_x = x)), add = TRUE, col = 'green', lty = 2)
    }
  }
  dev.off() 
  f_res <- list()
  f_res[['fit1']] <- f_fit1
  f_res[['fit2']] <- f_fit2
  return(f_res)
}

#==============================================
#up2024_0530_17:00

fig_6_fit <- list()
for(c_vari in varis){
  fig_6_fit[[c_vari]] <- fig_6f(c_vari)
}

#==============================================
#up2024_0531_21:00

time_set <- 1 #to_be_set
str_set <- 1 #to_be_set
day_set <- 1 #to_be_set
check_1 <- regre_r1$TP[[time_set]][[str_set]][[day_set]]$coe
check_2 <- regre_r4$ORI$TP[[time_set]]$coe


#==============================================
#up2024_0601_21:30

fig1b_list_f <- function(f_vari, f_time){
  f_fig1b_list <- list()
  nn <- 0
  for(jj in strs_mo){
    for(kk in days_ori){
      nn <- nn + 1
      fc_1 <- (jj - 1) * len_sites + 1
      fc_2 <- jj * len_sites
      f_fig1b_list[[nn]] <- data.frame(
        xx = dis_1,
        yy = data2_2_ori[[f_vari]][[f_time]][fc_1:fc_2,kk]
      )
    }
  }
  return(f_fig1b_list)
}

#==============================================
#up2024_0601_21:30

fig1b_plot_f <- function(f_data, f_index){
  f_str <- (f_index - 1)%/% len_days_ori + 1
  f_day <- (f_index - 1)%% len_days_ori + 1
  f_model <- lm(yy ~ xx, data = f_data)
  f_interc <- coef(f_model)[1]
  f_slope <- coef(f_model)[2]
  f_eq <- paste0('y = ', round(f_interc, 2), ' + ', round(f_slope, 2), ' * x')
  
  f_p <- ggplot(f_data, aes(x = xx, y = yy)) +
    geom_point() +
    geom_smooth(method = 'lm', se = FALSE, col = 'blue') +
    annotate('text', x = Inf, y = Inf, label = f_eq, hjust = 1.1, vjust = 1.5, size = 5, color = 'red') +
    ggtitle(paste('str:', f_str,'_day:',f_day)) +
    theme_minimal()
  
  return(f_p)
}

#==============================================
#up2024_0602_11:30

fig1b_res_f <- function(f_vari, f_time){
  f_data_fig1b_list_1 <- fig1b_list_f(f_vari, f_time)
  f_plots_fig1b_1 <- lapply(seq_along(f_data_fig1b_list_1), function(nn) fig1b_plot_f(f_data_fig1b_list_1[[nn]], nn))
  f_comb_plots_fig1b_1 <- grid.arrange(grobs = f_plots_fig1b_1, ncol = len_days_ori, nrow = len_strs_mo)
  ggsave(paste0('FIG2/comb_plots_fig1b_1', f_vari, '_', f_time,'.jpg'), plot = f_comb_plots_fig1b_1, width = 15, height = 15, dpi = 300)
  return(f_comb_plots_fig1b_1)
}

#fig1b_res <- list()
#for(c_vari in varis){
#  fig1b_res[[c_vari]] <- list()
#  for(ii in times_set){
#    fig1b_res[[c_vari]][[ii]] <- fig1b_res_f(c_vari,ii)
#  }
#}

check_1 <- fig1b_res_f('TP',2) #to_be_set
#==============================================
#up2024_0601_21:30

fig2b_list_f <- function(f_vari){
  f_fig2b_list <- list()
  nn <- 0
  for(ii in times_set){
    for(kk in days_ori){
      nn <- nn + 1
      f_fig2b_list[[nn]] <- data.frame(
        xx = dis_2,
        yy = data2_2_ori[[f_vari]][[ii]][,kk]
      )     
    }
  }
  return(f_fig2b_list)
}

#==============================================
#up2024_0601_21:30

fig2b_plot_f <- function(f_data, f_index){
  f_time <- (f_index - 1)%/% len_days_ori + 1
  f_day <- (f_index - 1)%% len_days_ori + 1
  f_model <- lm(yy ~ xx, data = f_data)
  f_interc <- coef(f_model)[1]
  f_slope <- coef(f_model)[2]
  f_eq <- paste0('y = ', round(f_interc, 2), ' + ', round(f_slope, 2), ' * x')
  
  f_p <- ggplot(f_data, aes(x = xx, y = yy)) +
    geom_point() +
    geom_smooth(method = 'lm', se = FALSE, col = 'blue') +
    annotate('text', x = Inf, y = Inf, label = f_eq, hjust = 1.1, vjust = 1.5, size = 5, color = 'red') +
    ggtitle(paste('time:', f_time, 'day:',f_day)) +
    theme_minimal()
  
  return(f_p)
}

#==============================================
#up2024_0602_11:30

fig2b_res_f <- function(f_vari){
  cat(f_vari, '\n')
  f_data_fig2b_list_1 <- fig2b_list_f(f_vari)
  f_plots_fig2b_1 <- lapply(seq_along(f_data_fig2b_list_1), function(nn) fig2b_plot_f(f_data_fig2b_list_1[[nn]], nn))
  f_comb_plots_fig2b_1 <- grid.arrange(grobs = f_plots_fig2b_1, ncol = len_days_ori, nrow = len_times_set)
  ggsave(paste0('FIG2/comb_plots_fig2b_1', f_vari, '.jpg'), plot = f_comb_plots_fig2b_1, width = 15, height = 15, dpi = 300)
  return(f_comb_plots_fig2b_1)
}

#fig2b_res <- list()
#for(c_vari in varis){
#  fig2b_res[[c_vari]] <- fig2b_res_f(c_vari)
#}

#==============================================
#up2024_0601_21:30

fig3b_list_f <- function(f_sub, f_vari){
  f_fig3b_list <- list()
  nn <- 0
  for(ii in times_set){
    for(jj in strs_mo){
      nn <- nn + 1
      fc_1 <- (jj - 1) * len_sites + 1
      fc_2 <- jj * len_sites
      f_fig3b_list[[nn]] <- data.frame(
        xx = dis_1,
        yy = data2_2_mean[[f_sub]][[f_vari]][fc_1:fc_2,ii]
      )
    }
  }
  return(f_fig3b_list)
}

#==============================================
#up2024_0601_21:30

fig3b_plot_f <- function(f_data, f_index){
  f_time <- (f_index - 1)%/% len_strs_mo + 1
  f_str <- (f_index - 1)%% len_strs_mo + 1
  f_model <- lm(yy ~ xx, data = f_data)
  f_interc <- coef(f_model)[1]
  f_slope <- coef(f_model)[2]
  f_eq <- paste0('y = ', round(f_interc, 2), ' + ', round(f_slope, 2), ' * x')
  
  f_p <- ggplot(f_data, aes(x = xx, y = yy)) +
    geom_point() +
    geom_smooth(method = 'lm', se = FALSE, col = 'blue') +
    annotate('text', x = Inf, y = Inf, label = f_eq, hjust = 1.1, vjust = 1.5, size = 5, color = 'red') +
    ggtitle(paste('time:', f_time,'_str:',f_str)) +
    theme_minimal()
  
  return(f_p)
}

#==============================================
#up2024_0601_21:30

fig3b_res_f <- function(f_sub_name, f_vari){
  cat(f_sub_name, '_', f_vari, '\n')
  f_data_fig3b_list_1 <- fig3b_list_f(f_sub_name, f_vari)
  f_plots_fig3b_1 <- lapply(seq_along(f_data_fig3b_list_1), function(nn) fig3b_plot_f(f_data_fig3b_list_1[[nn]], nn))
  f_comb_plots_fig3b_1 <- grid.arrange(grobs = f_plots_fig3b_1, ncol = len_strs_mo, nrow = len_times_set)
  ggsave(paste0('FIG2/comb_plots_fig3b_1', f_sub_name, '_', f_vari,'.jpg'), plot = f_comb_plots_fig3b_1, width = 15, height = 15, dpi = 300)
  return(f_comb_plots_fig3b_1)
}

#fig3b_res <- list()
#for(c_sub_name in subs_name){
#  fig3b_res[[c_sub_name]] <- list()
#  for(c_vari in varis){
#    fig3b_res[[c_sub_name]][[c_vari]] <- fig3b_res_f(c_sub_name, c_vari)
#  }
#}

#==============================================
#up2024_0601_21:30

fig4b_list_f <- function(f_sub){
  f_fig4b_list <- list()
  nn <- 0
  for(c_vari in varis){
    for(ii in times_set){
      nn <- nn + 1
      f_fig4b_list[[nn]] <- data.frame(
        xx = dis_2,
        yy = data2_2_mean[[f_sub]][[c_vari]][,ii]
      )
    }
  }
  return(f_fig4b_list)
}

#==============================================
#up2024_0601_21:30

fig4b_plot_f <- function(f_data, f_index){
  f_vari <- (f_index - 1) %/% len_times_set + 1
  f_time <- (f_index - 1) %% len_times_set + 1
  f_model <- lm(yy ~ xx, data = f_data)
  f_interc <- coef(f_model)[1]
  f_slope <- coef(f_model)[2]
  f_eq <- paste0('y = ', round(f_interc, 2), ' + ', round(f_slope, 2), ' * x')
  
  f_p <- ggplot(f_data, aes(x = xx, y = yy)) +
    geom_point() +
    geom_smooth(method = 'lm', se = FALSE, col = 'blue') +
    annotate('text', x = Inf, y = Inf, label = f_eq, hjust = 1.1, vjust = 1.5, size = 5, color = 'red') +
    ggtitle(paste('vari:', varis[f_vari],'_time:',f_time)) +
    theme_minimal()
  
  return(f_p)
}

#==============================================
#up2024_0602_11:30

fig4b_res_f <- function(f_sub_name){
  cat(f_sub_name, '\n')
  f_data_fig4b_list_1 <- fig4b_list_f(f_sub_name)
  f_plots_fig4b_1 <- lapply(seq_along(f_data_fig4b_list_1), function(nn) fig4b_plot_f(f_data_fig4b_list_1[[nn]], nn))
  f_comb_plots_fig4b_1 <- grid.arrange(grobs = f_plots_fig4b_1, ncol = len_times_set, nrow = len_varis)
  ggsave(paste0('FIG2/comb_plots_fig4b_1', f_sub_name,'.jpg'), plot = f_comb_plots_fig4b_1, width = 15, height = 15, dpi = 300)  
  return(f_comb_plots_fig4b_1)
}

#fig4b_res <- list()
#for(c_sub_name in subs_name){
#  fig4b_res[[c_sub_name]] <- fig4b_res_f(c_sub_name)
#}

#==============================================
#check

check_2 <- fig4b_res_f('ORI')
