library(MASS)
library(skimr)
library(DataExplorer)
library(tidyverse)
library(caret)
library(pROC)
library(ggplot2)
library(readxl)

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2302_k2/ARCGIS')

#=================================================================
#up2024_0527_09:00

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

index_1 <- read.csv('index_1m_df.csv')[1:300,]
index_1$ele_2 <- ele_2c
index_2 <- index_1

#=================================================================
#up2024_0527_09:00
#set independent variables for regression

cname_index_1 <- colnames(index_1)
cname_index_2 <- cname_index_1[c(1,3,4)] #to_be_set_key
cname_index_3 <- paste(cname_index_2, collapse = ' + ')

#============================define functions=====================================
#up2024_0527_09:00
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
#up2024_0527_09:00

dis_1 <- seq(10, 500, 10) #to_be_set
dis_2 <- rep(dis_1, len_strs_mo)
dis_3 <- list()
dis_3[['ORI']] <- rep(dis_1, len_days_ori)
dis_3[['NOR']] <- rep(dis_1, len_days_nor)
dis_3[['HOT']] <- rep(dis_1, len_days_hot)

#==============================step 1: ========================
#ing
#function: input weather data(mean)


d2_mean_1f <- function(f_data_2, f_sub, f_vari){
  f_1 <- f_data_2[[f_sub]][[f_vari]]
  f_mean_1 <- list()
  f_mean_2 <- matrix(0, nrow = len_sites, ncol = len_times_set)
  f_mean_3 <- matrix(0, nrow = len_sites * len_strs_mo, ncol = len_times_set)
  for(ii in times_set){
    f_mean_1[[ii]] <- matrix(0, nrow = len_sites, ncol = len_strs_mo)
    for(jj in strs_mo){
      f_mean_1[[ii]][,jj] <- rowMeans(f_1[[ii]][[jj]])
    }
    f_mean_2[ ,ii] <- rowMeans(f_mean_1[[ii]])
    f_mean_3[ ,ii] <- as.vector(f_mean_1[[ii]])
  }
  f_res <- list()
  f_res[['r1']] <- f_mean_1
  f_res[['r2']] <- f_mean_2
  f_res[['r3']] <- f_mean_3
  return(f_res)
}

#==============================================
#ing
#summarize data_2 by averaging days

data_2_mean <- list()
for(c_sub_name in subs_name){
  data_2_mean[[c_sub_name]] <- list()
  for(c_vari in varis){
    data_2_mean[[c_sub_name]][[c_vari]] <- d2_mean_1f(data_2,c_sub_name, c_vari)
  }
}

cat('========================step 1: input data for regression==========================\n')
#up2024_0527_09:00
#get index_1

for(c_sub_name in subs_name){
  for(c_vari in varis){
    for(ii in times_set){
      c_name <- paste0('mean1_', c_sub_name, '_', c_vari, '_times_', ii)
      index_1[[c_name]] <- data_2_mean[[c_sub_name]][[c_vari]][,ii]
    }
  }
}

#======================================================
#up2024_0527_09:00
#function: input weather data(daily)

data_2_2f <- function(f_vari, f_time){
  f_name = paste0('RES2/rec_2_', f_vari, '_', f_time, '_df.csv')
  f_y = read.csv(f_name)
  return(f_y)
}

#======================================================
#up2024_0527_09:00
#input weather data(daily)

data_2_ori <- list()
for(c_vari in varis){
  data_2_ori[[c_vari]] <- list()
  for(ii in times_set){
    data_2_ori[[c_vari]][[ii]] <- as.matrix(data_2_2f(c_vari,ii))
  }
}


#======================================================
#up2024_0527_09:00
#get index_2

for(c_vari in varis){
  for(ii in times_set){
    for(kk in days_ori){
      c_name <- paste0('ORI_', c_vari, '_time', ii, '_day', kk)
      index_2[[c_name]] <- data_2_ori[[c_vari]][[ii]][,kk]
    }
  }
}

write.csv(index_2, paste0('RES2/index_2.csv'), row.names = FALSE)
#======================================================
#up2024_0527_09:00
#input weather data(daily) for hot and normal days

data_2 <- list()
data_2$ORI <- data_2_ori

data_2$NOR <- list()
data_2$HOT <- list()
for(c_vari in varis){
  data_2$NOR[[c_vari]] <- list()
  data_2$HOT[[c_vari]] <- list()
  for(ii in times_set){
    data_2$NOR[[c_vari]][[ii]] <- list()
    data_2$HOT[[c_vari]][[ii]] <- list()
    for(jj in strs_mo){
      data_2$NOR[[c_vari]][[ii]][[jj]] <- data_2_ori[[c_vari]][[ii]][[jj]][,days_nor]
      data_2$HOT[[c_vari]][[ii]][[jj]] <- data_2_ori[[c_vari]][[ii]][[jj]][,days_hot]
    }
  }
}

#==============================================
#up2024_0527_09:00
#separate index_1/index_2 by streets

index_1s <- list()
index_2s <- list()
for(ii in 1: len_strs_mo){
  c_1 <- (ii - 1) * len_sites + 1
  c_2 <- ii * len_sites
  index_1s[[ii]] <- index_1[c_1:c_2,]
  index_2s[[ii]] <- index_2[c_1:c_2,]
}

cat('========================step 2: rce calculation==========================\n')
#up2024_0527_09:00
#define function: calculate RCE indexes

rce_f  <- function(f_1, f_2, f_3){
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
#up2024_0527_09:00
#define function: calculate rce indexes

rce_1_f <- function(f_1, f_2){
  f_rce_1 <- lm(f_1 ~ poly(f_2, 3, raw = TRUE))
  f_rce_2 <- summary(f_rce_1)
  f_rce_coe <- rep(0,4)
  f_rce_coe[1] <- f_rce_2$coefficients[1,1]
  f_rce_coe[2] <- f_rce_2$coefficients[2,1]
  f_rce_coe[3] <- f_rce_2$coefficients[3,1]
  f_rce_coe[4] <- f_rce_2$coefficients[4,1]
  f_rci_1 <- rce_f(f_rce_coe[2], f_rce_coe[3], f_rce_coe[4])[['rci']]
  f_rcd_1 <- rce_f(f_rce_coe[2], f_rce_coe[3], f_rce_coe[4])[['rcd']]    
  
  f_res <- list()
  f_res[['model_1']] <- f_rce_1
  f_res[['model_2']] <- f_rce_2
  f_res[['model_coe']] <- f_rce_coe
  f_res[['model_rci_1']] <- f_rci_1
  f_res[['model_rcd_1']] <- f_rcd_1
  return(f_res)
}

#==============================================
#up2024_0527_09:00
#calculate rce indexes(days merged, streets together)

rce_r1 <- list()
for(c_sub_name in subs_name){
  rce_r1[[c_sub_name]] <- list()
  for(c_vari in varis){
    rce_r1[[c_sub_name]][[c_vari]] <- list()
    for(ii in times_set){dis_2
      rce_r1[[c_sub_name]][[c_vari]][[ii]] <- rce_1_f(data_2_mean[[c_sub_name]][[c_vari]][, ii], dis_2)
    }
  }
}

#==============================================
#up2024_0527_09:00
#rce calculation for each type of day, each variable, each time, each street, each day

rce_r2 <- list()
for(c_sub_name in subs_name){
  rce_r2[[c_sub_name]] <- list()
  c_days_s <- days_trans_f(c_sub_name)
  c_len_days_s <- length(c_days_s)
  for(c_vari in varis){
    rce_r2[[c_sub_name]][[c_vari]] <- list()
    for(ii in times_set){
      rce_r2[[c_sub_name]][[c_vari]][[ii]] <- list()
      for(jj in strs_mo){
        rce_r2[[c_sub_name]][[c_vari]][[ii]][[jj]] <- list()        
        for(kk in 1:c_len_days_s){
          rce_r2[[c_sub_name]][[c_vari]][[ii]][[jj]][[kk]] <- rce_1_f(data_2[[c_sub_name]][[c_vari]][[ii]][[jj]][,kk], dis_1)
        }
      }
    }
  }
}

#==============================================
#up2024_0527_09:00
#rce calculation for each type of day, each variable, each time, each street, all days together

data_2v <- list()
rce_r3 <- list()
for(c_sub_name in subs_name){
  data_2v[[c_sub_name]] <- list()
  rce_r3[[c_sub_name]] <- list()
  for(c_vari in varis){
    data_2v[[c_sub_name]][[c_vari]] <- list()
    rce_r3[[c_sub_name]][[c_vari]] <- list()
    for(ii in times_set){
      data_2v[[c_sub_name]][[c_vari]][[ii]] <- list()
      rce_r3[[c_sub_name]][[c_vari]][[ii]] <- list()
      for(jj in strs_mo){
        data_2v[[c_sub_name]][[c_vari]][[ii]][[jj]] <- as.vector(data_2[[c_sub_name]][[c_vari]][[ii]][[jj]])
        rce_r3[[c_sub_name]][[c_vari]][[ii]][[jj]] <- rce_1_f(data_2v[[c_sub_name]][[c_vari]][[ii]][[jj]], dis_3[[c_sub_name]])
      }
    }
  }
}

cat('========================step 3: regression==========================\n')
#up2024_0527_09:00
#define function: regression based on 'index_1'

regre_1f <- function(f_sub_name, f_vari, f_time){
  f_name <- paste0('mean1_', f_sub_name, '_', f_vari, '_times_', f_time)
  f_model_for <- as.formula(paste0(f_name, ' ~ ', cname_index_3)) 
  f_model_res <- lm(f_model_for, data = index_1)
  return(f_model_res)
}

#==============================================
#up2024_0527_09:00
#regression based on 'index_1'(days merged, streets put together)

regre_r1 <- list()
for(c_sub_name in subs_name){
  regre_r1[[c_sub_name]] <- list()
  for(c_vari in varis){
    regre_r1[[c_sub_name]][[c_vari]] <- list()
    for(ii in times_set){
      regre_r1[[c_sub_name]][[c_vari]][[ii]] <- regre_1f(c_sub_name, c_vari, ii)
    }
  }
}

#==============================================
#up2024_0527_09:00
#define function: regression for each type of days, each variable, each time, each street, selected days merged

regre_2f <- function(f_sub_name, f_vari, f_time, f_str){
  f_name <- paste0('mean1_', f_sub_name, '_', f_vari, '_times_', f_time)
  f_model_for <- as.formula(paste0(f_name, ' ~ ', cname_index_3)) 
  f_model_res <- lm(f_model_for, data = index_1s[[f_str]])
  return(f_model_res)
}

#==============================================
#up2024_0527_09:00
#regression for each type of days, each variable, each time, each street, selected days merged

regre_r2 <- list()
for(c_sub_name in subs_name){
  regre_r2[[c_sub_name]] <- list()
  for(c_vari in varis){
    regre_r2[[c_sub_name]][[c_vari]] <- list()
    for(ii in times_set){
      regre_r2[[c_sub_name]][[c_vari]][[ii]] <- list()
      for(jj in strs_mo){
        regre_r2[[c_sub_name]][[c_vari]][[ii]][[jj]] <- regre_2f(c_sub_name, c_vari, ii, jj)
      }
    }
  }
}

#==============================================
#up2024_0527_09:00
#define function: regression for each variable, each time, each day, all streets together

regre_3f <- function(f_vari, f_time, f_day){
  f_name <- paste0('ORI_', f_vari, '_time', f_time,'_day',f_day)
  f_model_for <- as.formula(paste0(f_name, ' ~ ', cname_index_3)) 
  f_model_res <- lm(f_model_for, data = index_2)
  return(f_model_res)
}

#==============================================
#up2024_0527_09:00
#regression for each variable, each time, each day, all streets together

regre_r3 <- list()
for(c_vari in varis){
  regre_r3[[c_vari]] <- list()
  for(ii in times_set){
    regre_r3[[c_vari]][[ii]] <- list()
    for(kk in days_ori){
      regre_r3[[c_vari]][[ii]][[kk]] <- regre_3f(c_vari, ii, kk)
    }
  }
}

#==============================================
#up2024_0527_09:00
#define function: regression for each variable, each time, each day, each street

regre_4f <- function(f_vari, f_time, f_day, f_str){
  f_name <- paste0('ORI_', f_vari, '_time', f_time,'_day',f_day)
  f_model_for <- as.formula(paste0(f_name, ' ~ ', cname_index_3)) 
  f_model_res <- lm(f_model_for, data = index_2s[[f_str]])
  return(f_model_res)
}

#==============================================
#up2024_0527_09:00
#regression for each variable, each time, each day, each street

regre_r4 <- list()
for(c_vari in varis){
  regre_r4[[c_vari]] <- list()
  for(ii in times_set){
    regre_r4[[c_vari]][[ii]] <- list()
    for(kk in days_ori){
      regre_r4[[c_vari]][[ii]][[kk]] <- list()
      for(jj in strs_mo){
        regre_r4[[c_vari]][[ii]][[kk]][[jj]] <- regre_4f(c_vari, ii, kk, jj)
      }
    }
  }
}

cat('========================step 4: figures==========================\n')
#up2024_0527_09:00
#days merged, streets together, each time, each variable, each type of day

windowsFonts(calibri = windowsFont("Calibri"))

fig_1f <- function(f_vari){
  jpeg(paste0('FIG2/fig1_data_2_mean_vs_dis_', f_vari,'.jpg'), width = 1500, height = 1500, res = 300)
  par(mfrow = c(len_subs, len_times_set), oma = c(3, 3, 3, 3), mar = c(2.8,2.8,2.8,2.8),  mgp = c(2, 0.5, 0)) 
  for(c_sub_name in subs_name){
    for(ii in times_set){
      f_y <- data_2_mean[[c_sub_name]][[f_vari]][,ii]
      plot(dis_2, f_y, main = paste0('sub:', c_sub_name, '__time: ', ii), xlab = 'Distance (m)', ylab = paste0('d ', f_vari), 
           col = 'skyblue', family = 'calibri', cex.main = 0.5, cex.lab = 0.6, cex.axis = 0.6, pch = 19, cex = 0.2)
      f_fit1 <- lm(f_y ~ poly(dis_2, 1, raw = TRUE))
      f_fit2 <- lm(f_y ~ poly(dis_2, 2, raw = TRUE))
      curve(predict(f_fit1, newdata = data.frame(dis_2 = x)), add = TRUE, col = "red", lty = 2)
      curve(predict(f_fit2, newdata = data.frame(dis_2 = x)), add = TRUE, col = "green", lty = 2)
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
#up2024_0527_09:00

fig_1_fit <- list()
for(c_vari in varis){
  fig_1_fit[[c_vari]] <- fig_1f(c_vari)
}

#==============================================
#up2024_0527_09:00
#each time, each street, days together, each sub, each variable

fig_2f <- function(f_sub, f_vari){
  jpeg(paste0('FIG1/fig2_days_together_', f_sub, '_', f_vari,'.jpg'), width = 1800, height = 1200)
  par(mfrow = c(len_times_set, len_strs_mo)) 
  for(ii in times_set){
    for(jj in strs_mo){
      f_1 <- (jj - 1) * 50 + 1
      f_2 <- jj * 50
      f_x <- dis_3[[f_sub]]
      f_y <- as.vector(data_2_ori[[f_vari]][[ii]][f_1:f_2,days_trans_f(f_sub)])
      plot(f_x, f_y, main = paste0('time:', ii, '_str: ', jj), 
           xlab = 'Distance (m)', ylab = paste0('d ', f_vari), pch = 19, col = 'skyblue')
      f_fit1 <- lm(f_y ~ poly(f_x, 1, raw = TRUE))
      f_fit2 <- lm(f_y ~ poly(f_x, 2, raw = TRUE))
      curve(predict(f_fit1, newdata = data.frame(f_x = x)), add = TRUE, col = "red", lty = 2)
      curve(predict(f_fit2, newdata = data.frame(f_x = x)), add = TRUE, col = "green", lty = 2)
    }
  }
  dev.off() 
  f_res <- list()
  f_res[['fit1']] <- f_fit1
  f_res[['fit2']] <- f_fit2
  return(f_res)
  
}

#==============================================
#up2024_0527_09:00

fig_2_fit <- list()
for(c_sub_name in subs_name){
  fig_2_fit[[c_sub_name]] <- list()
  for(c_vari in varis){
    fig_2_fit[[c_sub_name]][[c_vari]] <- fig_2f(c_sub_name, c_vari)
  }
}

#==============================================
#up2024_0522_19:00
#each time, each street, each day, each variable, no sub

fig_3f <- function(f_vari, f_time){
  jpeg(paste0('FIG1/fig2_each_day_', f_vari, '_', f_time,'.jpg'), width = 1800, height = 1200)
  par(mfrow = c(len_strs_mo, len_days_ori)) 
  for(jj in strs_mo){
    for(kk in days_ori){
      f_1 <- (jj - 1) * 50 + 1
      f_2 <- jj * 50
      f_y <- data_2_ori[[f_vari]][[f_time]][f_1:f_2,kk]
      plot(dis_1, f_y, main = paste0('str: ', jj, '_day:', kk), 
           xlab = 'Distance (m)', ylab = paste0('d ', f_vari), pch = 19, col = 'skyblue')
      f_fit1 <- lm(f_y ~ poly(dis_1, 1, raw = TRUE))
      f_fit2 <- lm(f_y ~ poly(dis_1, 2, raw = TRUE))
      curve(predict(f_fit1, newdata = data.frame(dis_1 = x)), add = TRUE, col = "red", lty = 2)
      curve(predict(f_fit2, newdata = data.frame(dis_1 = x)), add = TRUE, col = "red", lty = 2)
    }
  }
  dev.off() 
  f_res <- list()
  f_res[['fit1']] <- f_fit1
  f_res[['fit2']] <- f_fit2
  return(f_res)
}

#==============================================
#up2024_0522_19:00

fig_3_fit <- list()
for(c_vari in varis){
  fig_3_fit[[c_vari]] <- list()
  for(ii in times_set){
    fig_3_fit[[c_vari]][[ii]] <- fig_3f(c_vari, ii)
  }
}

