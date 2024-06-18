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

buf_set <- 100 #to_be_set
index_name1 <- paste0('index_1m_df3_buf', buf_set, '.csv')
index_1 <- read.csv(index_name1)[1:300,]
index_1$ele_2 <- ele_2c
index_2 <- index_1

indep_set <- 20 #to_be_set_key

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

cat('========================step 1: basic data(data2_2)==========================\n')
#up2024_0528_17:00

data2_2_ori <- list()
for(c_vari in varis){
  data2_2_ori[[c_vari]] <- list()
  for(ii in times_set){
    data2_2_ori[[c_vari]][[ii]] <- as.matrix(read.csv(paste0('RES2/recb_2_', c_vari, '_', ii,'_df.csv'))) #to_be_set
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
#fig(each sub,each variable, each time, each street, each day)

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

