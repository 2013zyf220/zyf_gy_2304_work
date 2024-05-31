library(MASS)
library(skimr)
library(DataExplorer)
library(tidyverse)
library(caret)
library(pROC)
library(ggplot2)
library(readxl)
library(zoo)

#note:time range:1995-1997,2003.3-2020.11
#data source: https://reg.bom.gov.au/climate/reg/oneminsolar/
#username: ncc5661,password: prP97Tes

setwd('E:/zyf_gn/zyf_gn_2301_data/ppb_2401_k1/STEP2')

#================================
#up2024_0531_08:15

site_num <- '023034' #to_be_set

read_data_1 <- function(f_year, f_mon){
  fc_year <- as.character(f_year)
  if(f_mon < 10){
    fc_mon <- paste0('0', as.character(f_mon))
  }else{
    fc_mon <- as.character(f_mon)
  }
  f_data_1 <- read.table(paste0('sl_', site_num, '_', fc_year, '_', fc_mon, '.txt/sl_', site_num, '_', fc_year, '_', fc_mon, '.txt'), sep = ',', header = TRUE)
  f_data_2 <- f_data_1[, c('Year.Month.Day.Hours.Minutes.in.YYYY', 'MM', 'DD','HH24','MI.format.in.Local.standard.time','Mean.global.irradiance..over.1.minute..in.W.sq.m')]
  
  f_res <- list()
  f_res[['data_1']] <- f_data_1
  f_res[['data_2']] <- f_data_2
  return(f_res)
}

#================================
#up2024_0531_08:15

years_1 <- 1995:1997 #to_be_set_key
years_2 <- 2003:2020 #to_be_set_key
years <- c(years_1, years_2)

#================================
#up2024_0531_08:15

mons_f <- function(f_year){
  if(f_year == 2003){
    f_mons <- 3:12
  }else if(f_year == 2020){
    f_mons <- 1:11
  }else{
    f_mons <- 1:12
  }
  return(f_mons)
}

#================================
#up2024_0531_08:15

f_is_leap_year <- function(f_year){
  if((f_year %% 4 == 0 && f_year %% 100 != 0)||(f_year %% 400 == 0)){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

days_in_month_f <- function(f_year, f_mon){
  f_days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  if(f_mon == 2 && f_is_leap_year(f_year)){
    return(29)
  }else{
    return(f_days[f_mon])
  }
}

#================================
#up2024_0531_08:15

days_in_year_f <- function(f_year){
  if(f_year == 2003){
    f_len <- 306
  }else if(f_year == 2020){
    f_len <- 335
  }else if(f_is_leap_year(f_year)){
    f_len <- 366
  }else{
    f_len <- 365
  }
  return(f_len)
}

#================================
#up2024_0531_08:15

data_1 <- list()
for(c_year in years){
  cat('year:', c_year)
  c_mons <- mons_f(c_year)
  cc_year <- as.character(c_year)
  data_1[[cc_year]] <- list()
  
  for(c_mon in c_mons){
    cc_mon <- as.character(c_mon)
    data_1[[cc_year]][[cc_mon]] <- read_data_1(c_year, c_mon)$data_2
  }
}

#================================
#up2024_0531_08:15

interp_thres_f <- function(f_data, f_max_cons_na){
  f_na_runs <- rle(is.na(f_data))  #找到连续缺失值的起始和结束位置
  f_na_len <- f_na_runs$lengths
  f_na_val <- f_na_runs$values
  
  f_keep_na <- rep(TRUE, length(f_data))  #创建一个逻辑向量，用于标记是否保留缺失值
  f_pos <- cumsum(f_na_len)  # 标记连续缺失值超过阈值的位置
  f_start_pos <- c(1, head(f_pos, -1) + 1)
  for(ii in seq_along(f_na_len)){
    if(f_na_val[ii] && f_na_len[ii] > f_max_cons_na){
      f_keep_na[f_start_pos[ii]:f_pos[ii]] <- FALSE
    }
  }
  f_data_2 <- f_data[f_keep_na]
  f_i_data <- f_data
  f_i_data[f_keep_na] <- na.approx(f_data_2, na.rm = FALSE)
  
  f_res <- list()
  f_res[['na_len']] <- f_na_len
  f_res[['na_val']] <- f_na_val
  f_res[['keep_na']] <- f_keep_na
  f_res[['pos']] <- f_pos
  f_res[['start_pos']] <- f_start_pos
  f_res[['data_2']] <- f_data_2
  f_res[['i_data']] <- f_i_data
  return(f_res)
}

#================================
#up2024_0531_08:15

mean_rad_f <- function(f_year, f_mon, f_day){
  f_mean_1 <- read_data_1(f_year, f_mon)$data_2
  f_mean_2 <- f_mean_1[f_mean_1$DD == f_day, 'Mean.global.irradiance..over.1.minute..in.W.sq.m']
  f_mean_3 <- f_mean_2[f_mean_2 > 10]  #to_be_set
  f_mean_4 <- interp_thres_f(f_mean_3, 10)[['i_data']] #to_be_set
  f_mean_5 <- mean(f_mean_4)
  
  f_res <- list()
  f_res[['mean_2']] <- f_mean_2
  f_res[['mean_3']] <- f_mean_3
  f_res[['mean_4']] <- f_mean_4
  f_res[['mean_5']] <- f_mean_5
  return(f_res)
}

#================================
#up2024_0531_08:15

plot(mean_rad_f(2003,3,9)$mean_4, type = 'o', col = 'skyblue', xlab = 'Hour', ylab = 'Radiation') #to_be_set

data_mon_set <- read_data_1(2003, 3)$data_2[,'Mean.global.irradiance..over.1.minute..in.W.sq.m'] #to_be_set
plot(data_mon_set, type = 'o', col = 'skyblue', xlab = 'Hour', ylab = 'Radiation')
#================================
#up2024_0531_08:15

mean_rad_1 <- list()
mean_rad_2 <- matrix(0, ncol = length(years), nrow = 370)

ii <- 0
for(c_year in years){
  ii <- ii + 1
  cc_year <- as.character(c_year)
  mean_rad_1[[cc_year]] <- matrix(0, ncol = 12, nrow = 31)
  c_mons <- mons_f(c_year)
  jj <- 0
  for(c_mon in c_mons){
    cc_mon <- as.character(c_mon)
    c_len_days <- days_in_month_f(c_year, c_mon)
    for(c_day in 1: c_len_days){
      jj <- jj + 1
      mean_rad_1[[cc_year]][c_day, c_mon] <- mean_rad_f(c_year, c_mon, c_day)$mean_5
      mean_rad_2[jj, ii] <- mean_rad_f(c_year, c_mon, c_day)$mean_5
    }
  }
}

mean_rad_2_df <- as.data.frame(mean_rad_2)
colnames(mean_rad_2_df) <- years

#==================================
#up2024_0531_08:15

mean_rad_3_df <- list()
for(c_year in years){
  cc_year <- as.character(c_year)
  c_len_days_2 <- days_in_year_f(c_year)
  mean_rad_3_df[[cc_year]] <- mean_rad_2_df[[cc_year]][1: c_len_days_2]
}
#==================================
#up2024_0531_08:15

interp_thres_2f <- function(f_data){
  f_n <- sum(is.na(f_data) & cumsum(!is.na(f_data)) == 0) # 计算前面连续NA的数量
  if (f_n > 0){
    f_real_val <- f_data[!is.na(f_data)] # 找到前两个非缺失值
    if(length(f_real_val) < 2){
      stop('no enough values')
    }
    f_real_val_no1 <- f_real_val[1]
    f_real_val_no2 <- f_real_val[2]
    f_dif <- f_real_val_no2 - f_real_val_no1
    f_data[1:f_n] <- seq(from = f_real_val_no1 - f_dif * f_n, by = f_dif, length.out = f_n)
  }
  return(f_data)
}

#=============================================
#up2024_0531_08:15

year_interp <- function(f_year){
  fc_year <- as.character(f_year)
  f_1 <- mean_rad_3_df[[fc_year]]
  f_2 <- interp_thres_2f(f_1)
  f_3 <- interp_thres_f(f_2, 10)[['i_data']] #to_be_set
  
  f_res <- list()
  f_res[['r1']] <- f_1
  f_res[['r2']] <- f_2
  f_res[['r3']] <- f_3
  return(f_res)
}

#=============================================
#up2024_0531_08:15

data_year_1 <- list()
data_year_1m <- matrix(0, ncol = length(years), nrow = 370)
data_year_1_mean <- rep(0, length(years))

ii <- 0 
for(c_year in years){
  ii <- ii + 1
  fc_year <- as.character(c_year)
  data_year_1[[fc_year]] <- year_interp(c_year)$r3
  
  c_len_days_2 <- days_in_year_f(c_year)
  data_year_1m[1: c_len_days_2,ii] <- data_year_1[[fc_year]]
  data_year_1_mean[ii] <- mean(data_year_1[[fc_year]], na.rm = TRUE)
}

data_year_1_mean_df <- as.data.frame(data_year_1_mean)
rownames(data_year_1_mean_df) <- years

data_year_1m_df <- as.data.frame(data_year_1m)
colnames(data_year_1m_df) <- years
#=============================================
#up2024_0531_08:15

write.csv(data_year_1_mean_df, file = 'RES/data_year_1_mean.csv', row.names = TRUE)
write.csv(data_year_1m_df, file = 'RES/data_year_1m_1.csv', row.names = FALSE)
write.csv(mean_rad_2_df, file = 'RES/data_year_1m_0.csv', row.names = FALSE)

#=============================================
#up2024_0531_08:15

for(c_year in years){
  cc_year <- as.character(c_year)
  jpeg(paste0('RES/fig_yearly_rad_0_', cc_year, '.jpg'), width = 2000, height = 2000)
  plot(mean_rad_2_df[[cc_year]], type = 'o', col = 'skyblue', xlab = 'Hour', ylab = 'Radiation', main = cc_year) 
  dev.off()
}
