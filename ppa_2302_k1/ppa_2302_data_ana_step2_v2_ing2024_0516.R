library(MASS)
library(skimr)
library(DataExplorer)
library(tidyverse)
library(caret)
library(pROC)
library(ggplot2)
library(readxl)

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2302_k2/ARCGIS')

#==============================================
#up2024_0515_19:00_s

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
#up2024_0515_19:00_e
#====================================
#up2024_0515_19:00_s

load('RES1/ANA1_data_1_paras.RData')
load('RES1/ANA1_data_1.RData')
load('RES1/ANA1_data_2.RData')

times_set <- data_1_paras[['times_set']]
strs_co <- data_1_paras[['strs_co']]
strs_mo <- data_1_paras[['strs_mo']]
days_ori <- data_1_paras[['days_ori']]
days_nor <- data_1_paras[['days_nor']]
days_hot <- data_1_paras[['days_hot']]
VARIS <- data_1_paras[['VARIS']]
SUBS1 <- data_1_paras[['SUBS1']]
SUBS1_NAME <- data_1_paras[['SUBS1_NAME']]

SUBS <- c(ORI = list(days_ori), SUBS1)
SUBS_NAME <- c('ORI', SUBS1_NAME)

len_times_set <- length(times_set)
len_strs_co <- length(strs_co)
len_strs_mo <- length(strs_mo)
len_days_ori <- length(days_ori)
len_days_nor <- length(days_nor)
len_days_hot <- length(days_hot)
len_VARIS <- length(VARIS)
len_SUBS <- length(SUBS)
len_sites <- 50 #to_be_set

index_1 <- read.csv('index_1m_df.csv')[1:300,]
ele_2a <- read.csv('ele_2_table.csv')
ele_2b <- as.matrix(ele_2a)
ele_2c <- matrix(ele_2b, nrow = len_sites * len_strs_mo, ncol = 1)
index_1$ele_2 <- ele_2c

#up2024_0515_19:00_e
cat('==============step1================\n')
#up2024_0515_19:00_s

data_mean_1f <- function(f_data_2, f_SUB, f_VARI){
  f_1 <- f_data_2[[f_SUB]][[f_VARI]]
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


#up2024_0515_19:00_e
#==============================================
#up2024_0515_19:00_s

data_2_MEAN <- list()
for(c_SUB_NAME in SUBS_NAME){
  data_2_MEAN[[c_SUB_NAME]] <- list()
  for(c_VARI in VARIS){
    data_2_MEAN[[c_SUB_NAME]][[c_VARI]] <- data_mean_1f(data_2,c_SUB_NAME, c_VARI)
  }
}

#up2024_0515_19:00_e
#==============================================
#up2024_0515_19:00_s

dis_1 <- seq(10, 500, 10) #to_be_set
dis_2 <- rep(dis_1, len_strs_mo)

write.csv(dis_1, 'dis_1.csv', row.names = FALSE)
write.csv(dis_2, 'dis_2.csv', row.names = FALSE)

#plot(dis_1, data_2_MEAN$ORI$TP$r2[ ,3], main = 'Scatter Plot', xlab = 'Distance (m)', ylab = 'Y') #to_be_set
#plot(dis_2, data_2_MEAN$ORI$TP$r3[ ,3], main = 'Scatter Plot', xlab = 'Distance (m)', ylab = 'Y') #to_be_set

#up2024_0515_19:00_e
cat('==============step2================\n')
#up2024_0515_22:00_s

f_model_1f <- function(f_1, f_2){
  f_model_1 <- lm(f_1 ~ poly(f_2, 3, raw = TRUE))
  f_model_2 <- summary(f_model_1)
  f_model_coe <- rep(0,4)
  f_model_coe[1] <- f_model_2$coefficients[1,1]
  f_model_coe[2] <- f_model_2$coefficients[2,1]
  f_model_coe[3] <- f_model_2$coefficients[3,1]
  f_model_coe[4] <- f_model_2$coefficients[4,1]
  f_model_rci_1 <- rce_f(f_model_coe[2], f_model_coe[3], f_model_coe[4])[['rci']]
  f_model_rcd_1 <- rce_f(f_model_coe[2], f_model_coe[3], f_model_coe[4])[['rcd']]    
  
  f_res <- list()
  f_res[['model_1']] <- f_model_1
  f_res[['model_2']] <- f_model_2
  f_res[['model_coe']] <- f_model_coe
  f_res[['model_rci_1']] <- f_model_rci_1
  f_res[['model_rcd_1']] <- f_model_rcd_1
  return(f_res)
}

#up2024_0515_22:00_e
#==============================================
#up2024_0515_22:00_s

model_r1 <- list()
for(c_SUB_NAME in SUBS_NAME){
  model_r1[[c_SUB_NAME]] <- list()
  for(c_VARI in VARIS){
    model_r1[[c_SUB_NAME]][[c_VARI]] <- list()
    for(c_time in times_set){
      model_r1[[c_SUB_NAME]][[c_VARI]][[c_time]] <- f_model_1f(data_2_MEAN[[c_SUB_NAME]][[c_VARI]]$r3[, c_time], dis_2)
    }
  }
}

#up2024_0515_22:00_e
cat('==============step3================\n')
#up2024_0515_22:00_s

cname_index_1 <- colnames(index_1)
cname_index_2 <- cname_index_1[c(1,3,4)] #to_be_set
cname_index_3 <- paste(cname_index_2, collapse = ' + ')

for(c_SUB_NAME in SUBS_NAME){
  for(c_VARI in VARIS){
    for(c_time in times_set){
      c_name <- paste0(c_SUB_NAME, '_', c_VARI, '_times_', c_time)
      index_1[[c_name]] <- data_2_MEAN[[c_SUB_NAME]][[c_VARI]]$r3[,c_time]
    }
  }
}

#up2024_0515_22:00_e
#==============================================
#up2024_0515_22:00_s

f_model_2f <- function(f_SUB_NAME, f_VARI, f_time){
  c_1 <- paste0(f_SUB_NAME, '_', f_VARI, '_times_', f_time)
  f_model_for <- as.formula(paste0(c_1, ' ~ ', cname_index_3)) 
  f_model_res <- lm(f_model_for, data = index_1)
  return(f_model_res)
}

#up2024_0515_22:00_e
#==============================================
#up2024_0515_22:00_s

model_r2 <- list()
for(c_SUB_NAME in SUBS_NAME){
  model_r2[[c_SUB_NAME]] <- list()
  for(c_VARI in VARIS){
    model_r2[[c_SUB_NAME]][[c_VARI]] <- list()
    for(c_time in times_set){
      model_r2[[c_SUB_NAME]][[c_VARI]][[c_time]] <- f_model_2f(c_SUB_NAME, c_VARI, c_time)
    }
  }
}
#up2024_0515_22:00_e
cat('==============step4================\n')
#up2024_0515_22:00_s

mean_df_1f <- function(f_SUB_NAME, f_VARI){
  f_1 <- data_2_MEAN[[f_SUB_NAME]][[f_VARI]]
  f_r1 <- f_1$r1
  f_r2 <- as.data.frame(f_1$r2)
  f_r3 <- as.data.frame(f_1$r3)
  colnames(f_r2) <- times_set
  colnames(f_r3) <- times_set
  write.csv(f_r2, paste0('RES1/data_2_MEAN_r2_', f_SUB_NAME, '_', f_VARI,'_df.csv'), row.names = FALSE)
  write.csv(f_r3, paste0('RES1/data_2_MEAN_r3_', f_SUB_NAME, '_', f_VARI,'_df.csv'), row.names = FALSE)
  
  for(f_time in times_set){
    write.csv(f_r1[[f_time]], paste0('RES1/data_2_MEAN_r1_', f_SUB_NAME, '_', f_VARI, '_', f_time,'_df.csv'), row.names = FALSE)
  }
  
  f_res <- list()
  f_res[['r1']] <- f_r1
  f_res[['r2']] <- f_r2
  f_res[['r3']] <- f_r3
  return(f_res)
}

#up2024_0515_22:00_e
#==============================================
#up2024_0515_22:00_s

data_2_MEAN_df <- list() 
for(c_SUB_NAME in SUBS_NAME){
  data_2_MEAN_df[[c_SUB_NAME]] <- list() 
  for(c_VARI in VARIS){
    data_2_MEAN_df[[c_SUB_NAME]][[c_VARI]] <- mean_df_1f(c_SUB_NAME, c_VARI)
  }
}

#up2024_0515_22:00_e
cat('==============end================\n')