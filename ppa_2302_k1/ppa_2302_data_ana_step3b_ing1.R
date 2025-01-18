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
#对降温指标进行线性相关和回归
#=====================================
#up2024 1126 21:49

version_set <- 13 #to_be_set
times_set <- c(2,3) #to_be_set
varis_range <- c('A9:AJ10','AN9:BW10','CB9:DK10') #to_be_set
varis <- c('TP','RH','DI') #to_be_set

indexes <- c('RCD', 'RCI')
indeps <- c('BLD','VEG','SW','BH_4') #to_be_set
indeps_2 <- paste(indeps, collapse = ' + ')

len_varis <- length(varis)
len_indexes <- length(indexes)
len_indeps <- length(indeps)

#=====================================
#up2024 1126 22:09

data_1 <- list()
for(c_time in times_set){
  data_1[[c_time]] <- list()
  for(ii in 1: len_varis){
    data_1[[c_time]][[ii]] <- as.matrix(read_excel(paste0('RES3/PREPARE/PROCESS_2/V', version_set, '/REVISE2d1_Fig_z2_df_ORI_time', c_time, '_V', version_set, '.xlsx'), 
                                   sheet = 'Sheet3', range = varis_range[[ii]], col_names = FALSE))
  }
}

RCD_1 <- list()
RCI_1 <- list()
for(c_time in times_set){
  RCD_1[[c_time]] <- list()
  RCI_1[[c_time]] <- list()
  for(ii in 1: len_varis){
    RCD_1[[c_time]][[ii]] <- data_1[[c_time]][[ii]][1,]
    RCI_1[[c_time]][[ii]] <- data_1[[c_time]][[ii]][2,]
  }
}

#======================
#up2024 1126 22:23

data_2 <- read_excel(paste0('RES3/PREPARE/PROCESS_2/V', version_set, '/REVISE2d1_Fig_z2_df_ORI_time3_V', version_set, '.xlsx'), 
                    sheet = 'Sheet1', range = 'E1:K109') #to_be_set
data_2m <- as.matrix(data_2)

data_3 = list()
data_4 = list()
for(ii in 1: len_varis){
  c_1 <- (ii - 1) * 36 + 1
  c_2 <- ii * 36
  data_3[[ii]] <- data_2m[c_1: c_2, c(1,2,3,7)] #to_be_set 
  data_4[[ii]] <- as.data.frame(data_3[[ii]])
  colnames(data_4[[ii]]) <- indeps #to_be_set 
  data_4[[ii]][['RCD_time2']] <- RCD_1[[2]][[ii]]
  data_4[[ii]][['RCI_time2']] <- RCI_1[[2]][[ii]]
  data_4[[ii]][['RCD_time3']] <- RCD_1[[3]][[ii]]
  data_4[[ii]][['RCI_time3']] <- RCI_1[[3]][[ii]]
}


#====================
#up2024 1126 23:33

lm_1f <- function(f_time, f_vari, f_index, f_indep){
  f_name2 <- paste0(f_index, '_time', f_time)
  f_lm_model <- lm(as.formula(paste(f_name2, '~', f_indep)), data = data_4[[f_vari]])
  f_r2 = summary(f_lm_model)$r.squared
  
  f_res <- list()
  f_res[['R2']] <- f_r2
  
  return(f_res)
}

#=====================
#up2024 1127 00:12

lm_2f <- function(f_time, f_vari, f_index){
  f_name2 <- paste0(f_index, '_time', f_time)
  f_lm_model <- lm(as.formula(paste(f_name2, '~', indeps_2)), data = data_4[[f_vari]])
  f_r2 = summary(f_lm_model)$r.squared
  
  f_res <- list()
  f_res[['R2']] <- f_r2
  
  return(f_res)
}

#=====================
#up2024 1127 00:19

res_r2_1 <- list()
for(c_time in times_set){
  res_r2_1[[c_time]] <- list()
  for(ii in 1: len_varis){
    res_r2_1[[c_time]][[ii]] <- matrix(0, nrow = len_indeps, ncol = len_indexes)
    for(c_indexn in 1:len_indexes){
      c_index <- indexes[c_indexn]
      for(c_indepn in 1:len_indeps){
        c_indep <- indeps[c_indepn]
        #cat(c_time, ii, c_index, c_indep,'\n')
        res_r2_1[[c_time]][[ii]][c_indepn, c_indexn] <- lm_1f(c_time, ii, c_index, c_indep)[['R2']]
      }
    }
  }
}



#=====================
#up2024 1127 00:23

res_r2_2 <- list()
for(c_time in times_set){
  res_r2_2[[c_time]] <- matrix(0, nrow = len_varis, ncol = len_indexes)
  for(ii in 1: len_varis){
    for(c_indexn in 1:len_indexes){
      c_index <- indexes[c_indexn]
      #cat(c_time, ii, c_index,'\n')
      res_r2_2[[c_time]][ii, c_indexn] <- lm_2f(c_time, ii, c_index)[['R2']]
    }
  }
}