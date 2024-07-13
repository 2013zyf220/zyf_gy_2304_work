library(MASS)
library(skimr)
library(DataExplorer)
library(tidyverse)
library(caret)
library(pROC)
library(ggplot2)
library(readxl)

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2302_k2/ARCGIS')

#==================
#up2024_0712_22:03

len_sites <- 50 #to_be_set
len_strs_co <- 8  #to_be_set
len_days_ori <- 6 #to_be_set
times_set <- c(1,2,3) #to_be_set

#==================
#up2024_0712_22:03

date_1 <- c('15.8.2023', '16.8.2023', '17.8.2023', '20.8.2023', '22.8.2023', '28.8.2023')  #to_be_set
date_2 <- list()
date_3 <- c()

for(mm in 1:len_days_ori)
  date_2[[mm]] <- rep(date_1[mm], len_sites * len_strs_co) #to_be_set
  date_3 <- append(date_3, date_2[[mm]])
  
#==================
#up2024_0712_22:05

time_a <- c('10:00','15:00','20:00')
time_b <- c('11:00','16:00','21:00')

get_time_f <- function(f_time){
  f_time_a2 <- rep(time_a[f_time], len_sites)
  f_time_b2 <- rep(time_b[f_time], len_sites)
  f_time_3 <- c(f_time_a2, f_time_b2)
  f_time_4 <- rep(f_time_3, len_strs_co * len_days_ori * 0.5)
  return(f_time_4)
}

time_sum <- list()
for(ii in times_set){
  time_sum[[ii]] <- get_time_f(ii)
}

#==================
#up2024_0712_22:06

lon_1 <- 106 + 35/60  #to_be_set
lat_1 <- 29 + 43/60 #to_be_set
ele_1 <- 432  #to_be_set
timez_1 <- 8.0 #to_be_set

lon_2 <- rep(lon_1, len_sites * len_strs_co * len_days_ori)
lat_2 <- rep(lat_1, len_sites * len_strs_co * len_days_ori)
ele_2 <- rep(ele_1, len_sites * len_strs_co * len_days_ori)
timez_2 <- rep(timez_1, len_sites * len_strs_co * len_days_ori)

#==================
#up2024_0712_22:06

time_1 <- list()
time_2 <- list()
for(ii in times_set){
  time_1[[ii]] <- as.matrix(read.csv(paste0('RES2/rec_1_TIME_time', ii, '.csv')))
  time_2[[ii]] <- as.vector(time_1[[ii]])
  write.csv(time_2[[ii]], paste0('RES2/rayman_time_1_time', ii, '.csv'))
}

#==================
#up2024_0712_22:06

tp_1 <- list()
tp_2 <- list()
for(ii in times_set){
  tp_1[[ii]] <- as.matrix(read.csv(paste0('RES2/rec_1_TP_time', ii, '.csv')))
  tp_2[[ii]] <- as.vector(tp_1[[ii]])
}


#==================
#up2024_0712_22:06

rh_1 <- list()
rh_2 <- list()
for(ii in times_set){
  rh_1[[ii]] <- as.matrix(read.csv(paste0('RES2/rec_1_RH_time', ii, '.csv')))
  rh_2[[ii]] <- as.vector(rh_1[[ii]])
}

#==================
#up2024_0712_22:06

ws_1 <- list()
ws_2 <- list()
for(ii in times_set){
  ws_1[[ii]] <- as.matrix(read.csv(paste0('RES2/recw2_1_1WS_time', ii, '.csv')))
  ws_2[[ii]] <- as.vector(ws_1[[ii]])
}

#==================
#up2024_0712_22:06

could_1 <- 0.0 #to_be_set
could_2 <- rep(could_1, len_sites * len_strs_co * len_days_ori)

#==================
#up2024_0712_22:06

rayman_1 <- list()
for(ii in times_set){
  rayman_1[[ii]] <- matrix(0, nrow = len_sites * len_strs_co * len_days_ori, ncol = 10) #to_be_set
  rayman_1[[ii]][,1] <- date_3
  rayman_1[[ii]][,2] <- time_sum[[ii]]
  rayman_1[[ii]][,3] <- lon_2
  rayman_1[[ii]][,4] <- lat_2
  rayman_1[[ii]][,5] <- ele_2
  rayman_1[[ii]][,6] <- timez_2
  rayman_1[[ii]][,7] <- tp_2[[ii]]
  rayman_1[[ii]][,8] <- rh_2[[ii]]
  rayman_1[[ii]][,9] <- ws_2[[ii]]
  rayman_1[[ii]][,10] <- could_2
  write.csv(rayman_1[[ii]], paste0('RES2/rayman_1_time', ii, '.csv'))
}

