setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2302_k2/ARCGIS')

#==================
#up2024_0709_22:00

len_sites <- 50 #to_be_set
len_strs_co <- 8  #to_be_set
len_days_ori <- 6 #to_be_set
times_set <- c(1,2,3) #to_be_set
#==================
#up2024_0709_22:00

date_1 <- c('2023/8/15', '2023/8/16', '2023/8/17', '2023/8/20', '2023/8/22', '2023/8/28')  #to_be_set
date_2 <- list()
date_3 <- c()

for(mm in 1:len_days_ori)
  date_2[[mm]] <- rep(date_1[mm], len_sites * len_strs_co) #to_be_set
  date_3 <- append(date_3, date_2[[mm]])
write.csv(date_3, 'RES1/date_3.csv')

#==================
#up2024_0709_22:00

time_s <- c('10:00','15:00','20:00')
time_e <- c('11:00','16:00','21:00')

get_time_f <- function(f_time){
  f_time_s2 <- rep(time_s[f_time], len_sites)
  f_time_e2 <- rep(time_e[f_time], len_sites)
  f_time_3 <- c(f_time_s2, f_time_e2)
  f_time_4 <- rep(f_time_3, len_strs_co * len_days_ori/2)
  return(f_time_4)
}

time_sum <- list()
for(ii in times_set){
  time_sum[[ii]] <- get_time_f(ii)
}

#==================
#up2024_0709_22:00

lon_1 <- 106  #to_be_set
lat_1 <- 29 #to_be_set

lon_2 <- rep(lon_1, len_sites * len_strs_co * len_days_ori)
lat_2 <- rep(lat_1, len_sites * len_strs_co * len_days_ori)

#==================
#up2024_0709_22:00

ta_1 <- list()
ta_2 <- list()
for(ii in times_set){
  ta_1[[ii]] <- as.matrix(read.csv(paste0('RES1/data_1_TP_time', ii, '.csv')))
  ta_2[[ii]] <- as.vector(ta_1[[ii]])
}

#==================
#up2024_0709_22:00

rh_1 <- list()
rh_2 <- list()
for(ii in times_set){
  rh_1[[ii]] <- as.matrix(read.csv(paste0('RES1/data_1_RH_time', ii, '.csv')))
  rh_2[[ii]] <- as.vector(rh_1[[ii]])
}

#==================
#up2024_0709_22:00

rayman_1 <- list()
for(ii in times_set){
  rayman_1[[ii]] <- matrix(0, nrow = len_sites * len_strs_co * len_days_ori, ncol = 6)
  rayman_1[[ii]][,1] <- date_3
  rayman_1[[ii]][,2] <- time_sum[[ii]]
  rayman_1[[ii]][,3] <- lon_2
  rayman_1[[ii]][,4] <- lat_2
  rayman_1[[ii]][,5] <- ta_2[[ii]]
  rayman_1[[ii]][,6] <- rh_2[[ii]]
  write.csv(rayman_1[[ii]], paste0('RES1/rayman_1_time', ii, '.csv'))
}

