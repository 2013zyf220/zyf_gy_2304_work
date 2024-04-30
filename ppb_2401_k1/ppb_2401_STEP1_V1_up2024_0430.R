library(MASS)
library(skimr)
library(DataExplorer)
library(tidyverse)
library(caret)
library(pROC)
library(ggplot2)
library(pdp)
library(readxl)
library(maptools)
library(suncalc)
library(dplyr)
library(zoo)

setwd("E:/zyf_gn/zyf_gn_2301_data/ppb_2401_k1")

#=================================
#up2024_0430_09:17_s
#basic setting

lat_1 <- -34.95  #to_be_set
lon_1 <- 138.53  #to_be_set
tz_set <- "Australia/Adelaide" #to_be_set
date_set_1 <- seq(as.Date("1986-01-01"), as.Date("2018-12-31"), by = "day") #to_be_set_key
sun_times_1 <- getSunlightTimes(date = date_set_1, lat = lat_1, lon = lon_1, tz = tz_set)
sun_times_2 <- sun_times_1[, c("date", "sunrise", "sunset")]

wea_data_0 <- read.csv("ade_air_30yrs.csv")
#wea_start <- 17499 + (365 * 30 + 7) * 48 #to_be_set
wea_start <- 17499 #to_be_set
wea_end <- nrow(wea_data_0) - 8906  #to_be_set
wea_data <- wea_data_0[wea_start: wea_end, ] 

len_wea <- nrow(wea_data)
len_days <- nrow(sun_times_2)

#up2024_0430_09:17_e
#=================================
#up2024_0430_09:17_s
#interpolation of air temperature

at_unfix <- wea_data$Air.Temperature.in.degrees.C
at_unfix_na_loc <- which(at_unfix == -9999)

at_unfix[at_unfix == -9999] <- NA
at_fix <- na.approx(at_unfix)
wea_data$at_fix <- at_fix

write.csv(at_unfix, "at_unfix.csv", row.names = FALSE)
write.csv(at_fix, "at_fix.csv", row.names = FALSE)

#up2024_0430_09:17_e
#=================================
#up2024_0430_09:18_s
#change the sunrise and sunset time to standard

sunrise2_res <- rep(0, len_days)
sunset2_res <- rep(0, len_days)
for(ii in 1: len_days){
  sunrise2_res[ii] <- as.numeric(as.POSIXct(sun_times_2$sunrise[ii], format = "%Y-%m-%d %H:%M:%S", tz = tz_set))
  sunset2_res[ii] <- as.numeric(as.POSIXct(sun_times_2$sunset[ii], format = "%Y-%m-%d %H:%M:%S", tz = tz_set))
}

sun_times_2$sunrise2 <- sunrise2_res
sun_times_2$sunset2 <- sunset2_res

write.csv(sun_times_2, "sun_times_2_adj1.csv", row.names = FALSE)

#up2024_0430_09:18_e
#=================================
#up2024_0430_09:25_s
#check whether the sunrise and sunset time is standard

sunrise2_dif <- rep(0, len_days)
sunset2_dif <- rep(0, len_days)
sunrise2_dif[1] <- 86400
sunset2_dif[1] <- 86400

for(ii in 2: len_days){
  sunrise2_dif[ii] <- sunrise2_res[ii] - sunrise2_res[ii - 1]
  sunset2_dif[ii] <- sunset2_res[ii] - sunset2_res[ii - 1]
}
sun_times_2$sunrise2_dif <- sunrise2_dif
sun_times_2$sunset2_dif <- sunset2_dif

sunrise2_dif_max <- max(sunrise2_dif)
sunrise2_dif_min <- min(sunrise2_dif)
sunset2_dif_max <- max(sunset2_dif)
sunset2_dif_min <- min(sunset2_dif)

#up2024_0430_09:25_e
#=================================
#up2024_0430_09:29_s
#change weather data time to standard

time_all_0 <- list()
time_all <- rep(0, len_wea)

for(ii in 1: len_wea){
  c_yy <- wea_data$Year.Month.Day.Hour.Minutes.in.YYYY[ii]
  c_mm <- wea_data$MM[ii]
  c_dd <- wea_data$DD[ii]
  c_hh <- wea_data$HH24[ii]
  c_min <- wea_data$MI.format.in.Local.time[ii]
  time_all_0[[ii]] <- paste(c_yy, c_mm, c_dd, c_hh, c_min, sep="-")
  time_all[ii] <- as.POSIXct(time_all_0[[ii]], format="%Y-%m-%d-%H-%M", tz = tz_set)
}

wea_data$time_all_0 <- time_all_0
wea_data$time_all <- time_all

#up2024_0430_09:29_e
#============================
#up2024_0430_09:34_s
#check whether the time of weather data is standard

time_all_st <- rep(0, len_wea) #method 1
time_all_subs <- rep(0, len_wea) #method 2

time_all_st[1] <- time_all[1]
for(ii in 2: len_wea){
  time_all_st[ii] <- time_all_st[ii - 1] + 1800
  time_all_subs[ii] <- time_all[ii] - time_all[ii - 1]
}

time_all_dif <- time_all_st - time_all
wea_data$time_all_st <- time_all_st
wea_data$time_all_dif <- time_all_dif
wea_data$time_all_subs <- time_all_subs

time_all_sum <- cbind(time_all_0, time_all, time_all_subs, time_all_st, time_all_dif)
colnames(time_all_sum) <- c('time_all_0', 'time_all', 'time_all_subs', 'time_all_st', 'time_all_dif')
write.csv(time_all_sum, "time_all_sum.csv", row.names = FALSE)

#up2024_0430_09:34_e
#=================================
#up2024_0430_09:39_s
#calculate daily mean air temperature

wea_data2 <- list()
wea_data2_at <- list()
wea_data2_at_mean <- rep(0, len_days)
wea_year <- rep(0, len_days)

for(ii in 1: len_days){
  print(ii)
  c_rise_1 <- sun_times_2$sunrise2[ii]
  c_sset_1 <- sun_times_2$sunset2[ii]
  wea_data2[[ii]] <- wea_data[wea_data$time_all > c_rise_1 & wea_data$time_all < c_sset_1, ]
  wea_data2_at[[ii]] <- wea_data2[[ii]]$at_fix
  wea_data2_at_mean[ii] <- round(mean(wea_data2_at[[ii]]),2)
  wea_year[ii] <- wea_data2[[ii]]$Year.Month.Day.Hour.Minutes.in.YYYY[1]
}

sun_times_2$at_mean <- wea_data2_at_mean
sun_times_2$wea_year <- wea_year

#up2024_0430_09:39_e
#=================================
#up2024_0430_10:08_s

wea_date_0 <- list()
wea_date <- c()
for(ii in 1: len_days){
  c_year <- wea_data2[[ii]]$Year.Month.Day.Hour.Minutes.in.YYYY[1]
  c_mon <- wea_data2[[ii]]$MM[1]
  c_day <- wea_data2[[ii]]$DD[1]
  wea_date_0[[ii]] <- paste(c_year, c_mon, c_day, sep="-")
  wea_date <- c(wea_date, wea_date_0[[ii]])
}

wea_export_daily <- cbind(wea_date, wea_data2_at_mean)
write.csv(wea_export_daily, "wea_export_daily.csv", row.names = FALSE)

#up2024_0430_10:08_e
#=================================
#up2024_0430_09:47_s
#calculate yearly mean air temperature

wea_year_min0 <- min(wea_year)
wea_year_max0 <- max(wea_year)
wea_year_min <- wea_year_min0 + 0 #to_be_set
wea_year_max <- wea_year_max0 - 0 #to_be_set
wea_year_len <- wea_year_max - wea_year_min + 1
wea_years <- wea_year_min: wea_year_max
  
sun_times_b <- list()
sun_times_b_at <- list()
sun_times_b_atmean <- rep(0, wea_year_len)
ii <- 0
for(c_year in wea_years){
  ii <- ii + 1
  sun_times_b[[ii]] <- sun_times_2[sun_times_2$wea_year == c_year, ]
  sun_times_b_at[[ii]] <- sun_times_b[[ii]]$at_mean
  sun_times_b_atmean[ii] <- round(mean(sun_times_b_at[[ii]]),2)
}

wea_export_yearly <- cbind(wea_years, sun_times_b_atmean)
write.csv(wea_export_yearly, "wea_export_yearly.csv", row.names = FALSE)

#up2024_0430_09:47_e


