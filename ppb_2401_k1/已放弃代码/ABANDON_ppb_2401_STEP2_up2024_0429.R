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

#=======================
#up2024_0429_15:37_s

sun_times <- read.csv("sun_times_2.csv")
wea_data_0 <- read.csv("ade_air_30yrs.csv")
wea_data <- wea_data_0[510000:570000, ] #to_be_set
len_wea <- nrow(wea_data)
len_days <- nrow(sun_times)

#up2024_0429_15:37_e
#=======================
#up2024_0429_15:48_s

time_all_0 <- list()
time_all <- rep(0, len_wea)
for(ii in 1: len_wea){
  c_yy <- wea_data$Year.Month.Day.Hour.Minutes.in.YYYY00[ii]
  c_mm <- wea_data$MM00[ii]
  c_dd <- wea_data$DD00[ii]
  c_hh <- wea_data$HH2400[ii]
  c_min <- wea_data$MI.format.in.Local.standard.time00[ii]
  time_all_0[[ii]] <- paste(c_yy, c_mm, c_dd, c_hh, c_min, sep="-")
  time_all[ii] <- as.POSIXct(time_all_0[[ii]], format="%Y-%m-%d-%H-%M")
}
wea_data$time_new <- time_all
wea_data$time_new0 <- time_all_0

#up2024_0429_15:48_e
#=======================
#ing
at_unfix <- wea_data$Air.Temperature.in.degrees.C
at_unfix[at_unfix == -9999] <- NA
at_fix <- na.approx(at_unfix)

write.csv(at_unfix, "at_unfix.csv", row.names = FALSE)
write.csv(at_fix, "at_fix.csv", row.names = FALSE)

wea_data$at_fix <- at_fix
#=======================
#up2024_0429_16:15_s

rise_1 <- rep(0,len_days)
sset_1 <- rep(0,len_days)

wea_data2 <- list()
wea_data2_at <- list()
wea_data2_at_mean <- rep(0, len_days)
wea_year <- rep(0, len_days)
for(ii in 1: len_days){
  rise_1[ii] <- sun_times$sunrise2[ii]
  c_rise_2 <- as.numeric(as.POSIXct(rise_1[ii], format="%Y-%m-%d %H:%M"))
  
  sset_1[ii] <- sun_times$sunset2[ii]
  c_sset_2 <- as.numeric(as.POSIXct(sset_1[ii], format="%Y-%m-%d %H:%M"))
  wea_data2[[ii]] <- wea_data[wea_data$time_new > c_rise_2 & wea_data$time_new < c_sset_2,]
  wea_data2_at[[ii]] <- wea_data2[[ii]]$at_fix
  wea_data2_at_mean[ii] <- mean(wea_data2_at[[ii]])
  wea_year[ii] <- as.numeric(substr(sun_times$date[ii], 1, 4))
}

sun_times$at_mean <- wea_data2_at_mean
sun_times$wea_year <- wea_year
write.csv(sun_times, "sun_times_3.csv", row.names = FALSE)

wea_year_min0 <- min(wea_year)
wea_year_max0 <- max(wea_year)
wea_year_min <- wea_year_min0 + 0 #to_be_set
wea_year_max <- wea_year_max0 - 0 #to_be_set
wea_year_len <- wea_year_max - wea_year_min + 1

#up2024_0429_16:15_e
#=======================
#up2024_0429_16:55_s
sun_times_b <- list()
sun_times_b_at <- list()
sun_times_b_atmean <- rep(0, wea_year_len)
ii <- 0
for(c_year in wea_year_min: wea_year_max){
  ii <- ii + 1
  sun_times_b[[ii]] <- sun_times[sun_times$wea_year == c_year, ]
  sun_times_b_at[[ii]] <- sun_times_b[[ii]]$at_mean
  sun_times_b_atmean[ii] <- mean(sun_times_b_at[[ii]])
}

write.csv(sun_times_b_atmean, "sun_times_b_atmean.csv", row.names = FALSE)
#up2024_0429_16:55_e