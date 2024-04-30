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

setwd("E:/zyf_gn/zyf_gn_2301_data/ppb_2401_k1")

#=================================
#up2024_0429_19:50_s

adj_w_f <- function(f_1){
  if(f_1 == 0){
    f_2 <- 0
  }else{
    f_2 <- 7 - f_1
  }
  return(f_2)
}

#up2024_0429_19:50_e
#=================================
#up2024_0429_19:52_s
#basic setting

lat_1 <- -34.95  #to_be_set
lon_1 <- 138.53  #to_be_set
tz_set <- "Australia/Adelaide" #to_be_set
date_set_1 <- seq(as.Date("1986-01-01"), as.Date("2018-12-31"), by = "day") #to_be_set_key
sun_times_1 <- getSunlightTimes(date = date_set_1, lat = lat_1, lon = lon_1, tz = tz_set)
sun_times_2 <- sun_times_1[, c("date", "sunrise", "sunset")]
len_days <- nrow(sun_times_2)

#up2024_0429_19:52_e
#=================================
#up2024_0429_20:08_s

year_bs <- 1985 #to_be_set_key
year_be <- 2018 #to_be_set_key
year_b <- year_bs: year_be
len_year_b <- length(year_b)

date_bs <- rep(0, len_year_b)
date_be <- rep(0, len_year_b)

kk <- 0
for(c_year in year_b){
  kk <- kk + 1
  c_year_2 <- c_year + 1
  
  c_date_1 <- as.POSIXct(paste(c_year, "-10-01", sep = ""), tz = tz_set)
  c_date_1w <- as.integer(format(c_date_1, "%w"))
  c_date_2 <- c_date_1 + adj_w_f(c_date_1w) * 86400
  date_bs[kk] <-format(c_date_2, "%Y-%m-%d 03:00:00")
  
  c_date_3 <- as.POSIXct(paste(c_year_2, "-04-01", sep = ""), tz = tz_set)
  c_date_3w <- as.integer(format(c_date_3, "%w"))
  c_date_4 <- c_date_3 + adj_w_f(c_date_3w) * 86400
  date_be[kk] <-format(c_date_4, "%Y-%m-%d 03:00:00")
}

date_bsum_1 <- cbind(date_bs, date_be)
colnames(date_bsum_1) <- c("start", "end")
date_bsum_2 <- as.data.frame(date_bsum_1)
write.csv(date_bsum_2, "date_bsum_2.csv", row.names = FALSE)

#up2024_0429_20:08_e
#=================================
#up2024_0429_20:18_s

time_adj_f <- function(f_time){
  f_time_2 <- as.POSIXct(f_time, format = "%Y-%m-%d %H:%M:%S", tz = tz_set)
  f_time_3 <- as.numeric(f_time_2)
  f_time_y0 <- as.numeric(format(f_time_2, "%Y"))
  f_date_b1 <- date_bsum_2$start[f_time_y0 - year_bs + 1]
  f_date_b2 <- as.POSIXct(f_date_b1, format = "%Y-%m-%d %H:%M:%S", tz = tz_set)
  f_date_b3 <- as.numeric(f_date_b2)
  if(f_time_3 < f_date_b3){
    f_time_y <- f_time_y0 - 1
  }else{
    f_time_y <- f_time_y0
  }
  f_order <- f_time_y - year_bs + 1
  
  f_date_s <- date_bsum_2$start[f_order]
  f_date_e <- date_bsum_2$end[f_order]
  f_date_s2 <- as.POSIXct(f_date_s, format = "%Y-%m-%d %H:%M:%S", tz = tz_set)
  f_date_e2 <- as.POSIXct(f_date_e, format = "%Y-%m-%d %H:%M:%S", tz = tz_set)
  f_date_s3 <- as.numeric(f_date_s2)
  f_date_e3 <- as.numeric(f_date_e2)
  if(f_time_3 > f_date_s3 & f_time_3 < f_date_e3){
    f_time_4 <- f_time_3 - 3600
  }else{
    f_time_4 <- f_time_3
  }
  
  f_res <- list()
  f_res['time_3'] <- f_time_3
  f_res['time_4'] <- f_time_4
  f_res['time_y0'] <- f_time_y0
  f_res['time_y'] <- f_time_y
  f_res['order'] <- f_order
  f_res['date_b1'] <- f_date_b1
  f_res['date_b2'] <- f_date_b2
  f_res['date_b3'] <- f_date_b3
  f_res['date_s'] <- f_date_s
  f_res['date_e'] <- f_date_e
  f_res['date_s3'] <- f_date_s3
  f_res['date_e3'] <- f_date_e3

  return(f_res)
}

#up2024_0429_20:18_e
#=============
#up2024_0429_20:19_s

sunrise2_res <- rep(0, len_days)
sunset2_res <- rep(0, len_days)
for(ii in 1:len_days){
  c_res_1 <- time_adj_f(sun_times_2$sunrise[ii])$time_4
  c_res_2 <- as.POSIXct(c_res_1, origin = "1970-01-01", tz = tz_set)
  c_res_3 <- format(c_res_2, "%Y-%m-%d %H:%M:%S")
  sunrise2_res[ii] <- c_res_3
  
  c_res_4 <- time_adj_f(sun_times_2$sunset[ii])$time_4
  c_res_5 <- as.POSIXct(c_res_4, origin = "1970-01-01", tz = tz_set)
  c_res_6 <- format(c_res_5, "%Y-%m-%d %H:%M:%S")
  sunset2_res[ii] <- c_res_6
}

sun_times_2$sunrise2 <- sunrise2_res
sun_times_2$sunset2 <- sunset2_res

write.csv(sun_times_2, "sun_times_2.csv", row.names = FALSE)

#up2024_0429_20:19_e
