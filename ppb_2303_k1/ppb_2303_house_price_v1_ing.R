library(readxl)
setwd("E:/zyf_gy/zyf_gy_2304_work/ppb_2303_k1")
data_1 <- read_excel('340个城市-二手房历史数据（2009-2023.10）.xlsx')

city_1 <- "成都"; #to_be_set
cond_1 <- data_1$城市 == city_1
data_2 = data_1[cond_1, c("城市","年份","月份","房屋单价")]
data_3 <- as.matrix(data_2)

year_s1 <- min(data_3[,"年份"])
year_s2 <- as.numeric(year_s1)
year_e1 <- max(data_3[,"年份"])
year_e2 <- as.numeric(year_e1)

cond_2a <- data_2$年份 == year_s1
data_4a <- data_2[cond_2a,]
month_s1 <- as.matrix(data_4a)[,"月份"]
month_s2 <- as.numeric(min(month_s1))

cond_2b <- data_2$年份 == year_e1
data_4b <- data_2[cond_2b,]
month_e1 <- as.matrix(data_4b)[,"月份"]
month_e2 <- as.numeric(max(month_e1))

months_1 <- seq(from = month_s2, to = 12)
months_2 <- seq(from = 1, to = 12)
months_3 <- seq(from = 1, to = month_e2)
years_1 <- seq(from = year_s2, to = year_e2)

data_len <- (year_e2 - year_s2 - 1) * 12 + month_e2 + (12 - month_s2) + 1
price_1 <- rep(0, data_len)
date_show <- c()
#=========================================

format_date <- function(f_year, f_month){
  f_month_2 <- sprintf("%02d", f_month)
  f_res <- paste(f_year, f_month_2, sep = "-")
  return(f_res)
}

#=========================================
cond_2_sum <- list();
data_2_sum <- list();
ii <- 0
for(c_year in years_1){
  c_cond_1 <- data_2$年份 == c_year
  c_data_1 <- data_2[c_cond_1, ]
  if(c_year == year_s2){
    for(c_month in months_1){
      ii <- ii + 1
      cat('ii:', ii, '_mon', c_month)
      
      cond_2_sum[[ii]] <- c_data_1$月份 == sprintf("%02d", c_month)
      data_2_sum[[ii]] <- c_data_1[cond_2_sum[[ii]], ]
      price_1[ii] <- data_2_sum[[ii]][,"房屋单价"]
      date_show <- c(date_show, format_date(c_year, c_month))
    }
  }else if(c_year == year_e2){
    for(c_month in months_3){
      ii <- ii + 1
      cat('ii:', ii, '_mon', c_month)
      cond_2_sum[[ii]] <- c_data_1$月份 == sprintf("%02d", c_month)
      data_2_sum[[ii]] <- c_data_1[cond_2_sum[[ii]], ]
      price_1[ii] <- data_2_sum[[ii]][,"房屋单价"]
      date_show <- c(date_show, format_date(c_year, c_month))
    }    
  }else{
    for(c_month in months_2){
      ii <- ii + 1
      cat('ii:', ii, '_mon', c_month)
      cond_2_sum[[ii]] <- c_data_1$月份 == sprintf("%02d", c_month)
      data_2_sum[[ii]] <- c_data_1[cond_2_sum[[ii]], ]
      price_1[ii] <- data_2_sum[[ii]][,"房屋单价"]
      date_show <- c(date_show, format_date(c_year, c_month))
    }    
  }
}

res_city <- cbind(date_show, price_1)
write.csv(res_city, file = paste0('ppb_2303_house_price_',city_1, '.csv'), row.names = FALSE)
