library(MASS)
library(gbm)
library(randomForest)
library(skimr)
library(DataExplorer)
library(tidyverse)
library(caret)
library(pROC)
library(ggplot2)
library(pdp)
library(readxl)

setwd("E:/zyf_gn/zyf_gn_2301_data/ppa_2302_k2/DATA_PRO_1")

#====================

order_1_f <- function(f_x){
  if (f_x >= 1 & f_x <= 50){
    f_y <- f_x
  }else if(f_x >= 51 & f_x <= 100){
    f_y <- 101 - f_x
  }else{
    print("ERROR")
  }
  return(f_y)
}

#====================

data_pro1_f <- function(f_route_n, f_adj_1){
  f_data_1 <- read_excel(paste0('RH_TP_NO', f_route_n, '_1.xlsx'));
  f_adj_2 <- f_adj_1 + 720 - 1
  f_data_2 <- f_data_1[f_adj_1: f_adj_2,]
  f_data_3 <- f_data_2[c(seq(1, nrow(f_data_2), by = 6)), ]
  f_data_4 <- rbind(f_data_3[1:50, ], f_data_3[61:110, ])
  
  f_res <- list(data_1 = f_data_1, data_4 = f_data_4)
  return(f_res)
}

#====================

adj_para_f <- function(f_route_n, time_f){
  adj_para_1 <- read_excel('adj_para_1_temp.xlsx',skip = 0);
  if(f_route_n == 1){
    f_y1 <- adj_para_1$NO1
  }else if(f_route_n == 2){
    f_y1 <- adj_para_1$NO2
  }else if(f_route_n == 3){
    f_y1 <- adj_para_1$NO3
  }else if(f_route_n == 4){
    f_y1 <- adj_para_1$NO4
  }else{
    print("ERROR")
  }
  f_y2 <- f_y1[time_f]
  return(f_y2)
}

day_set <- 1  #to_be_set_key
time_0_set <- 1 #to_be_set_key
time_1 <- time_0_set + (day_set - 1) * 3
route_set <- 1 #to_be_set_key
route_ref <- 4
adj_para_set <- adj_para_f(route_set, time_1)
adj_para_ref <- adj_para_f(route_ref, time_1)
#====================
data_s1 <- data_pro1_f(route_set, adj_para_set)$data_4
data_r1 <- data_s1[1:50,]
data_r2 <- data_s1[51:100,]

data_s0 <- data_pro1_f(route_ref, adj_para_ref)$data_4
data_r0a <- data_s0[1:50,]
data_r0b <- data_s0[51:100,]

data_r1_TP <- data_r1$TP
data_r1_RH <- data_r1$RH

data_r2_TP <- rev(data_r2$TP)
data_r2_RH <- rev(data_r2$RH)

data_r0a_TP <- data_r0a$TP
data_r0a_RH <- data_r0a$RH

data_r0b_TP <- rev(data_r0b$TP)
data_r0b_RH <- rev(data_r0b$RH)

data_r1_TP_dif <- data_r1_TP - data_r0a_TP

#===========










#接下来：编号（如332）建函数，求各时段平均值


#===========
routes <- 1:3
for (ii in routes) {
  print(i)
}