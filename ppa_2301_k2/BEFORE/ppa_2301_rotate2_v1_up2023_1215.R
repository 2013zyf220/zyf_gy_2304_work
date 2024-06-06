library(rgdal)
library(raster)
library(sf)
library(sp)
library(dplyr)
library(tidyr)
library(landscapemetrics)
library(ggplot2)
#rm(list = ls())

setwd("E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/shp/rotate")

#==================================
turn_f <- function(f_angle_1){
  if(f_angle_1 < 180){
    f_angle_2 <- f_angle_1 + 180
  }else{
    f_angle_2 <- f_angle_1 - 180
  }
  return(f_angle_2)
}

wind_dif_f <- function(f_1, f_2){
  f_3 <- f_2 - f_1
  if(f_3 > 180){
    f_4 <- f_3 - 360
  }else if(f_3 < -180){
    f_4 <- f_3 + 360
  }else{
    f_4 <- f_3
  }
  return(f_4)
}

#==================================

shp_1 <- st_read("2301_cq_water_rotate_4.shp") #to_be_set
shp_2 <- as.data.frame(shp_1)
shp_3 <- shp_2[,c("NUMBER", "XA_ANGLE_2", "NUMBER2")]
shp_3_rows <- nrow(shp_3)
shp_4 <- rep(0, shp_3_rows)
shp_5 <- cbind(shp_3, XA_ANGLE_3 = shp_4)

#==================================

shp_turn_1_sub <- list()
shp_turn_1_sub[[1]] <- c(1,49,72)   #to_be_set
shp_turn_1_sub[[2]] <- c(99:141)   #to_be_set
shp_turn_1_sub[[3]] <- c(144:174)   #to_be_set
shp_turn_1_sub[[4]] <- c(177:184)   #to_be_set
shp_turn_1 <- c(shp_turn_1_sub[[1]], shp_turn_1_sub[[2]], shp_turn_1_sub[[3]], shp_turn_1_sub[[4]])  #to_be_set
shp_turn_2 <- 142  #to_be_set
shp_turn_3 <- c(48,97)  #to_be_set
shp_turn_4 <- 185  #to_be_set
orders <- 1:185  #to_be_set

#==================================

for(ii in orders){
  if(ii %in% shp_turn_1){
    c_1 <- shp_5[shp_5$NUMBER == ii, "XA_ANGLE_2"]
    c_res <- turn_f(c_1)
    shp_5[shp_5$NUMBER == ii, "XA_ANGLE_3"] = c_res
  }else if(ii %in% shp_turn_2){
    c_1 <- shp_5[shp_5$NUMBER == ii & shp_5$NUMBER2 == 1, "XA_ANGLE_2"]
    c_2 <- shp_5[shp_5$NUMBER == ii & shp_5$NUMBER2 == 2, "XA_ANGLE_2"]
    c_res1 <- turn_f(c_1)    
    c_res2 <- turn_f(c_2)   
    shp_5[shp_5$NUMBER == ii & shp_5$NUMBER2 == 1, "XA_ANGLE_3"] = c_res1
    shp_5[shp_5$NUMBER == ii & shp_5$NUMBER2 == 2, "XA_ANGLE_3"] = c_res2
  }else if(ii %in% shp_turn_3){
    c_1 <- shp_5[shp_5$NUMBER == ii & shp_5$NUMBER2 == 1, "XA_ANGLE_2"]
    c_2 <- shp_5[shp_5$NUMBER == ii & shp_5$NUMBER2 == 2, "XA_ANGLE_2"]
    shp_5[shp_5$NUMBER == ii & shp_5$NUMBER2 == 1, "XA_ANGLE_3"] = c_1
    shp_5[shp_5$NUMBER == ii & shp_5$NUMBER2 == 2, "XA_ANGLE_3"] = c_2
  }else if(ii %in% shp_turn_4){
    c_1 <- shp_5[shp_5$NUMBER == ii & shp_5$NUMBER2 == 1, "XA_ANGLE_2"]
    c_2 <- shp_5[shp_5$NUMBER == ii & shp_5$NUMBER2 == 2, "XA_ANGLE_2"]
    c_res1 <- turn_f(c_1)
    c_res2 <- c_2
    shp_5[shp_5$NUMBER == ii & shp_5$NUMBER2 == 1, "XA_ANGLE_3"] = c_res1
    shp_5[shp_5$NUMBER == ii & shp_5$NUMBER2 == 2, "XA_ANGLE_3"] = c_res2
  }else{
    c_1 <- shp_5[shp_5$NUMBER == ii, "XA_ANGLE_2"]
    shp_5[shp_5$NUMBER == ii, "XA_ANGLE_3"] <- c_1
  }
}

#==================================

angled_f1 <- function(ii){
  f_data_1 <- shp_5[shp_5$NUMBER == ii, "XA_ANGLE_3"]
  f_data_2 <- shp_5[shp_5$NUMBER == ii + 1 & shp_5$NUMBER2 == 1, "XA_ANGLE_3"]
  f_data_3 <- wind_dif_f(f_data_1,f_data_2)
  return(f_data_3)
}

angled_f2 <- function(ii){
  f_data_1 <- shp_5[shp_5$NUMBER == ii & shp_5$NUMBER2 == 1, "XA_ANGLE_3"]
  f_data_2 <- shp_5[shp_5$NUMBER == ii & shp_5$NUMBER2 == 2, "XA_ANGLE_3"]
  f_data_3 <- wind_dif_f(f_data_1,f_data_2)
  return(f_data_3)
}

angled_f3 <- function(ii){
  f_data_1 <- shp_5[shp_5$NUMBER == ii, "XA_ANGLE_3"]
  f_data_2 <- shp_5[shp_5$NUMBER == ii + 1, "XA_ANGLE_3"]
  f_data_3 <- wind_dif_f(f_data_1,f_data_2)
  return(f_data_3)
}

data_a1 <- shp_5[shp_5$NUMBER == 71, "XA_ANGLE_3"]
data_a2 <- shp_5[shp_5$NUMBER == 185 & shp_5$NUMBER2 == 2, "XA_ANGLE_3"]
data_a3 <- wind_dif_f(data_a1,data_a2)

#==================================

grid_len <- 185 #to_be_set
angle_dif_1 <- rep(0,185) #to_be_set
angle_dif_2 <- rep(0,185) #to_be_set
angle_sub0 <- c(1:185) #to_be_set
angle_sub1 <- c(47, 96, 141, 184) #to_be_set
angle_sub2 <- c(48, 97, 142, 185) #to_be_set
angle_sub3 <- 71 #to_be_set
angle_sub_b1 <- c(1:97) #to_be_set
angle_sub_b2 <- c(98:185) #to_be_set

for(ii in angle_sub0){
  if (ii %in% angle_sub1){
    angle_dif_1[ii] <- angled_f1(ii)
  }
  else if(ii %in% angle_sub2){
    angle_dif_1[ii] <- angled_f2(ii)
  }else if(ii %in% angle_sub3){
    angle_dif_1[ii] <- data_a3
  }else{
    angle_dif_1[ii] <- angled_f3(ii)
  }
}

for(ii in angle_sub0){
  if (ii %in% angle_sub_b1){
    angle_dif_2[ii] <- -angle_dif_1[ii]
  }else{
    angle_dif_2[ii] <- angle_dif_1[ii]
  }
}

#==================================

shp_1$XA_ANGLE_3 <- shp_5$XA_ANGLE_3
shp_1$XA_ANDIF_1 <- rep(0, shp_3_rows)
shp_1$XA_ANDIF_2 <- rep(0, shp_3_rows)

for(ii in angle_sub0){
  shp_1[shp_1$NUMBER == ii & shp_1$NUMBER2 == 1, "XA_ANDIF_1"] <- angle_dif_1[ii]
  shp_1[shp_1$NUMBER == ii & shp_1$NUMBER2 == 1, "XA_ANDIF_2"] <- angle_dif_2[ii]
}

#st_write(shp_1, "2301_cq_water_rotate_5.shp")

shp_5$XA_ANDIF_1 <- shp_1$XA_ANDIF_1
shp_5$XA_ANDIF_2 <- shp_1$XA_ANDIF_2
write.csv(shp_5, "2301_cq_water_rotate_5a.csv")

shp_6 <- matrix(0, ncol = 2, nrow = grid_len)
shp_6[,1] <- angle_sub0
shp_6[,2] <- angle_dif_2

colnames_shp_6 <- c('NUMBER', 'XA_ANDIF_2')
colnames(shp_6) <- colnames_shp_6
write.csv(shp_6, "2301_cq_water_rotate_5b.csv")