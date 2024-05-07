library(terra)
library(data.table)
library(dplyr)
library(raster)
library(rgdal)
library(utils)
library(rgdal)
library(sf)
library(sp)
library(ggplot2)

setwd("E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/V2");

data_1 <- st_read("DATA_DIREC_1/cq_water_direc_3.shp")
data_2 <- data_1[c("NUMBER0b", "NUMBER","BEARING")]
data_3 <- st_set_geometry(data_2, NULL)
data_4 <- data_3[186:nrow(data_3), ]

#=============================

trans_f1 <- function(f_x){
  if(f_x == 48){
    f_y = 1001
  }else if(f_x == 71){
    f_y = 1002
  }else if(f_x == 97){
    f_y = 1003
  }else if(f_x == 104){
    f_y = 1004
  }else if(f_x == 185){
    f_y = 1002
  }else if(f_x > 0 & f_x < 186){
    f_y = f_x + 1
  }else{
    print("ERROR")
  }
  return(f_y)
}

#=============================

angle_dif <- function(f_1, f_2) {
  f_dif <- abs(f_1 - f_2)
  return(min(f_dif, 360 - f_dif))
}

#=============================

angle_f1 <- function(f_row_1){
  f_row_2 <- trans_f1(f_row_1)
  f_data_1 <- data_4[data_4$NUMBER == f_row_1, ]
  f_data_2 <- data_4[data_4$NUMBER == f_row_2, ]
  f_angle_1 <- f_data_1$BEARING
  f_angle_2 <- f_data_2$BEARING
  f_angle_dif <- angle_dif(f_angle_1, f_angle_2)
  
  f_res <- list()
  f_res["angle_1"] <- f_angle_1
  f_res["angle_2"] <- f_angle_2
  f_res["angle_dif"] <- f_angle_dif
  return(f_res)
}

#=============================
angle_dif_res_0 <- rep(0,185)
for(ii in 1:185){
  cat("number:",ii)
  angle_dif_res_0[ii] <- angle_f1(ii)[["angle_dif"]]
}

angle_dif_res <- round(angle_dif_res_0, 3)
df <- data.frame(NUMBER = 1:185, rotate = angle_dif_res)
write.csv(df, "DATA_ANA_1/ppa_2301_rotate.csv", row.names = FALSE)
