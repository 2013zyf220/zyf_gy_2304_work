library(MASS)
library(skimr)
library(DataExplorer)
library(tidyverse)
library(caret)
library(pROC)
library(ggplot2)
library(pdp)
library(readxl)

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2302_k2/ARCGIS')

ele_1 <- raster(paste0('DEM/DEM_ELE_2.tif'))
site_1 <- st_read('LINES/streets_5.shp')[ ,1]

plot(ele_1)
plot(site_1, add = T)

ele_2 <- extract(ele_1, site_1)
site_ref <- 101 #to_be_set
ele_ref <- ele_2[site_ref]

ele_2_dif <- ele_2 - ele_ref
TP_adj_1 <- ele_2_dif * -0.006

len_site <- 50 #to_be_set
len_rou <- 6 #to_be_set
TP_adj_2 <- matrix(0, nrow = len_site, ncol = len_rou)
ele_2_table <- matrix(0, nrow = len_site, ncol = len_rou)

for(ii in 1: len_rou){
  c_s <- (ii - 1) * len_site + 1
  c_e <- ii * len_site
  cat(c_s,' to ', c_e, '\n')
  TP_adj_2[ ,ii] <- TP_adj_1[c_s: c_e]
  ele_2_table[ ,ii] <- ele_2[c_s: c_e]
}

TP_adj_2_df <- as.data.frame(TP_adj_2)
ele_2_table_df <- as.data.frame(ele_2_table)
rou_titles <- c('rou1','rou2','rou3','rou4','rou5','rou6')
colnames(TP_adj_2_df) <- rou_titles
colnames(ele_2_table_df) <- rou_titles

write.csv(TP_adj_2_df, file = 'TP_adj_2.csv', row.names = FALSE)
write.csv(ele_2_table_df, file = 'ele_2_table.csv', row.names = FALSE)