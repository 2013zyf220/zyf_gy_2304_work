library(MASS)
library(skimr)
library(DataExplorer)
library(tidyverse)
library(caret)
library(pROC)
library(ggplot2)
library(readxl)
library(raster)

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2302_k2/ARCGIS')

#=========================================================================

len_strs_mo <- 6  #to_be_set
len_sites <- 50 #to_be_set
len_all <- len_strs_mo * len_sites

buf_set <- 100 #to_be_set

svf_1 <- read.csv('D:/zyf_gn/zyf_gn_2301_data/ppa_2302_k1/SVF_PHOTOS/SVF_DATA2.csv')
svf_1m <- as.matrix(svf_1)

tc_1 <- raster('E:/zyf_gn/zyf_gn_2301_data/ppa_2302_k2/ARCGIS/TC/TC_4.tif')
strs_1 <- shapefile(paste0('E:/zyf_gn/zyf_gn_2301_data/ppa_2302_k2/ARCGIS/LINES/streets_5_buf', buf_set, '.shp'))
strs_1_len <- nrow(strs_1)

tc_2 <- list()
tc_3 <- list()
tc_3v <- list()
tc_4 <- list()
tc_5 <- list()
tc_4_len <- rep(0, len_all)
tc_5_len <- rep(0, len_all)
tc_cover <- rep(0, len_all)
for(ii in 1: len_all){
  if(ii %% 100 == 0){
    cat('buffer number:', ii, '\n')
  }
  tc_2[[ii]] <- crop(tc_1, extent(strs_1[ii, ]))
  tc_3[[ii]] <- mask(tc_2[[ii]], strs_1[ii, ])
  tc_3v[[ii]] <- getValues(tc_3[[ii]])
  tc_4[[ii]] <- na.omit(tc_3v[[ii]])
  tc_4_len[ii] <- length(tc_4[[ii]])
  tc_5[[ii]] <- tc_4[[ii]][tc_4[[ii]] != 0]
  tc_5_len[ii] <- length(tc_5[[ii]])
  tc_cover[ii] <- tc_5_len[ii]/tc_4_len[ii]
}

res_1 <- cbind(tc_cover, svf_1m)
res_2 <- as.data.frame(res_1)
colnames(res_2) <- c('TREECOVER','SVF')


#=========================

index_1 <- read.csv(paste0('E:/zyf_gn/zyf_gn_2301_data/ppa_2302_k2/ARCGIS/index_1m_df3_buf', buf_set, '.csv'))
index_2 <- cbind(index_1, res_2)
write.csv(index_2, paste0('E:/zyf_gn/zyf_gn_2301_data/ppa_2302_k2/ARCGIS/index_1m_df4_buf', buf_set, '.csv'))
