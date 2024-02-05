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

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/shp/6')

rw_1 <- st_read('arcgis/cq_water_rw4.shp')
rw_2 <-  as.data.frame(rw_1)
rw_3 <- rw_2[, c("NUMBER", "length")]
write.csv(rw_3, file = 'res2/ppa_2301_rw_1.csv', row.names = FALSE)
