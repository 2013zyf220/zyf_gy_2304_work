library(rgdal)
library(raster)
library(sf)
library(sp)
library(dplyr)
library(tidyr)
library(landscapemetrics)
library(ggplot2)
library(PerformanceAnalytics)
library(car)

order_1 <- 5  #to_be_set
buffer_1 <- 'd01s5'  #to_be_set  
interv <- c(0,5,10,15,20) #to_be_set 
vari_sel <- c('XY_rcd', 'XY_rci', 'XY_crci') #to_be_set
vari_sel_0 <- 'XY_rcd' #to_be_set

data_1 <- read.csv(paste0('res2/ppa_2301_ana_s', order_1, '_buf', buffer_1, '.csv'))

rows_sel <- list()
rows_sel_2 <- list()
  
len_interv <- length(interv) - 1
vari_sel_len <- length(vari_sel)
ave_vari <- matrix(0, nrow = len_interv, ncol = vari_sel_len + 1)

for(ii in 1: len_interv){
  c_1 <- interv[ii]
  c_2 <- interv[ii+1]
  rows_sel[[ii]] <- data_1[data_1[, vari_sel_0] >= c_1 & data_1[, vari_sel_0] < c_2, ]  
  rows_sel_2[[ii]] <- rows_sel[[ii]][, vari_sel]
  ave_vari[ii,1: vari_sel_len] <- colMeans(rows_sel_2[[ii]], na.rm = TRUE)
  ave_vari[ii, vari_sel_len + 1] <- nrow(rows_sel_2[[ii]])
}
  
colnames(ave_vari) <- c(vari_sel, 'Number')
write.csv(ave_vari, file = paste0('res2/2301_seperate_s', order_1, '_buf', buffer_1, '.csv'), row.names = TRUE)




