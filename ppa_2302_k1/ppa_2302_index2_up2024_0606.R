library(sf)
library(terra)

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2302_k2/ARCGIS')
#=======================================
#up2024_0606_19:49

len_strs_mo <- 6  #to_be_set
len_sites <- 50 #to_be_set
len_all <- len_strs_mo * len_sites

lines_1 <- st_read('BH/str_lines2.shp')
bh_1 <- rast("BH/BH_CP_4.tif")

plot(bh_1)
plot(lines_1[1:2,], add = T)

#=======================================
#up2024_0606_19:49

bh_1_data <- list()
bh_1_data_mean <- rep(0, len_strs_mo)

for(ii in 1:len_strs_mo){
  c_1 <- (ii - 1) * 2 + 1
  c_2 <- ii * 2
  cat(c_1, c_2, '\n')
  bh_1_data[[ii]] <- extract(bh_1, lines_1[c_1: c_2,], fun = NULL, na.rm = FALSE)
  bh_1_data_mean[ii] <- mean(bh_1_data[[ii]][['BH_CP_4']], na.rm = TRUE)
}

#=======================================
#up2024_0606_19:49

bh_2_data <- bh_1_data
bh_2_data_mean <- rep(0, len_strs_mo)
for(ii in 1:len_strs_mo){
  bh_2_data[[ii]][is.na(bh_2_data[[ii]])] <- 0
  bh_2_data_mean[ii] <- mean(bh_2_data[[ii]][['BH_CP_4']])
}

#=======================================
#up2024_0606_19:49

bh_1_data_mean2 <- rep(0, len_all)
bh_2_data_mean2 <- rep(0, len_all)
for(ii in 1:len_strs_mo){
  c_1 <- (ii - 1) * 50 + 1
  c_2 <- ii * 50
  cat(c_1, c_2, '\n')
  bh_1_data_mean2[c_1:c_2] <- bh_1_data_mean[ii]
  bh_2_data_mean2[c_1:c_2] <- bh_2_data_mean[ii]
}

#========================================
#up2024_0606_19:49

dis_1 <- seq(10,500,10) #to_be_set
dis_2 <- rep(dis_1, len_strs_mo) #to_be_set

sw_1 <- rep(0, len_all)
sw_2 <- c(25,25,45,20,25,20)  #to_be_set

for(ii in 1:len_strs_mo){
  c_1 <- (ii - 1) * 50 + 1
  c_2 <- ii * 50
  sw_1[c_1:c_2] <- sw_2[ii]
}

asp_1 <- bh_1_data_mean2/sw_1
asp_2 <- bh_2_data_mean2/sw_1

#========================================
#up2024_0606_19:49

buf_set <- 100 #to_be_set_key
index_name1 <- paste0('index_1m_df1_buf', buf_set, '.csv')
index_name2 <- paste0('index_1m_df2_buf', buf_set, '.csv')

index_1 <- read.csv(index_name1)[1: len_all, ]
asp_3 <- index_1$bh_3_mean2/sw_1

index_1_add <- matrix(0, ncol = 5, nrow = len_all)
index_1_add[,1] <- dis_2
index_1_add[,2] <- sw_1
index_1_add[,3] <- asp_1
index_1_add[,4] <- asp_2
index_1_add[,5] <- asp_3

index_1_add_df <- as.data.frame(index_1_add)
colnames(index_1_add_df) <- c('dis','str_wid','asp_1','asp_2','asp_3')
index_2 <- cbind(index_1, index_1_add_df)
write.csv(index_2, index_name2)

#========================================