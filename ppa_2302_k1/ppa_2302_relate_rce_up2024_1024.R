library(raster)
library(sf)
library(landscapemetrics)
library(dplyr)
library(sp)
library(terra)
setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2302_k2/ARCGIS')

#=======================================
#up2024_0816_19:00

TEST_NUM <- 45 #to_be_set_key
buf_data_set <- 2 #to_be_set
buf_data_set2 <- 2 #to_be_set
len_strs_mo <- 6  #to_be_set
len_sites <- 50 #to_be_set

bh_1 <- rast("BH/BH_CP_4.tif")
bh_b <- raster("BH/BH_CP_4.tif")

if(buf_data_set == 1){
  lines_1 <- st_read('BH/str_lines3_b0_c2.shp')
}else if(buf_data_set == 2){
  lines_1 <- st_read(paste0('BH/TEST', TEST_NUM, '/str_lines3_c1_test', TEST_NUM, '.shp'))
}else{
  print('ERROR')
}


plot(bh_1)
plot(lines_1[1,], add = T)

#=======================================
#up2024_0816_19:00

buf_set <- 100  #to_be_set


if(buf_data_set == 1){
  buf_1 <- shapefile(paste0('BH/str_lines3_b6_buf', buf_set, '.shp'))
}else if(buf_data_set == 2){
  buf_1 <- shapefile(paste0('BH/TEST', TEST_NUM, '/str_lines5_buf', buf_set, '_test', TEST_NUM,'.shp'))
}else{
  print('ERROR')
}

luse_1p <- raster('LUSEB/landuse_b4.tif')
buf_1p <- spTransform(buf_1, crs(luse_1p))

plot(luse_1p)
plot(buf_1p, add = T)

class_bld <- 2 #to_be_set
class_veg <- 3 #to_be_set

#=======================================
#up2024_0816_19:00

bh_1_data <- list()
bh_1_data_mean <- rep(0, len_strs_mo)

for(ii in 1:len_strs_mo){
  c_1 <- (ii - 1) * 2 + 1
  c_2 <- ii * 2
  cat(c_1, c_2, '\n')
  bh_1_data[[ii]] <- terra::extract(bh_1, lines_1[c_1: c_2,], fun = NULL, na.rm = FALSE)
  bh_1_data_mean[ii] <- mean(bh_1_data[[ii]][['BH_CP_4']], na.rm = TRUE)
}

#=======================================
#up2024_0816_19:00

bh_2_data <- bh_1_data
bh_2_data_mean <- rep(0, len_strs_mo)
for(ii in 1:len_strs_mo){
  bh_2_data[[ii]][is.na(bh_2_data[[ii]])] <- 0
  bh_2_data_mean[ii] <- mean(bh_2_data[[ii]][['BH_CP_4']])
}

#=======================================
#up2024_0816_19:00


grid_num <- rep(0, len_strs_mo)
grid_x <- rep(0, len_strs_mo)
grid_y <- rep(0, len_strs_mo)
grid_ps_bld <- rep(0, len_strs_mo)
grid_ps_veg <- rep(0, len_strs_mo)

for(ii in 1: len_strs_mo){
  grid_num[ii] <- ii
  grid_x[ii] <- buf_1p@polygons[[ii]]@labpt[1]
  grid_y[ii] <- buf_1p@polygons[[ii]]@labpt[2];
  c_lss_0 <- sample_lsm(luse_1p, buf_1p[ii,], what = c('lsm_c_pland','lsm_c_cohesion'), shape = 'square'); #to_be_set
  c_class_lss <- data.frame(class = rep(c(1:3),2), metric = c(rep('pland',3),rep('cohesion',3))) #to_be_set
  c_lss <- left_join(c_class_lss, c_lss_0)
  c_lss$value <- ifelse(is.na(c_lss$value), 0, c_lss$value)
  
  c_pland_s <- c_lss %>% filter(metric == 'pland')
  c_pland_s$value <- as.numeric(round(c_pland_s$value, 2))
  grid_ps_bld[ii] <- c_pland_s$value[c_pland_s$class == class_bld]
  grid_ps_veg[ii] <- c_pland_s$value[c_pland_s$class == class_veg]
}

#=======================================
#up2024_0816_19:00

str_wid <- c(51,45,73,44,48,43)  #to_be_set
asp_1 <- rep(0, len_strs_mo)  #to_be_set
asp_2 <- rep(0, len_strs_mo)  #to_be_set
for(ii in 1:len_strs_mo){
  asp_1[ii] <- bh_1_data_mean[ii]/str_wid[ii]
  asp_2[ii] <- bh_2_data_mean[ii]/str_wid[ii]
}

#=======================================
#up2024_0816_19:00

bh_sub_2v <- list()
bh_sub_3 <- list()
bh_sub_4 <- list()
bh_sub_3_len <- rep(0, len_strs_mo)
bh_sub_4_len <- rep(0, len_strs_mo)
bh_sub_3_mean <- rep(0, len_strs_mo)
bh_sub_4_mean <- rep(0, len_strs_mo)
bh_ratio <- rep(0, len_strs_mo)

for(ii in 1: len_strs_mo){
  c_bh_sub_1 <- crop(bh_b, extent(buf_1p[ii, ]))
  c_bh_sub_1[is.na(c_bh_sub_1)] <- 0
  c_bh_sub_2 <- mask(c_bh_sub_1, buf_1p[ii, ])
  bh_sub_2v[[ii]] <- getValues(c_bh_sub_2)
  bh_sub_3[[ii]] <- na.omit(bh_sub_2v[[ii]])
  bh_sub_4[[ii]] <- bh_sub_3[[ii]][bh_sub_3[[ii]] != 0]
  bh_sub_3_len[ii] <- length(bh_sub_3[[ii]])
  bh_sub_4_len[ii] <- length(bh_sub_4[[ii]])
  bh_ratio[ii] <- bh_sub_4_len[ii]/bh_sub_3_len[ii]
  if(bh_sub_4_len[ii] == 0){
    bh_sub_3_mean[ii] <- 0
    bh_sub_4_mean[ii] <- 0
  }else{
    bh_sub_3_mean[ii] <- mean(bh_sub_3[[ii]])
    bh_sub_4_mean[ii] <- mean(bh_sub_4[[ii]])
  }
}

#=======================================
#up2024_0816_19:00

rce_relate_1 <- matrix(0, nrow = len_strs_mo, ncol = 12) #to_be_set
rce_relate_1[,1] <- grid_num
rce_relate_1[,2] <- grid_x
rce_relate_1[,3] <- grid_y
rce_relate_1[,4] <- grid_ps_bld
rce_relate_1[,5] <- grid_ps_veg
rce_relate_1[,6] <- str_wid
rce_relate_1[,7] <- bh_1_data_mean
rce_relate_1[,8] <- bh_2_data_mean
rce_relate_1[,9] <- bh_sub_3_mean
rce_relate_1[,10] <- round(bh_sub_4_mean,3)
rce_relate_1[,11] <- asp_1
rce_relate_1[,12] <- asp_2
rce_relate_2 <- do.call(rbind, replicate(6, rce_relate_1, simplify = FALSE))

rce_relate_1_df <- as.data.frame(rce_relate_1)
rce_relate_2_df <- as.data.frame(rce_relate_2)
colnames(rce_relate_1_df) <- c('NUMBER','GRID_X','GRID_Y','PS_BLD','PS_VEG','SW','BH_1', 'BH_2','BH_3', 'BH_4','ASP1','ASP2') #to_be_set
colnames(rce_relate_2_df) <- c('NUMBER','GRID_X','GRID_Y','PS_BLD','PS_VEG','SW','BH_1', 'BH_2','BH_3', 'BH_4','ASP1','ASP2') #to_be_set
if(buf_data_set2 == 1){
  write.csv(rce_relate_1_df, paste0('RCE/ppa_2302_rce_relate_buf', buf_set, '.csv'))
}else if(buf_data_set2 == 2){
  write.csv(rce_relate_1_df, paste0('RCE/ppa_2302_rce_relate_buf', buf_set, '_test', TEST_NUM, '.csv'))
  write.csv(rce_relate_2_df, paste0('RCE/ppa_2302_rce_relate2_buf', buf_set, '_test', TEST_NUM, '.csv'))
}else{
  print('ERROR')
}
