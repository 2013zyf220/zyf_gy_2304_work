library(raster)

setwd('D:/zyf_gn/zyf_gn_2301_data/ppa_2401_k1')

reso_set <- 1  #to_be_set
sub_area_set <- 1 #to_be_set
lu_set <- 2 #to_be_set_key

ras_size <- 300 #to_be_set
len_x <- 90 #to_be_set
len_y <- 90 #to_be_set
start_x0 <- 100000 #to_be_set
start_y0 <- 792000 #to_be_set
end_x <- start_x0 + len_x * ras_size
end_y <- start_y0 + len_y * ras_size
start_x <- start_x0 + 1
start_y <- start_y0 + 1

bh_r1 <- as.matrix(read.csv(paste0('DATA_1/D1_BH_S', sub_area_set, '_R', reso_set, '_L', lu_set, '.csv')))
bu_r1 <- as.matrix(read.csv(paste0('DATA_1/D1_BU_S', sub_area_set, '_R', reso_set, '_L', lu_set, '.csv')))
lu_r1 <- as.matrix(read.csv(paste0('DATA_1/D1_LU_S', sub_area_set, '_R', reso_set, '_L', lu_set, '.csv')))

#=========================
#up2024_08:01_10:10

lcz_f <- function(f_bh, f_bu, f_lu){
  if(f_bu < 0.05){
    f_lcz <- 0
  }else if(f_bu > 0.2){
    if(f_bh > 50){
      f_lcz <- 1
    }else if(f_bh < 50 & f_bh > 25){
      f_lcz <- 2
    }else{
      f_lcz <- 3
    }
  }else{
    if(f_bh > 50){
      f_lcz <- 4
    }else if(f_bh < 50 & f_bh > 25){
      f_lcz <- 5
    }else{
      f_lcz <- 6
    }    
  }
  return(f_lcz)
}

#=========================
#up2024_08:01_10:10

lcz_1 <- matrix(0, nrow = len_x, ncol = len_y)
for(ii_x in 1: len_x){
  for(ii_y in 1: len_y){
    c_bh <- bh_r1[ii_x,ii_y]
    c_bu <- bu_r1[ii_x,ii_y]
    c_lu <- lu_r1[ii_x,ii_y]
    lcz_1[ii_x,ii_y] <- lcz_f(c_bh,c_bu,c_lu)
  }
}

#=========================
#up2024_08:01_10:10

fig_raster_f <- function(f_data_1){
  if(f_data_1 == 'LCZ'){
    f_data_2 <- raster(lcz_1)
  }else if(f_data_1 == 'BH'){
    f_data_2 <- raster(bh_r1)
  }else if(f_data_1 == 'BU'){
    f_data_2 <- raster(bu_r1)
  }else if(f_data_1 == 'LU'){
    f_data_2 <- raster(lu_r1)
  }else{
    print('ERROR!')
  }
  extent(f_data_2) <- c(start_y, end_y, start_x, end_x)
  crs(f_data_2) <- '+proj=aea +lat_1=15 +lat_2=65 +lat_0=30 +lon_0=95 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs'
  plot(f_data_2, main = paste0(f_data_1, ' map'))
  writeRaster(f_data_2, filename = paste0('FIG_1/F1_', f_data_1, '_S', sub_area_set, '_R', reso_set, '_L', lu_set, '.tif'), format = 'GTiff', overwrite = TRUE)
  return(f_data_2)
}

data_sum_1 <- c('LCZ', 'BH', 'BU', 'LU') #to_be_set

fig_raster_res <- list()
for(c_data in data_sum_1){
  fig_raster_res[[c_data]] <- fig_raster_f(c_data)
}

#=========================
#up2024_08:01_10:10

hist_f <- function(f_data_1){
  if(f_data_1 == 'LCZ'){
    f_data_2 <- lcz_1
  }else if(f_data_1 == 'BH'){
    f_data_2 <- bh_r1
  }else if(f_data_1 == 'BU'){
    f_data_2 <- bu_r1
  }else if(f_data_1 == 'LU'){
    f_data_2 <- lu_r1
  }else{
    print('ERROR!')
  }  
  
  jpeg(filename = paste0('FIG_1/HIST1_', f_data_1, '_S', sub_area_set, '_R', reso_set, '_L', lu_set,'.jpg'))
  hist(f_data_2, main = paste0('Hist of ', f_data_1), xlab = 'Values', ylab = 'Frequency', col = 'skyblue', border = 'black')
  dev.off()
  return(f_data_2)
}

hist_res <- list()
for(c_data in data_sum_1){
  hist_res[[c_data]] <- hist_f(c_data)
}

