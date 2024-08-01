library(raster)

setwd('D:/zyf_gn/zyf_gn_2301_data/ppa_2401_k1')

#==========================
#up2024_08:01_10:00

reso_set <- 1  #to_be_set
sub_area_set <- 1 #to_be_set
lu_set <- 2 #to_be_set_key

bh_1 <- raster(paste0('BH_1/BH_4_R', reso_set, '_S', sub_area_set, '.tif'))
lu_1 <- raster(paste0('LU_', lu_set, '/LU', lu_set, '_S', sub_area_set, 'p.tif'))

#==========================
#up2024_08:01_10:10

ras_size <- 300 #to_be_set
len_x <- 90 #to_be_set
len_y <- 90 #to_be_set
start_x0 <- 100000 #to_be_set
start_y0 <- 792000 #to_be_set
end_x <- start_x0 + len_x * ras_size
end_y <- start_y0 + len_y * ras_size
start_x <- start_x0 + 1
start_y <- start_y0 + 1

#==========================
#up2024_08:01_10:10

extent_boxs <- list()
bh_2 <- list()
lu_2 <- list()
bh_3 <- list()
lu_3 <- list()
len_bh_3 <- matrix(0, nrow = len_x, ncol = len_y)
len_lu_3 <- matrix(0, nrow = len_x, ncol = len_y)
bh_r1 <- matrix(0, nrow = len_x, ncol = len_y)
bu_r1 <- matrix(0, nrow = len_x, ncol = len_y)
lu_r1 <- matrix(0, nrow = len_x, ncol = len_y)

kk <- 0 
for(ii_x in 1: len_x){
  extent_boxs[[ii_x]] <- list()
  bh_2[[ii_x]] <- list()
  lu_2[[ii_x]] <- list()
  bh_3[[ii_x]] <- list()
  lu_3[[ii_x]] <- list()
  for(ii_y in 1: len_y){
    kk <- kk + 1
    cat(kk, '\n')
    
    c_1 <- start_y0 + (ii_y - 1) * ras_size + 1
    c_2 <- start_y0 + ii_y * ras_size
    c_3 <- start_x0 + (ii_x - 1) * ras_size + 1
    c_4 <- start_x0 + ii_x * ras_size
    #cat(c_1, c_2, c_3, c_4, '\n')
    extent_boxs[[ii_x]][[ii_y]] <- extent(c_1, c_2, c_3, c_4)
    c_bh_2 <- crop(bh_1, extent_boxs[[ii_x]][[ii_y]])
    c_lu_2 <- crop(lu_1, extent_boxs[[ii_x]][[ii_y]])
    c_bh_2[is.na(c_bh_2)] <- 0
    bh_2[[ii_x]][[ii_y]] <- c_bh_2
    lu_2[[ii_x]][[ii_y]] <- c_lu_2
    
    c_bh_3 <- as.matrix(c_bh_2)
    c_lu_3 <- as.matrix(c_lu_2)
    bh_3[[ii_x]][[ii_y]] <- c_bh_3
    lu_3[[ii_x]][[ii_y]] <- c_lu_3
    len_bh_3[ii_x,ii_y] <- length(bh_3[[ii_x]][[ii_y]])
    len_lu_3[ii_x,ii_y] <- length(lu_3[[ii_x]][[ii_y]])                
                       
    if(sum(c_bh_3 > 0) == 0){
      bh_r1[ii_x,ii_y] <- 0
      bu_r1[ii_x,ii_y] <- 0
    }else{
      c_bh_3b <- c_bh_3[c_bh_3 > 0]
      bh_r1[ii_x,ii_y] <- mean(c_bh_3b)
      bu_r1[ii_x,ii_y] <- sum(c_bh_3b > 0)/(len_bh_3[ii_x,ii_y])
    }
    
    if(lu_set == 1){
      lu_r1[ii_x,ii_y] <- sum(c_lu_3 == 8, na.rm = TRUE)/(len_lu_3[ii_x,ii_y])
    }else if(lu_set == 2){
      lu_r1[ii_x,ii_y] <- sum(c_lu_3 == 1|c_lu_3 == 6, na.rm = TRUE)/(len_lu_3[ii_x,ii_y])
    }else{
      print('ERROR!')
    }
  }
}

#===================================
#up2024_08:01_10:10

palette_1 <- colorRampPalette(c('lightgreen', 'darkgreen'))

raster_plot_f <- function(f_data_1, f_sel_x, f_sel_y){
  if(f_data_1 == 'BH'){
    f_data_2 <- bh_2
  }else if(f_data_1 == 'LU'){
    f_data_2 <- lu_2
  }else{
    print('ERROR!')
  }
  
  f_data_3 <- f_data_2[[f_sel_x]][[f_sel_y]]
  f_data_3m <- as.matrix(f_data_3)
  f_minv <- min(f_data_3m, na.rm = TRUE)
  f_maxv <- max(f_data_3m, na.rm = TRUE)

  plot(f_data_3, col = palette_1(100), main = paste0(f_data_1, '_', f_sel_x, '_', f_sel_y), legend.args = list(text = 'Value', side = 4, font = 2, line = 2.5))
  legend('topright', legend = c(f_minv, f_maxv), fill = palette_1(2), title = 'Legend', bty = 'n')
}

sel_x <- 1 #to_be_set
sel_y <- 13 #to_be_set

raster_plot_f('BH', sel_x, sel_y) #to_be_set

#===================================
#up2024_08:01_10:10

write.csv(bh_r1, file = paste0('DATA_1/D1_BH_S', sub_area_set, '_R', reso_set, '_L', lu_set, '.csv'), row.names = FALSE)
write.csv(bu_r1, file = paste0('DATA_1/D1_BU_S', sub_area_set, '_R', reso_set, '_L', lu_set, '.csv'), row.names = FALSE)
write.csv(lu_r1, file = paste0('DATA_1/D1_LU_S', sub_area_set, '_R', reso_set, '_L', lu_set, '.csv'), row.names = FALSE)