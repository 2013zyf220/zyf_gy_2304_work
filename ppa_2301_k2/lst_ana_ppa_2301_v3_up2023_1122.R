library(rgdal)
library(raster)
library(sf)
library(sp)
library(dplyr)
library(tidyr)
library(landscapemetrics)
library(ggplot2)
#rm(list = ls())

#============================================================================
#up2023_0924_10:31_s
#load and plot data

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2')
order_1 <- 1; #to_be_set_key
lst_data <- 2; #to_be_set

buffer_1 <- read.csv('shp/1/2301_cq_water_b10_buf500_a19_buf.csv')
buffer_2 <- buffer_1$buffer

grid_1a <- shapefile(paste0('shp/1/2301_cq_water_b10_buf500_a20.shp'));
grid_1b <- spTransform(grid_1a, '+init=epsg:4326');
grid_2 <- st_read(paste0('shp/1/2301_cq_water_b10_buf500_a20.shp'));

if(lst_data == 1){
  lst_1 <- raster(paste0('raster/ppa_2301_lst_s', order_1,'.tif'));
  dis_1 <- raster(paste0('raster/a2301_disr2.tif')); #land cover data
}else{
  lst_1 <- raster(paste0('raster/ppa_2301_lstb_s', order_1,'.tif'));
  dis_1 <- raster(paste0('raster/a2301_disr3.tif')); #land cover data
}

lst_2 <- projectRaster(lst_1, crs = '+init=epsg:4326')
dis_2 <- projectRaster(dis_1, crs = '+init=epsg:4326')

custom_colors <- colorRampPalette(c('white', 'red'))(256)

jpeg(paste0('shp/3/ppa_2301_rce_lst_v1_', order_1,'.jpg'), width = 800, height = 600, quality = 100)  # Adjust width, height, and quality as needed
plot(lst_2, main = 'Land surface temperature', col = custom_colors);
plot(grid_1b, add = T);
dev.off()  # Close the jpeg device

jpeg(paste0('shp/3/ppa_2301_rce_dis_v1_', order_1,'.jpg'), width = 800, height = 600, quality = 100)  # Adjust width, height, and quality as needed
plot(dis_2, main = 'Distance to river', col = custom_colors);
plot(grid_1b, add = T);
dev.off()  

#up2023_0924_10:31_e

#============================================================================
#up2023_0924_10:31_s

rc_ana_1 <- function(f_grid_num){
  f_dis_exa_1 <- crop(dis_2, extent(grid_1b[f_grid_num,]));
  f_lst_exa_1 <- crop(lst_2, extent(grid_1b[f_grid_num,]));
  
  f_dis_exa <- mask(f_dis_exa_1,grid_1b[f_grid_num,]);
  f_lst_exa <- mask(f_lst_exa_1,grid_1b[f_grid_num,]);
  
  plot(f_dis_exa, col = custom_colors);
  plot(grid_1b[f_grid_num,],add = T);
  
  plot(f_lst_exa, col = custom_colors);
  plot(grid_1b[f_grid_num,],add = T);
  
  f_disv_exa <- getValues(f_dis_exa);
  f_lstv_exa <- getValues(f_lst_exa);
  f_data_df_1 <- data.frame(dis = f_disv_exa, lst = f_lstv_exa);
  
  plot(f_data_df_1$dis, f_data_df_1$lst, main = 'Scatter Plot of distance vs LST', 
       xlab = 'distance', ylab = 'LST', pch = 19, col = 'black');
  
  return(f_data_df_1);
}

#up2023_0924_10:31_e

#==========================
#up2023_0924_10:31_s
rc_ana_2 <- function(f_grid_num, f_thres_1){
  f_breaks_1 <- c(0, f_thres_1, 80); #to_be_set
  f_labels_1 <- list();
  f_labels_1[1] <- paste('0 - ', f_thres_1);
  f_labels_1[2] <- paste(f_thres_1,' - 80');
  
  f_data_df_1 <- data_df_1[[f_grid_num]];
  f_data_df_1$bin <- cut(f_data_df_1$lst, breaks = f_breaks_1, labels = c(f_labels_1[1], f_labels_1[2]), include.lowest = TRUE);
  head(f_data_df_1);
  
  f_data_df_2 <- split(f_data_df_1, f_data_df_1$bin)[[2]];
  
  f_df <- data.frame(dis = f_data_df_2$dis, lst = f_data_df_2$lst)
  f_plot <- ggplot(data = f_df, aes(x = dis, y = lst)) + geom_point() + labs(title = 'Scatter Plot of distance vs lst (water temperature excluded)',
                   x = 'distance', y = 'LST');
  print(f_plot)
  
  return(f_data_df_2);
}

#up2023_0924_10:31_e

#==============================
#up2023_0924_10:31_s
rc_ana_3 <- function(f_grid_num, f_breaks_2_end, f_breaks_2_by){
  f_breaks_2 <- seq(0, f_breaks_2_end, by = f_breaks_2_by)
  f_data_df_2 <- data_df_2[[f_grid_num]]
  f_data_df_2$bin <- cut(f_data_df_2$dis, breaks = f_breaks_2, labels = FALSE, include.lowest = TRUE)
  head(f_data_df_2)
  
  f_data_df_3 <- split(f_data_df_2, f_data_df_2$bin)
  return(f_data_df_3);
}

#up2023_0924_10:31_e

#===========================
#up2023_0924_10:31_s
rc_ana_3_mean <- function(f_grid_num, f_breaks_2_end, f_breaks_2_by){
  f_breaks_2b <- seq(f_breaks_2_by, f_breaks_2_end, by = f_breaks_2_by)
  f_breaks_2_num <- round(f_breaks_2_end / f_breaks_2_by)
  
  f_data_df_3 <- data_df_3[[f_grid_num]]
  f_data_df_3_mean <- rep(0, times = f_breaks_2_num)
  
  for (f_ii in 1:f_breaks_2_num) {
    f_ii_2 <- toString(f_ii);
    t_data_1 <- f_data_df_3[[f_ii_2]];
    if (is.null(t_data_1)) {
      f_data_df_3_mean[f_ii] <- NA
      print('The variable is NULL.');
    } else { 
      f_data_df_3_mean[f_ii] <- mean(f_data_df_3[[f_ii_2]]$lst)
    }
  }
  
  f_file_name1 <- paste('ppa_2301_rce_3_mean_', order_1, '_', f_grid_num, '.jpg', sep = '');
  jpeg(f_file_name1, width = 800, height = 600, quality = 100)  # Adjust width, height, and quality as needed
  f_df <- data.frame(dis = f_breaks_2b, lst = f_data_df_3_mean)
  f_plot <- ggplot(data = f_df, aes(x = dis, y = lst)) + geom_point() + labs(title = 'Scatter Plot of distance vs lst by intervals', x = 'distance', y = 'LST')
  print(f_plot)
  dev.off()  # Close the jpeg device
  
  f_file_name2 <- paste('ppa_2301_rce_3_mean_', order_1, '_', f_grid_num, '.csv', sep = '');
  write.table(f_data_df_3_mean, file = f_file_name2);
  
  return(f_data_df_3_mean)
}

#up2023_0924_10:31_e

#=====================
#up2023_0924_10:31_s
rc_ana_4 <- function(f_grid_num, f_breaks_2_num){
  f_breaks_2_num_2 <- f_breaks_2_num - 5;  #to_be_set
  f_data_df_3_mean <- data_df_3_mean[[f_grid_num]];
  
  for (f_ii in 1:f_breaks_2_num_2){
    if (is.na(f_data_df_3_mean[f_ii]) || f_data_df_3_mean[f_ii] <= max(f_data_df_3_mean[f_ii + 1:f_ii + 5], na.rm = TRUE)) {
    } else {
      cat('serial number of polygon ',f_grid_num,' is: ',  f_ii, '\n');
      f_RCD_1 <- f_ii;
      break;
    }
  }
  
  if (!is.na(f_data_df_3_mean[1])) {
    f_data_df_3_mean_s <- f_data_df_3_mean[1];
  } else if (!is.na(f_data_df_3_mean[2])){
    f_data_df_3_mean_s <- f_data_df_3_mean[2];
  } else {
    f_data_df_3_mean_s <- f_data_df_3_mean[3];
  }
  
  f_RCI_1 <- f_data_df_3_mean[f_RCD_1] - f_data_df_3_mean_s;
  f_CRCI_1 <- sum(f_data_df_3_mean[1:f_RCD_1], na.rm = TRUE)
  
  if(f_RCI_1 <= 0){
    f_RCI_2 <- 0;
    f_RCD_2 <- 0;
    f_CRCI_2 <- 0;
  }else{
    f_RCI_2 <- log(f_RCI_1);
    f_CRCI_2 <- log(f_CRCI_1);
    f_RCD_2 <- log(f_RCD_1);
  }
  
  f_RCI_3 <- exp(f_RCI_1);
  f_CRCI_3 <- exp(f_CRCI_1);
  f_RCD_3 <- exp(f_RCD_1);
  
  f_data_df_4 <- list();
  f_data_df_4[1] <- f_RCD_1;
  f_data_df_4[2] <- f_RCI_1;
  f_data_df_4[3] <- f_CRCI_1;
  f_data_df_4[4] <- f_RCD_2;
  f_data_df_4[5] <- f_RCI_2;
  f_data_df_4[6] <- f_CRCI_2;
  f_data_df_4[7] <- f_RCD_3;
  f_data_df_4[8] <- f_RCI_3;
  f_data_df_4[9] <- f_CRCI_3;
  f_data_df_4[10] <- grid_1b@polygons[[ii]]@labpt[1];
  f_data_df_4[11] <- grid_1b@polygons[[ii]]@labpt[2];
  return(f_data_df_4);
}

#up2023_0924_10:31_e

#=====================
#up2023_0924_10:31_s
grid_end1 <- length(grid_1b); #to_be_set
thres_1 <- rep(20, times = grid_end1); #to_be_set
breaks_2_end <- buffer_2;
breaks_2_by <- rep(20, times = grid_end1); #to_be_set

grid_end2 <- length(grid_1b)
grid_2$rx_x2 <- rep(0, grid_end2)
grid_2$rx_y2 <- rep(0, grid_end2)
grid_2$rx_rcd <- rep(0, grid_end2)
grid_2$rx_rci <- rep(0, grid_end2)
grid_2$rx_crci <- rep(0, grid_end2)
grid_2$rx_rcd2 <- rep(0, grid_end2)
grid_2$rx_rci2 <- rep(0, grid_end2)
grid_2$rx_crci2 <- rep(0, grid_end2)
grid_2$rx_rcd3 <- rep(0, grid_end2)
grid_2$rx_rci3 <- rep(0, grid_end2)
grid_2$rx_crci3 <- rep(0, grid_end2)

data_df_1 <- list();
data_df_2 <- list();
data_df_3 <- list();
data_df_3_mean <- list();
data_df_4 <- list();
data_df_4v <- matrix(0, nrow = grid_end1, ncol = 12);
breaks_2_num <- list();


for (ii in 1: grid_end1){
  breaks_2_num[[ii]] <- round(breaks_2_end[ii] / breaks_2_by[ii]);
  data_df_1[[ii]] <- rc_ana_1(ii);
  data_df_2[[ii]] <- rc_ana_2(ii, thres_1[ii]);
  data_df_3[[ii]] <- rc_ana_3(ii, breaks_2_end[ii], breaks_2_by[ii]);
  data_df_3_mean[[ii]] <- rc_ana_3_mean(ii, breaks_2_end[ii], breaks_2_by[ii]);
  data_df_4[[ii]] <- rc_ana_4(ii, breaks_2_num[[ii]]);
  data_df_4v[ii,1] <- data_df_4[[ii]][1][[1]];
  data_df_4v[ii,2] <- data_df_4[[ii]][2][[1]];
  data_df_4v[ii,3] <- data_df_4[[ii]][3][[1]];
  data_df_4v[ii,4] <- data_df_4[[ii]][4][[1]];
  data_df_4v[ii,5] <- data_df_4[[ii]][5][[1]];
  data_df_4v[ii,6] <- data_df_4[[ii]][6][[1]];
  data_df_4v[ii,7] <- data_df_4[[ii]][7][[1]];
  data_df_4v[ii,8] <- data_df_4[[ii]][8][[1]];
  data_df_4v[ii,9] <- data_df_4[[ii]][9][[1]];
  data_df_4v[ii,10] <- data_df_4[[ii]][10][[1]];
  data_df_4v[ii,11] <- data_df_4[[ii]][11][[1]];
  data_df_4v[ii,12] <- ii;
  
  grid_2$rx_rcd[ii] <- data_df_4[[ii]][1][[1]];
  grid_2$rx_rci[ii] <- data_df_4[[ii]][2][[1]];
  grid_2$rx_crci[ii] <- data_df_4[[ii]][3][[1]];
  grid_2$rx_rcd2[ii] <- data_df_4[[ii]][4][[1]];
  grid_2$rx_rci2[ii] <- data_df_4[[ii]][5][[1]];
  grid_2$rx_crci2[ii] <- data_df_4[[ii]][6][[1]];
  grid_2$rx_rcd3[ii] <- data_df_4[[ii]][7][[1]];
  grid_2$rx_rci3[ii] <- data_df_4[[ii]][8][[1]];
  grid_2$rx_crci3[ii] <- data_df_4[[ii]][9][[1]];
  grid_2$rx_x2[ii] <- data_df_4[[ii]][10][[1]];
  grid_2$rx_y2[ii] <- data_df_4[[ii]][11][[1]];
}

data_df_4v_col <- c('rx_rcd', 'rx_rci', 'rx_crci', 'rx_rcd2', 'rx_rci2', 'rx_crci2', 'rx_rcd3', 'rx_rci3', 'rx_crci3', 'rx_x', 'rx_y', 'NUMBER')
colnames(data_df_4v) <- data_df_4v_col
st_write(grid_2, paste0('shp/3/ppa_2301_rce_s', order_1,'.shp'))
write.csv(data_df_4v, file = paste0('shp/3/ppa_2301_rce_s',order_1,'.csv'), row.names = FALSE)

#up2023_0924_10:31_e

