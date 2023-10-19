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

rc_ana_1f <- function(f_grid_num){
  f_dis_exa_1 <- crop(dis_2, extent(grid_2[f_grid_num,]));
  f_lst_exa_1 <- crop(lst_2, extent(grid_2[f_grid_num,]));
  
  f_dis_exa <- mask(f_dis_exa_1,grid_2[f_grid_num,]);
  f_lst_exa <- mask(f_lst_exa_1,grid_2[f_grid_num,]);
  
  #plot(f_dis_exa, col = custom_colors);
  #plot(grid_2[f_grid_num,],add = T);
  
  #plot(f_lst_exa, col = custom_colors);
  #plot(grid_2[f_grid_num,],add = T);
  
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
rc_ana_2f <- function(f_grid_num, f_thres_1){
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
rc_ana_3f <- function(f_grid_num, f_breaks_2_end, f_breaks_2_by){
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
rc_ana_3_meanf <- function(f_grid_num, f_breaks_2_end, f_breaks_2_by){
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
  
  f_file_name1 <- paste('data_df_3_mean_', year,'_', f_grid_num, '.jpg', sep = '');
  jpeg(f_file_name1, width = 800, height = 600, quality = 100)  # Adjust width, height, and quality as needed
  f_df <- data.frame(dis = f_breaks_2b, lst = f_data_df_3_mean)
  f_plot <- ggplot(data = f_df, aes(x = dis, y = lst)) + geom_point() + labs(title = 'Scatter Plot of distance vs lst by intervals',
                                                                             x = 'distance', y = 'LST')
  print(f_plot)
  dev.off()  # Close the jpeg device
  
  f_file_name2 <- paste('data_df_3_mean_', year, '_', f_grid_num, '.csv', sep = '');
  write.table(f_data_df_3_mean, file = f_file_name2);
  
  return(f_data_df_3_mean)
}

#up2023_0924_10:31_e

#=====================
#up2023_0924_10:31_s
rc_ana_4f <- function(f_grid_num, f_breaks_2_num){
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
  
  f_data_df_4 <- list();
  f_data_df_4[1] <- f_RCD_1;
  f_data_df_4[2] <- f_RCI_1;
  f_data_df_4[3] <- f_CRCI_1;
  f_data_df_4[4] <- grid_1b@polygons[[ii]]@labpt[1];
  f_data_df_4[5] <- grid_1b@polygons[[ii]]@labpt[2];
  return(f_data_df_4);
}

#up2023_0924_10:31_e

#=====================

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/raster');
dis_1 <- raster(paste0('a2301_disr2.tif')); #land cover data
dis_2 <- projectRaster(dis_1, crs = '+init=epsg:4326')

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/shp/outputs');
custom_colors <- colorRampPalette(c('white', 'red'))(256);

lst_ana <- function(f_year){
  setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/shp/outputs');
  f_grid_1a <- shapefile(paste0('2301_river_4_', f_year,'.shp'));
  f_grid_1b <- spTransform(f_grid_1a, '+init=epsg:4326');
  f_grid_2 <- st_read(paste0('2301_river_4_', f_year,'.shp'));
  
  setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/LST_DATA');
  f_lst_1 <- raster(paste0('ppa_2301_cq_lst_', f_year,'.tif')); #to_be_set
  f_lst_2 <- projectRaster(f_lst_1, crs = '+init=epsg:4326');
  
  jpeg(paste0('ppa_2301_cq_lst_v2_', f_year,'.jpg'), width = 800, height = 600, quality = 100)  # Adjust width, height, and quality as needed
  plot(f_lst_2, main = 'Land surface temperature', col = custom_colors);
  plot(f_grid_2, add = T);
  dev.off()  # Close the jpeg device
  
  jpeg(paste0('ppa_2301_cq_dis_2_', f_year,'.jpg'), width = 800, height = 600, quality = 100)  # Adjust width, height, and quality as needed
  plot(dis_2, main = 'Distance to river', col = custom_colors);
  plot(f_grid_2, add = T);
  dev.off()  
  
  f_grid_end1 <- length(f_grid_1b); #to_be_set
  f_thres_1 <- rep(20, times = f_grid_end1); #to_be_set
  f_breaks_2_end <- rep(500, times = f_grid_end1); #to_be_set
  f_breaks_2_by <- rep(20, times = f_grid_end1); #to_be_set
  
  f_grid_end2 <- length(f_grid_1b)
  f_grid_2$rx_x2 <- rep(0, f_grid_end2)
  f_grid_2$rx_y2 <- rep(0, f_grid_end2)
  f_grid_2$rx_rcd <- rep(0, f_grid_end2)
  f_grid_2$rx_rci <- rep(0, f_grid_end2)
  f_grid_2$rx_crci <- rep(0, f_grid_end2)
  
  f_data_df_1 <- list();
  f_data_df_2 <- list();
  f_data_df_3 <- list();
  f_data_df_3_mean <- list();
  f_data_df_4 <- list();
  f_data_df_4v <- matrix(0, nrow = 3, ncol = f_grid_end1);
  f_breaks_2_num <- list();
  
  for (ii in 1: f_grid_end1){
    f_breaks_2_num[[ii]] <- round(f_breaks_2_end[ii] / f_breaks_2_by[ii]);
    f_data_df_1[[ii]] <- rc_ana_1f(ii);
    f_data_df_2[[ii]] <- rc_ana_2f(ii, f_thres_1[ii]);
    f_data_df_3[[ii]] <- rc_ana_3f(ii, f_breaks_2_end[ii], f_breaks_2_by[ii]);
    f_data_df_3_mean[[ii]] <- rc_ana_3_meanf(ii, f_breaks_2_end[ii], f_breaks_2_by[ii]);
    f_data_df_4[[ii]] <- rc_ana_4f(ii, f_breaks_2_num[[ii]]);
    f_data_df_4v[1,ii] <- f_data_df_4[[ii]][1][[1]];
    f_data_df_4v[2,ii] <- f_data_df_4[[ii]][2][[1]];
    f_data_df_4v[3,ii] <- f_data_df_4[[ii]][3][[1]];
    
    f_grid_2$rx_x2[ii] <- f_data_df_4[[ii]][4][[1]];
    f_grid_2$rx_y2[ii] <- f_data_df_4[[ii]][5][[1]];
    f_grid_2$rx_rcd[ii] <- f_data_df_4[[ii]][1][[1]];
    f_grid_2$rx_rci[ii] <- f_data_df_4[[ii]][2][[1]];
    f_grid_2$rx_crci[ii] <- f_data_df_4[[ii]][3][[1]];
  }
  
  st_write(f_grid_2, paste0('2301_river_5_', f_year,'.shp'))
  write.csv(f_data_df_4v, file = paste0('2301_data_df_4v_', f_year,'.csv'), row.names = FALSE)
}

#=====================================

year_s <- 2019; #to_be_set
year_e <- 2019; #to_be_set

for(c_year in year_s: year_e){
  lst_ana(c_year);
}