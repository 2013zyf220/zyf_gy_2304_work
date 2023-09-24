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

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/shp');
grid_1 <- shapefile('2301_cq_water_10_a2.shp');
grid_2 <- spTransform(grid_1, '+init=epsg:4326');

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/LST_DATA');
lst_1 <- raster(paste0('ppa_2301_cq_lst_2019.tif')); #to_be_set
lst_2 <- projectRaster(lst_1, crs = '+init=epsg:4326')

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/raster');
dis_1 <- raster(paste0('a2301_disr2.tif')); #land cover data
dis_2 <- projectRaster(dis_1, crs = '+init=epsg:4326')

setwd('E:/zyf_gy/zyf_gy_2304_work/ppa_2301_k2/outputs');
custom_colors <- colorRampPalette(c("white", "red"))(256)

jpeg('ppa_2301_lst_2.jpg', width = 800, height = 600, quality = 100)  # Adjust width, height, and quality as needed
plot(lst_2, main = 'Land surface temperature', col = custom_colors);
plot(grid_2, add = T);
dev.off()  # Close the jpeg device

jpeg('ppa_2301_dis_2.jpg', width = 800, height = 600, quality = 100)  # Adjust width, height, and quality as needed
plot(dis_2, main = 'Distance to river', col = custom_colors);
plot(grid_2, add = T);
dev.off()  


#up2023_0924_10:31_e

#============================================================================
#up2023_0924_10:31_s

rc_ana_1 <- function(f_grid_num){
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
  f_plot <- ggplot(data = f_df, aes(x = dis, y = lst)) + geom_point() + labs(title = "Scatter Plot of distance vs lst (water temperature excluded)",
                   x = "distance", y = "LST");
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
  
  f_file_name1 <- paste("data_df_3_mean_", f_grid_num, ".jpg", sep = "");
  jpeg(f_file_name1, width = 800, height = 600, quality = 100)  # Adjust width, height, and quality as needed
  f_df <- data.frame(dis = f_breaks_2b, lst = f_data_df_3_mean)
  f_plot <- ggplot(data = f_df, aes(x = dis, y = lst)) + geom_point() + labs(title = "Scatter Plot of distance vs lst by intervals",
         x = "distance", y = "LST")
  print(f_plot)
  dev.off()  # Close the jpeg device
  
  f_file_name2 <- paste("data_df_3_mean_", ii, ".csv", sep = "");
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
  
  f_data_df_4 <- list();
  f_data_df_4[1] <- f_RCD_1;
  f_data_df_4[2] <- f_RCI_1;
  f_data_df_4[3] <- f_CRCI_1;
  return(f_data_df_4);
}

#up2023_0924_10:31_e

#=====================
#up2023_0924_10:31_s
grid_end <- 5; #to_be_set
thres_1 <- rep(45, times = grid_end); #to_be_set
breaks_2_end <- rep(500, times = grid_end); #to_be_set
breaks_2_by <- rep(20, times = grid_end); #to_be_set

data_df_1 <- list();
data_df_2 <- list();
data_df_3 <- list();
data_df_3_mean <- list();
data_df_4 <- list();
data_df_4v <- matrix(0, nrow = 3, ncol = grid_end);
breaks_2_num <- list();

for (ii in 1: grid_end){
  breaks_2_num[[ii]] <- round(breaks_2_end[ii] / breaks_2_by[ii]);
  data_df_1[[ii]] <- rc_ana_1(ii);
  data_df_2[[ii]] <- rc_ana_2(ii, thres_1[ii]);
  data_df_3[[ii]] <- rc_ana_3(ii, breaks_2_end[ii], breaks_2_by[ii]);
  data_df_3_mean[[ii]] <- rc_ana_3_mean(ii, breaks_2_end[ii], breaks_2_by[ii]);
  data_df_4[[ii]] <- rc_ana_4(ii, breaks_2_num[[ii]]);
  data_df_4v[1,ii] <- data_df_4[[ii]][1][[1]];
  data_df_4v[2,ii] <- data_df_4[[ii]][2][[1]];
  data_df_4v[3,ii] <- data_df_4[[ii]][3][[1]];
}

write.csv(data_df_4v, file = "data_df_4v.csv", row.names = FALSE)

#up2023_0924_10:31_e

