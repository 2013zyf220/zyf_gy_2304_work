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
order_1 <- 5; #to_be_set_key
season_1 <- 2; #to_be_set_key
lst_data <- 3; #to_be_set

buffer_loc1 <- 'shp/4/2301_cq_water7_buf2.csv' #to_be_set
buffer_loc2 <- 'shp/2/2301_cq_water_b12_buf1500_a09e_buf2.csv'
buffer_loc3 <- 'shp/6/arcgis/cq_water_buf1.csv'

grid_data_1 <- 'shp/4/2301_cq_water7_a8_buf800_3e.shp'
grid_data_2 <- 'shp/2/2301_cq_water_b12_buf1500_a09g.shp'
grid_data_3 <- 'shp/6/arcgis/cq_water_b06f.shp'


if(lst_data == 1){
  lst_1 <- raster(paste0('raster/ppa_2301_lst_s', order_1,'p.tif')); #to_be_set
  dis_1 <- raster('raster/a2301_disr2c.tif'); #land cover data #to_be_set
  grid_1a <- shapefile(grid_data_2); #to_be_set
  grid_2 <- st_read(grid_data_2); #to_be_set
  buffer_1 <- read.csv(buffer_loc2) #to_be_set
}else if(lst_data == 2){
  lst_1 <- raster(paste0('raster/ppa_2301_lstb_s', order_1,'p.tif')); #to_be_set
  dis_1 <- raster('raster/a2301_disr3p.tif'); #land cover data #to_be_set
  grid_1a <- shapefile(grid_data_2); #to_be_set
  grid_2 <- st_read(grid_data_2); #to_be_set
  buffer_1 <- read.csv(buffer_loc2) #to_be_set
}else if(lst_data == 3){
  lst_1 <- raster(paste0('raster/ppa_2301_lstb_s', order_1,'p.tif')); #to_be_set
  dis_1 <- raster('shp/6/arcgis/raster/cq_water_dis1p.tif'); #land cover data #to_be_set
  grid_1a <- shapefile(grid_data_3); #to_be_set
  grid_2 <- st_read(grid_data_3); #to_be_set
  buffer_1 <- read.csv(buffer_loc3) #to_be_set
}else{
  lst_1 <- raster(paste0('raster/ppa_2301_lstb_s', order_1,'p.tif')); #to_be_set
  dis_1 <- raster('raster/a2301_disr3c.tif'); #land cover data #to_be_set  
  grid_1a <- shapefile(grid_data_1); #to_be_set
  grid_2 <- st_read(grid_data_1); #to_be_set
  buffer_1 <- read.csv(buffer_loc1) #to_be_set
}

buffer_2 <- buffer_1$buffer
grid_1b <- grid_1a
lst_2 <- lst_1
dis_2 <- dis_1

custom_colors <- colorRampPalette(c('white', 'red'))(256)

#jpeg(paste0('shp/6/res1/ppa_2301_rce_lst_v1_', order_1,'.jpg'), width = 800, height = 600, quality = 100)  # Adjust width, height, and quality as needed
#plot(lst_2, main = 'Land surface temperature', col = custom_colors);
#plot(grid_1b, add = T);
#dev.off()  # Close the jpeg device

#jpeg(paste0('shp/6/res1/ppa_2301_rce_dis_v1_', order_1,'.jpg'), width = 800, height = 600, quality = 100)  # Adjust width, height, and quality as needed
#plot(dis_2, main = 'Distance to river', col = custom_colors);
#plot(grid_1b, add = T);
#dev.off()  

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
  
  f_disv_exa <- na.omit(getValues(f_dis_exa));
  f_lstv_exa <- na.omit(getValues(f_lst_exa));
  
  f_data_df_1 <- data.frame(dis = f_disv_exa, lst = f_lstv_exa);
  
  plot(f_data_df_1$dis, f_data_df_1$lst, main = 'Scatter Plot of distance vs LST', 
       xlab = 'distance', ylab = 'LST', pch = 19, col = 'black');
  
  return(f_data_df_1);
}

#up2023_0924_10:31_e

#==========================
#up2023_0924_10:31_s
rc_ana_2 <- function(f_grid_num, f_thres_1){
  f_breaks_1 <- c(-20, f_thres_1, 80); #to_be_set_key
  f_labels_1 <- list();
  f_labels_1[1] <- paste('-20 - ', f_thres_1);
  f_labels_1[2] <- paste(f_thres_1,' - 80');
  
  f_data_df_1 <- data_df_1[[f_grid_num]];
  f_data_df_1$bin <- cut(f_data_df_1$lst, breaks = f_breaks_1, labels = c(f_labels_1[1], f_labels_1[2]), include.lowest = TRUE);
  head(f_data_df_1);
  
  f_data_df_2a <- split(f_data_df_1, f_data_df_1$bin);
  f_data_df_2 <- f_data_df_2a[[2]];
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
  #cat('mean:',f_grid_num)
  f_breaks_2b <- seq(f_breaks_2_by, f_breaks_2_end, by = f_breaks_2_by)
  f_breaks_2_num <- round(f_breaks_2_end / f_breaks_2_by)
  f_data_df_3_mean <- rep(0, times = f_breaks_2_num)
  
  f_data_df_3 <- data_df_3[[f_grid_num]]
  
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
  
  #f_file_name1 <- paste('shp/6/res1/ppa_2301_rce_3_mean_', order_1, '_', f_grid_num, '.jpg', sep = '');
  #jpeg(f_file_name1, width = 800, height = 600, quality = 100)  # Adjust width, height, and quality as needed
  #f_df <- data.frame(dis = f_breaks_2b, lst = f_data_df_3_mean)
  #f_plot <- ggplot(data = f_df, aes(x = dis, y = lst)) + geom_point() + labs(title = 'Scatter Plot of distance vs lst by intervals', x = 'distance', y = 'LST')
  #print(f_plot)
  #dev.off()  # Close the jpeg device
  
  f_file_name2 <- paste('shp/6/res1/ppa_2301_rce_3_mean_', order_1, '_', f_grid_num, '.csv', sep = '');
  write.table(f_data_df_3_mean, file = f_file_name2, row.names = FALSE);
  
  return(f_data_df_3_mean)
}

#up2023_0924_10:31_e

#=====================
#up2023_0924_10:31_s
rc_ana_4 <- function(f_grid_num, f_breaks_2_num, f_season, f_adj){
  f_breaks_2_num_2 <- f_breaks_2_num - 1 - f_adj;  #to_be_set_key
  f_len1 <- length(data_df_3_mean[[f_grid_num]])
  f_adj2 <- 1 + f_adj
  f_data_df_3_mean <- data_df_3_mean[[f_grid_num]][f_adj2: f_len1];
  f_data_df_3_mean_s <- f_data_df_3_mean[1];
  f_mean1 <- list()
  f_max1 <- rep(0, f_breaks_2_num_2)
  f_min1 <- rep(0, f_breaks_2_num_2)
 
  for (f_ii in 1:f_breaks_2_num_2){
    f_a1 <- f_ii + 1
    f_a2 <- f_ii + 1 #to_be_set
    f_mean1[[f_ii]] <- f_data_df_3_mean[f_a1:f_a2]
    f_max1[f_ii] <- max(f_mean1[[f_ii]], na.rm = TRUE)
    f_min1[f_ii] <- min(f_mean1[[f_ii]], na.rm = TRUE)
    if(f_season == 2){
      if(is.na(f_data_df_3_mean[f_ii])) {
        cat('serial number of polygon ',f_grid_num,' is: ',  f_ii, '\n');
        f_RCD_1 <- f_ii;
        break;
      } else if(f_data_df_3_mean[f_ii] <= f_max1[f_ii]){
      } else{
        cat('serial number of polygon ',f_grid_num,' is: ',  f_ii, '\n');
        f_RCD_1 <- f_ii;
        break;
      }
    }else if(f_season == 4){
      if(is.na(f_data_df_3_mean[f_ii])) {
        cat('serial number of polygon ',f_grid_num,' is: ',  f_ii, '\n');
        f_RCD_1 <- f_ii;
        break;
      } else if(f_data_df_3_mean[f_ii] >= f_min1[f_ii]){
      } else{
        cat('serial number of polygon ',f_grid_num,' is: ',  f_ii, '\n');
        f_RCD_1 <- f_ii;
        break;
      }      
    }else{
      print('ERROR!')
    }
  }
  f_RCI_1 <- f_data_df_3_mean[f_RCD_1] - f_data_df_3_mean_s
  f_max <- f_data_df_3_mean[f_RCD_1]
  f_CRCI_1 <- f_max * f_RCD_1 - sum(f_data_df_3_mean[1:f_RCD_1], na.rm = TRUE)
  
  
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
  f_data_df_4[[1]] <- f_RCD_1;
  f_data_df_4[[2]] <- f_RCI_1;
  f_data_df_4[[3]] <- f_CRCI_1;
  f_data_df_4[[4]] <- f_RCD_2;
  f_data_df_4[[5]] <- f_RCI_2;
  f_data_df_4[[6]] <- f_CRCI_2;
  f_data_df_4[[7]] <- f_RCD_3;
  f_data_df_4[[8]] <- f_RCI_3;
  f_data_df_4[[9]] <- f_CRCI_3;
  f_data_df_4[[10]] <- grid_1b@polygons[[ii]]@labpt[1];
  f_data_df_4[[11]] <- grid_1b@polygons[[ii]]@labpt[2];
  f_data_df_4[[12]] <- f_data_df_3_mean
  f_data_df_4[[13]] <- f_mean1
  f_data_df_4[[14]] <- f_max1
  f_data_df_4[[15]] <- f_min1
  return(f_data_df_4);
}

#up2023_0924_10:31_e

#=====================
#up2023_0924_10:31_s
grid_end1 <- length(grid_1b); #to_be_set
thres_1 <- rep(2, times = grid_end1); #to_be_set
breaks_2_end <- buffer_2;
breaks_2_by <- rep(30, times = grid_end1); #to_be_set_key
adj_num <- 0 #to_be_set_key
grid_end2 <- length(grid_1b)
grid_2$XY_x2 <- rep(0, grid_end2)
grid_2$XY_y2 <- rep(0, grid_end2)
grid_2$XY_rcd <- rep(0, grid_end2)
grid_2$XY_rci <- rep(0, grid_end2)
grid_2$XY_crci <- rep(0, grid_end2)
grid_2$XY_rcd2 <- rep(0, grid_end2)
grid_2$XY_rci2 <- rep(0, grid_end2)
grid_2$XY_crci2 <- rep(0, grid_end2)
grid_2$XY_rcd3 <- rep(0, grid_end2)
grid_2$XY_rci3 <- rep(0, grid_end2)
grid_2$XY_crci3 <- rep(0, grid_end2)

data_df_1 <- list();
data_df_2 <- list();
data_df_3 <- list();
data_df_3_mean <- list();
data_df_4 <- list();
data_df_4v <- matrix(0, nrow = grid_end1, ncol = 12);
breaks_2_num <- list();
cir_s <- 64  #to_be_set
cir_e <- 67 #to_be_set
data_df_3_mean_all <- matrix(0, nrow = grid_end1, ncol = 50)
for (ii in cir_s: cir_e){
  breaks_2_num[[ii]] <- round(breaks_2_end[ii] / breaks_2_by[ii]);
  data_df_1[[ii]] <- rc_ana_1(ii);
  data_df_2[[ii]] <- rc_ana_2(ii, thres_1[ii]);
  data_df_3[[ii]] <- rc_ana_3(ii, breaks_2_end[ii], breaks_2_by[ii]);
  data_df_3_mean[[ii]] <- rc_ana_3_mean(ii, breaks_2_end[ii], breaks_2_by[ii]);
  data_df_3_mean_all[ii,1:length(data_df_3_mean[[ii]])] <- data_df_3_mean[[ii]]
  data_df_4[[ii]] <- rc_ana_4(ii, breaks_2_num[[ii]], season_1, adj_num);
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
  
  grid_2$XY_rcd[ii] <- data_df_4[[ii]][1][[1]];
  grid_2$XY_rci[ii] <- data_df_4[[ii]][2][[1]];
  grid_2$XY_crci[ii] <- data_df_4[[ii]][3][[1]];
  grid_2$XY_rcd2[ii] <- data_df_4[[ii]][4][[1]];
  grid_2$XY_rci2[ii] <- data_df_4[[ii]][5][[1]];
  grid_2$XY_crci2[ii] <- data_df_4[[ii]][6][[1]];
  grid_2$XY_rcd3[ii] <- data_df_4[[ii]][7][[1]];
  grid_2$XY_rci3[ii] <- data_df_4[[ii]][8][[1]];
  grid_2$XY_crci3[ii] <- data_df_4[[ii]][9][[1]];
  grid_2$XY_x2[ii] <- data_df_4[[ii]][10][[1]];
  grid_2$XY_y2[ii] <- data_df_4[[ii]][11][[1]];
}

data_df_4v_col <- c('XY_rcd', 'XY_rci', 'XY_crci', 'XY_rcd2', 'XY_rci2', 'XY_crci2', 'XY_rcd3', 'XY_rci3', 'XY_crci3', 'XY_x', 'XY_y', 'NUMBER')
colnames(data_df_4v) <- data_df_4v_col
#st_write(grid_2, paste0('shp/6/res1/ppa_2301_rce_s', order_1,'_adj',adj_num, '_data', lst_data,'.shp'))
write.csv(data_df_4v, file = paste0('shp/6/res1/ppa_2301_rce_s',order_1,'_adj', adj_num, '_data', lst_data,'.csv'), row.names = FALSE)
write.csv(data_df_3_mean_all, file = paste0('shp/6/res1/ppa_2301_rce_mean_s',order_1,'_adj',adj_num, '_data', lst_data, '.csv'), row.names = FALSE)
#up2023_0924_10:31_e

#====================================
#check

check_num <- 67 #to_be_set
check_s1_0 <- data_df_1[[check_num]]
check_s1_1 <- crop(dis_2, extent(grid_1b[check_num,]))
check_s1_2 <- mask(check_s1_1,grid_1b[check_num,])
#plot(check_s1_2)
check_s1_1v <- getValues(check_s1_1)
check_s1_1_nrow <- check_s1_1@nrows
check_s1_1_ncol <- check_s1_1@ncols
check_s1_1vm <- matrix(check_s1_1v, nrow = check_s1_1_nrow, ncol = check_s1_1_ncol, byrow = TRUE)
#write.table(check_s1_1vm,'shp/6/check1/check_s1_1vm.csv', row.names = FALSE, sep = ',')
check_s1_2v <- getValues(check_s1_2)
check_s1_2_nrow <- check_s1_2@nrows
check_s1_2_ncol <- check_s1_2@ncols
check_s1_2vm <- matrix(check_s1_2v, nrow = check_s1_2_nrow, ncol = check_s1_2_ncol,  byrow = TRUE)
#write.table(check_s1_2vm,'shp/6/check1/check_s1_2vm.csv', row.names = FALSE, sep = ',')
check_s1_1va <- na.omit(check_s1_1v)
#write.table(check_s1_1va,'shp/6/check1/check_s1_1va.csv', row.names = FALSE)
check_s1_2va <- na.omit(check_s1_2v)
#write.table(check_s1_2va,'shp/6/check1/check_s1_2va.csv', row.names = FALSE)

check_s1_3 <- crop(lst_2, extent(grid_1b[check_num,]))
check_s1_3v <- getValues(check_s1_3)
check_s1_3_nrow <- check_s1_3@nrows
check_s1_3_ncol <- check_s1_3@ncols
check_s1_3vm <- matrix(check_s1_3v, nrow = check_s1_3_nrow, ncol = check_s1_3_ncol, byrow = TRUE)
write.table(check_s1_3vm,'shp/6/check1/check_s1_3vm.csv', row.names = FALSE, sep = ',')
check_s1_3va <- na.omit(check_s1_3v)
write.table(check_s1_3va,'shp/6/check1/check_s1_3va.csv', row.names = FALSE)

check_s1_4 <- mask(check_s1_3,grid_1b[check_num,])
check_s1_4v <- getValues(check_s1_4)
check_s1_4_nrow <- check_s1_4@nrows
check_s1_4_ncol <- check_s1_4@ncols
check_s1_4vm <- matrix(check_s1_4v, nrow = check_s1_4_nrow, ncol = check_s1_4_ncol, byrow = TRUE)
#write.table(check_s1_4vm,'shp/6/check1/check_s1_4vm.csv', row.names = FALSE, sep = ',')
check_s1_4va <- na.omit(check_s1_4v)
#write.table(check_s1_4va,'shp/6/check1/check_s1_4va.csv', row.names = FALSE)

check_s2_thres <- 20
check_s2_break_1 <- c(-20, check_s2_thres, 80);
check_s2_label <- list()
check_s2_label[1] <- paste('-20 - ', check_s2_thres);
check_s2_label[2] <- paste(check_s2_thres,' - 80');
check_s2_input <- data_df_1[[check_num]]
check_s2_input$bin <- cut(check_s2_input$lst, breaks = check_s2_break_1, labels = c(check_s2_label[1], check_s2_label[2]), include.lowest = TRUE);
check_s2_input_2 <- split(check_s2_input, check_s2_input$bin)
check_s2_input_sp1 <- check_s2_input_2[[1]]
check_s2_input_sp2 <- check_s2_input_2[[2]]
check_s2_input_sp1b <- check_s2_input_2[['1']]
check_s2_input_sp1b <- check_s2_input_2[['2']]
is.null(check_s2_input_sp1b)

check_s3_break <- seq(0, 990, by = 30)
check_s3_input <- data_df_2[[check_num]]
check_s3_input$bin <- cut(check_s3_input$dis, breaks = check_s3_break, labels = FALSE, include.lowest = TRUE)
check_s3_output <- split(check_s3_input, check_s3_input$bin)

check_s3m_para1 <- 1
check_s3m_para2 <- toString(check_s3m_para1)
check_s3m_input <- data_df_3[[check_num]]
check_s3m_v1 <- check_s3m_input[[check_s3m_para1]]
check_s3m_v2 <- check_s3m_input[[check_s3m_para2]]
check_s3m_v2lst <- check_s3m_input[[check_s3m_para2]]$lst

check_s4_len <- length(data_df_3_mean[[check_num]])
check_s4_input <- data_df_3_mean[[check_num]]
check_s4_input2 <- check_s4_input[2:check_s4_len]