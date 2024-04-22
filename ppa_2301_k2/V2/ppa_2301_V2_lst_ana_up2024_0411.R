library(rgdal)
library(raster)
library(sf)
library(sp)
library(dplyr)
library(tidyr)
library(landscapemetrics)
library(ggplot2)
#rm(list = ls())

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/V2')

#============================================================================
#load and plot data

order_set <- 4; #to_be_set_key
season_set <- 2; #to_be_set

lst_1 <- raster(paste0('DATA_LST_1/ppa_2301_lstb_s', order_set,'p.tif'));
dis_1 <- raster('DATA_DIS_1/cq_water_dis1p.tif'); #to_be_set

grid_data_1 <- 'DATA_SHP_1/cq_water_b07dp.shp'  #to_be_set
grid_1 <- shapefile(grid_data_1); 
grid_2 <- st_read(grid_data_1); 
grid_len <- length(grid_1);

buf_data <- read.csv('DATA_SHP_1/cq_water_buf1.csv')
breaks_end <- buf_data$buffer

thres_1 <- rep(2, times = grid_len); #to_be_set
breaks_by <- 30; #to_be_set
adj_set <- 0 #to_be_set_key

custom_colors <- colorRampPalette(c('white', 'red'))(256)

jpeg(paste0('DATA_RCE_1/ppa_2301_rce_lst_v1_', order_set,'.jpg'), width = 800, height = 600, quality = 100)  # Adjust width, height, and quality as needed
plot(lst_1, main = 'Land surface temperature', col = custom_colors);
plot(grid_1, add = T);
dev.off()  # Close the jpeg device

jpeg(paste0('DATA_RCE_1/ppa_2301_rce_dis_v1_', order_set,'.jpg'), width = 800, height = 600, quality = 100)  # Adjust width, height, and quality as needed
plot(dis_1, main = 'Distance to river', col = custom_colors);
plot(grid_1, add = T);
dev.off()  

#============================================================================

rc_ana_1 <- function(f_grid_num){
  f_dis_exa_1 <- crop(dis_1, extent(grid_1[f_grid_num,]));
  f_lst_exa_1 <- crop(lst_1, extent(grid_1[f_grid_num,]));
  f_dis_exa_2 <- mask(f_dis_exa_1,grid_1[f_grid_num,]);
  f_lst_exa_2 <- mask(f_lst_exa_1,grid_1[f_grid_num,]);
  f_disv_exa <- na.omit(getValues(f_dis_exa_2));
  f_lstv_exa <- na.omit(getValues(f_lst_exa_2));
  f_data_df_1 <- data.frame(dis = f_disv_exa, lst = f_lstv_exa); #final output
  
  plot(f_dis_exa_2, col = custom_colors);
  plot(grid_1[f_grid_num,],add = T);
  plot(f_lst_exa_2, col = custom_colors);
  plot(grid_1[f_grid_num,],add = T);
  plot(f_data_df_1$dis, f_data_df_1$lst, main = 'Scatter Plot of distance vs LST', 
       xlab = 'distance', ylab = 'LST', pch = 19, col = 'black');
  
  f_res <- list(dis_exa_1 = f_dis_exa_1, lst_exa_1 = f_lst_exa_1, dis_exa_2 = f_dis_exa_2, lst_exa_2 = f_lst_exa_2, data_df_1 = f_data_df_1)
  return(f_res);
}

#==========================

rc_ana_2 <- function(f_grid_num, f_thres_1){
  f_breaks_1 <- c(-20, f_thres_1, 80); #to_be_set
  f_labels_1 <- list();
  f_labels_1[1] <- paste('-20 - ', f_thres_1);
  f_labels_1[2] <- paste(f_thres_1,' - 80');
  
  f_data_df_1 <- data_df_1[[f_grid_num]];
  f_data_df_1$bin <- cut(f_data_df_1$lst, breaks = f_breaks_1, labels = c(f_labels_1[1], f_labels_1[2]), include.lowest = TRUE);
  head(f_data_df_1);
  
  f_data_df_2a <- split(f_data_df_1, f_data_df_1$bin);
  f_data_df_2 <- f_data_df_2a[[2]]; #final output
  
  f_df <- data.frame(dis = f_data_df_2$dis, lst = f_data_df_2$lst)
  f_plot <- ggplot(data = f_df, aes(x = dis, y = lst)) + geom_point() +
                   labs(title = 'Scatter Plot of distance vs lst (water temperature excluded)', x = 'distance', y = 'LST');
  print(f_plot)
  
  f_res <- list(df_1 = f_data_df_1, data_df_2a = f_data_df_2a, data_df_2 = f_data_df_2)
  return(f_res);
}

#==============================

rc_ana_3 <- function(f_grid_num){
  f_breaks_end <- breaks_end[f_grid_num]
  f_breaks_2 <- seq(0, f_breaks_end, by = breaks_by)
  
  f_data_df_2 <- data_df_2[[f_grid_num]]
  f_data_df_2$bin <- cut(f_data_df_2$dis, breaks = f_breaks_2, labels = FALSE, include.lowest = TRUE)
  head(f_data_df_2)
  
  f_data_df_3 <- split(f_data_df_2, f_data_df_2$bin)
  f_res <- list(df_2 = f_data_df_2, data_df_3 = f_data_df_3)
  return(f_res);
}

#===========================

rc_ana_3_mean <- function(f_grid_num){
  #cat('rc_ana_3_mean:',f_grid_num)
  f_breaks_end <- breaks_end[f_grid_num]
  f_breaks_len <- round(f_breaks_end/breaks_by)
  f_data_mean <- rep(0, times = f_breaks_len)
  f_breaks_2b <- seq(breaks_by, f_breaks_end, by = breaks_by)
  
  f_data_df_3 <- data_df_3[[f_grid_num]]
  
  for (f_ii in 1: f_breaks_len) {
    f_ii_2 <- toString(f_ii);
    fc_data_1 <- f_data_df_3[[f_ii_2]];
    if (is.null(fc_data_1)) {
      f_data_mean[f_ii] <- NA
      print('The variable is NULL.');
    } else { 
      f_data_mean[f_ii] <- mean(f_data_df_3[[f_ii_2]]$lst)
    }
  }
  
  #f_file_name1 <- paste('DATA_ANA_1/MEAN/ppa_2301_RCE_MEAN_', order_set, '_', f_grid_num, '.jpg', sep = '');
  #jpeg(f_file_name1, width = 800, height = 600, quality = 100)  # Adjust width, height, and quality as needed
  #f_df <- data.frame(dis = f_breaks_2b, lst = f_data_mean)
  #f_plot <- ggplot(data = f_df, aes(x = dis, y = lst)) + geom_point() + labs(title = 'Scatter Plot of distance vs lst by intervals', x = 'distance', y = 'LST')
  #print(f_plot)
  #dev.off()  # Close the jpeg device
  
  f_file_name2 <- paste('DATA_ANA_1/MEAN/ppa_2301_RCE_MEAN_', order_set, '_', f_grid_num, '.csv', sep = '');
  write.table(f_data_mean, file = f_file_name2, row.names = FALSE);
  
  return(f_data_mean)
}

#=====================

rc_ana_4 <- function(f_grid_num, f_season, f_adj){
  f_start <- 1 + f_adj
  f_end <- length(data_df_3_mean[[f_grid_num]])
  f_len_1 <- f_end - 1 - f_adj;  #to_be_set_key
  
  f_data_mean <- data_df_3_mean[[f_grid_num]][f_start: f_end];
  f_data_mean_s <- f_data_mean[1];
  
  f_cmax1 <- rep(0, f_len_1)
  for (f_ii in 1: f_len_1){
    f_cmax1[f_ii] <- f_data_mean[f_ii + 1]  #to_be_set

    if(f_season == 2){
      if(is.na(f_data_mean[f_ii])) {
        cat('---out of limit!---')
        cat('length of polygon NO', f_grid_num,' is: ',  f_ii, '\n');
        f_RCD_1 <- -9999;
        break;
      } else if(f_data_mean[f_ii] <= f_cmax1[f_ii]){
      } else{
        cat('length of polygon NO.',f_grid_num,' is: ',  f_ii, '\n');
        f_RCD_1 <- f_ii;
        break;
      }
    }else{
      print('ERROR!')
    }
  }
  f_RCI_1 <- f_data_mean[f_RCD_1] - f_data_mean_s
  f_max <- f_data_mean[f_RCD_1]
  f_CRCI_1 <- (f_max * f_RCD_1 - sum(f_data_mean[1:f_RCD_1], na.rm = TRUE)) * 30
  
  if(f_RCI_1 <= 0){
    f_RCI_2 <- 0;
    f_RCD_2 <- 0;
    f_CRCI_2 <- 0;
  }else{
    f_RCI_2 <- log(f_RCI_1);
    f_RCD_2 <- log(f_RCD_1);
    f_CRCI_2 <- log(f_CRCI_1);
  }
  
  f_RCI_3 <- exp(f_RCI_1);
  f_RCD_3 <- exp(f_RCD_1);
  f_CRCI_3 <- exp(f_CRCI_1);
  
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
  f_data_df_4[[10]] <- grid_1@polygons[[ii]]@labpt[1];
  f_data_df_4[[11]] <- grid_1@polygons[[ii]]@labpt[2];
  f_data_df_4[[12]] <- f_data_mean
  f_data_df_4[[13]] <- f_cmax1
  return(f_data_df_4);
}

#=====================

grid_2$XY_x2 <- rep(0, grid_len)
grid_2$XY_y2 <- rep(0, grid_len)
grid_2$XY_rcd <- rep(0, grid_len)
grid_2$XY_rci <- rep(0, grid_len)
grid_2$XY_crci <- rep(0, grid_len)
grid_2$XY_rcd2 <- rep(0, grid_len)
grid_2$XY_rci2 <- rep(0, grid_len)
grid_2$XY_crci2 <- rep(0, grid_len)
grid_2$XY_rcd3 <- rep(0, grid_len)
grid_2$XY_rci3 <- rep(0, grid_len)
grid_2$XY_crci3 <- rep(0, grid_len)

data_df_1 <- list();
data_df_2 <- list();
data_df_3 <- list();
data_df_3_mean <- list();
data_df_4 <- list();
data_df_4v <- matrix(0, nrow = grid_len, ncol = 6); #to_be_set

cir_s <- 1  #to_be_set
cir_e <- grid_len #to_be_set
data_df_3_mean_all <- matrix(0, nrow = grid_len, ncol = 50)

for (ii in cir_s:cir_e){
  data_df_1[[ii]] <- rc_ana_1(ii)$data_df_1;
  data_df_2[[ii]] <- rc_ana_2(ii, thres_1[ii])$data_df_2;
  data_df_3[[ii]] <- rc_ana_3(ii)$data_df_3;
  data_df_3_mean[[ii]] <- rc_ana_3_mean(ii);
  data_df_3_mean_all[ii,1:length(data_df_3_mean[[ii]])] <- data_df_3_mean[[ii]]
  
  data_df_4[[ii]] <- rc_ana_4(ii, season_set, adj_set);
  data_df_4v[ii,1] <- data_df_4[[ii]][[1]] * 30;
  data_df_4v[ii,2] <- data_df_4[[ii]][[2]];
  data_df_4v[ii,3] <- data_df_4[[ii]][[3]];
  data_df_4v[ii,4] <- data_df_4[[ii]][[10]];
  data_df_4v[ii,5] <- data_df_4[[ii]][[11]];
  data_df_4v[ii,6] <- ii;
  
  grid_2$XY_rcd[ii] <- data_df_4[[ii]][[1]];
  grid_2$XY_rci[ii] <- data_df_4[[ii]][[2]];
  grid_2$XY_crci[ii] <- data_df_4[[ii]][[3]];
  grid_2$XY_rcd2[ii] <- data_df_4[[ii]][[4]];
  grid_2$XY_rci2[ii] <- data_df_4[[ii]][[5]];
  grid_2$XY_crci2[ii] <- data_df_4[[ii]][[6]];
  grid_2$XY_rcd3[ii] <- data_df_4[[ii]][[7]];
  grid_2$XY_rci3[ii] <- data_df_4[[ii]][[8]];
  grid_2$XY_crci3[ii] <- data_df_4[[ii]][[9]];
  grid_2$XY_x2[ii] <- data_df_4[[ii]][[10]];
  grid_2$XY_y2[ii] <- data_df_4[[ii]][[11]];
}

data_df_4v_col <- c('XY_rcd', 'XY_rci', 'XY_crci', 'XY_rce_x', 'XY_rce_y', 'NUMBER')
colnames(data_df_4v) <- data_df_4v_col
#st_write(grid_2, paste0('DATA_ANA_1/ppa_2301_RCE_s', order_set,'_adj',adj_set, '_data.shp'), overwrite = TRUE)
write.csv(data_df_4v, file = paste0('DATA_ANA_1/ppa_2301_RCE_s',order_set,'_adj', adj_set, '_data.csv'), row.names = FALSE)
write.csv(data_df_3_mean_all, file = paste0('DATA_ANA_1/ppa_2301_RCE_MEAN_s',order_set,'_adj',adj_set, '_data.csv'), row.names = FALSE)

#====================================
#check

#check_num <- 2 #to_be_set
#check_s1_0 <- data_df_1[[check_num]]
#check_s1_1 <- crop(dis_1, extent(grid_1[check_num,]))
#plot(check_s1_1)
#check_s1_1v <- getValues(check_s1_1)
#check_s1_1_nrow <- check_s1_1@nrows
#check_s1_1_ncol <- check_s1_1@ncols
#check_s1_1vm <- matrix(check_s1_1v, nrow = check_s1_1_nrow, ncol = check_s1_1_ncol, byrow = TRUE)
#write.table(check_s1_1vm,'DATA_RCE_1/CHECK/check_s1_1vm.csv', row.names = FALSE, sep = ',')

#check_s1_2 <- mask(check_s1_1,grid_1[check_num,])
#check_s1_2v <- getValues(check_s1_2)
#check_s1_2_nrow <- check_s1_2@nrows
#check_s1_2_ncol <- check_s1_2@ncols
#check_s1_2vm <- matrix(check_s1_2v, nrow = check_s1_2_nrow, ncol = check_s1_2_ncol,  byrow = TRUE)
#write.table(check_s1_2vm,'DATA_RCE_1/CHECK/check_s1_2vm.csv', row.names = FALSE, sep = ',')
#check_s1_1va <- na.omit(check_s1_1v)
#write.table(check_s1_1va,'DATA_RCE_1/CHECK/check_s1_1va.csv', row.names = FALSE)
#check_s1_2va <- na.omit(check_s1_2v)
#write.table(check_s1_2va,'DATA_RCE_1/CHECK/check_s1_2va.csv', row.names = FALSE)

#check_s1_3 <- crop(lst_1, extent(grid_1[check_num,]))
#check_s1_3v <- getValues(check_s1_3)
#check_s1_3_nrow <- check_s1_3@nrows
#check_s1_3_ncol <- check_s1_3@ncols
#check_s1_3vm <- matrix(check_s1_3v, nrow = check_s1_3_nrow, ncol = check_s1_3_ncol, byrow = TRUE)
#write.table(check_s1_3vm,'DATA_RCE_1/CHECK/check_s1_3vm.csv', row.names = FALSE, sep = ',')
#check_s1_3va <- na.omit(check_s1_3v)
#write.table(check_s1_3va,'DATA_RCE_1/CHECK/check_s1_3va.csv', row.names = FALSE)

#check_s1_4 <- mask(check_s1_3,grid_1[check_num,])
#check_s1_4v <- getValues(check_s1_4)
#check_s1_4_nrow <- check_s1_4@nrows
#check_s1_4_ncol <- check_s1_4@ncols
#check_s1_4vm <- matrix(check_s1_4v, nrow = check_s1_4_nrow, ncol = check_s1_4_ncol, byrow = TRUE)
#write.table(check_s1_4vm,'DATA_RCE_1/CHECK/check_s1_4vm.csv', row.names = FALSE, sep = ',')
#check_s1_4va <- na.omit(check_s1_4v)
#write.table(check_s1_4va,'DATA_RCE_1/CHECK/check_s1_4va.csv', row.names = FALSE)

#check_s2_thres <- 20
#check_s2_break_1 <- c(-20, check_s2_thres, 80);
#check_s2_label <- list()
#check_s2_label[1] <- paste('-20 - ', check_s2_thres);
#check_s2_label[2] <- paste(check_s2_thres,' - 80');
#check_s2_input <- data_df_1[[check_num]]
#check_s2_input$bin <- cut(check_s2_input$lst, breaks = check_s2_break_1, labels = c(check_s2_label[1], check_s2_label[2]), include.lowest = TRUE);
#check_s2_input_2 <- split(check_s2_input, check_s2_input$bin)
#check_s2_input_sp1 <- check_s2_input_2[[1]]
#check_s2_input_sp2 <- check_s2_input_2[[2]]
#check_s2_input_sp1b <- check_s2_input_2[['1']]
#check_s2_input_sp1b <- check_s2_input_2[['2']]
#is.null(check_s2_input_sp1b)

#check_s3_break <- seq(0, 990, by = 30)
#check_s3_input <- data_df_2[[check_num]]
#check_s3_input$bin <- cut(check_s3_input$dis, breaks = check_s3_break, labels = FALSE, include.lowest = TRUE)
#check_s3_output <- split(check_s3_input, check_s3_input$bin)

#check_s3m_para1 <- 1
#check_s3m_para2 <- toString(check_s3m_para1)
#check_s3m_input <- data_df_3[[check_num]]
#check_s3m_v1 <- check_s3m_input[[check_s3m_para1]]
#check_s3m_v2 <- check_s3m_input[[check_s3m_para2]]
#check_s3m_v2lst <- check_s3m_input[[check_s3m_para2]]$lst

#check_s4_len <- length(data_df_3_mean[[check_num]])
#check_s4_input <- data_df_3_mean[[check_num]]
#check_s4_input2 <- check_s4_input[2:check_s4_len]