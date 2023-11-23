library(sf)

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/shp/3')

data_terrain_var <- c('NUMBER', 'XT_CEN_X', 'XT_CEN_Y', 'XT_DEM_MEA', 'XT_DEM_STD', 'XT_SLOPE_M', 'XT_ASPECT_', 'XT_DIS_CEN');  #to_be_set
data_ndvi_var <- c('NUMBER', 'XN_CEN_X2', 'XN_CEN_Y2', 'XN_NDVI_ME');  #to_be_set

merged_data_f <- function(f_order, f_year, f_buffer){
 
  f_data_lsi <- read.csv(paste0('ppa_2301_lsi_', f_year, '_buf', f_buffer, '.csv')) 
  f_data_lsi$NUMBER <- f_data_lsi$rx_grid_id
  f_data_bh <- read.csv(paste0('ppa_2301_bh_1_buf', f_buffer, '.csv'))
  
  f_data_terrain_1 <- st_read(paste0('ppa_2301_terrain_1_buf', f_buffer, '.shp'))
  f_data_terrain_2 <- as.data.frame(f_data_terrain_1)
  f_data_terrain_3 <- f_data_terrain_2[, data_terrain_var]
  f_data_ndvi_1 <- st_read(paste0('ppa_2301_ndvi_s', f_order, '_buf', f_buffer, '.shp'))
  f_data_ndvi_2 <- as.data.frame(f_data_ndvi_1)
  f_data_ndvi_3 <- f_data_ndvi_2[, data_ndvi_var]
  
  f_data_lines <- read.csv(paste0('ppa_2301_lines_1.csv')) 
  f_data_rce <- read.csv(paste0('ppa_2301_rce_s', f_order, '.csv'))
  
  f_merged_data_1 <- merge(f_data_lines, f_data_lsi, by = "NUMBER", all = TRUE)
  f_merged_data_2 <- merge(f_merged_data_1, f_data_bh, by = "NUMBER", all = TRUE)
  f_merged_data_3 <- merge(f_merged_data_2, f_data_terrain_3, by = "NUMBER", all = TRUE)
  f_merged_data_4 <- merge(f_merged_data_3, f_data_ndvi_3, by = "NUMBER", all = TRUE)
  f_merged_data_5 <- merge(f_merged_data_4, f_data_rce, by = "NUMBER", all = TRUE)
  
  write.csv(f_merged_data_5, file = paste0('ppa_2301_ana_s', f_order, '_buf', f_buffer, '.csv'), row.names = FALSE)
  
  f_res_list <- list()
  f_res_list[['data_rce']] <- f_data_rce
}

#==========================================
order_1 <- 1  #to_be_set_key
buffer_1 <- 1000  #to_be_set_key

if(order_1 == 1){
  year_1 <- 2020
}else if(order_1 == 2|order_1 == 3){
  year_1 <- 2021
}else if(order_1 == 4){
  year_1 <- 2022
}else{
  print('ERROR')
}
merged_data_res <- merged_data_f(1,2021,1000)  #to_be_set_key
