library(sf)

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/shp')

year_f <- function(f_order){
  if(f_order == 1){
    f_year <- 2020
  }else if(f_order == 2|f_order == 3){
    f_year <- 2021
  }else if(f_order == 4|f_order == 5){
    f_year <- 2022
  }else if(f_order == 6){
    f_year <- 2019
  }else{
    print('ERROR')
  }
}

data_rw <- read.csv(paste0('rw/2301_cq_water_rw_7.csv'))
data_rotate <- read.csv(paste0('rotate/2301_cq_water_rotate_5b.csv'))
data_lines <- read.csv(paste0('3/ppa_2301_lines_1.csv')) 

lst_data <- 3; #to_be_set
adj_num <- 0; #to_be_set

merged_data_f <- function(f_order, f_buffer){
  f_year <- year_f(f_order)
  f_data_lsi <- read.csv(paste0('6/res2/ppa_2301_lsi_', f_year, '_buf', f_buffer, '.csv')) 
  f_data_bh <- read.csv(paste0('6/res2/ppa_2301_bh_1_buf', f_buffer, '.csv'))

  f_data_terrain_1 <- read.csv(paste0('6/res2/ppa_2301_data_terrain_', f_buffer, '.csv'))
  f_data_terrain_2 <- read.csv(paste0('6/res2/ppa_2301_data_slope_', f_buffer, '.csv'))
  f_data_terrain_3 <- read.csv(paste0('6/res2/ppa_2301_data_aspect_', f_buffer, '.csv'))
  f_data_ndvi <- read.csv(paste0('6/res2/ppa_2301_data_ndvi_', f_buffer, '.csv'))
  f_data_rce <- read.csv(paste0('6/res1/ppa_2301_rce_s',f_order,'_adj', adj_num, '_data', lst_data,'.csv')) #to_be_set
  
  f_merged_data_1 <- merge(data_lines, f_data_lsi, by = "NUMBER", all = TRUE)
  f_merged_data_2 <- merge(f_merged_data_1, f_data_bh, by = "NUMBER", all = TRUE)
  f_merged_data_3 <- merge(f_merged_data_2, f_data_terrain_1, by = "NUMBER", all = TRUE)
  f_merged_data_4 <- merge(f_merged_data_3, f_data_terrain_2, by = "NUMBER", all = TRUE)
  f_merged_data_5 <- merge(f_merged_data_4, f_data_terrain_3, by = "NUMBER", all = TRUE)
  f_merged_data_6 <- merge(f_merged_data_5, f_data_ndvi, by = "NUMBER", all = TRUE)
  f_merged_data_7 <- merge(f_merged_data_6, f_data_rce, by = "NUMBER", all = TRUE)
  f_merged_data_8 <- merge(f_merged_data_7, data_rw, by = "NUMBER", all = TRUE)
  f_merged_data_9 <- merge(f_merged_data_8, data_rotate, by = "NUMBER", all = TRUE)
  write.csv(f_merged_data_9, file = paste0('6/res2/ppa_2301_ana_s', f_order, '_buf', f_buffer, '.csv'), row.names = FALSE)
  
  f_res_list <- list()
  f_res_list[['data_rce']] <- f_data_rce
  f_res_list[['data_lines']] <- data_lines
  f_res_list[['data_lsi']] <- f_data_lsi
}

#==========================================
orders <- c(5)  #to_be_set_key
buffers <- c('d01s5')  #to_be_set_key


merged_data_res <- list()
for(c_order in orders){
  merged_data_res[[c_order]] <- list()
  for(c_buf in buffers){
    merged_data_res[[c_order]][[c_buf]] <- merged_data_f(c_order, c_buf)  #to_be_set_key
  }
}

