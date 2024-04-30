library(sf)

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/V2')

#======================================

year_set <- 2022
adj_set <- 0; #to_be_set
data_rw <- read.csv(paste0('DATA_ANA_1/ppa_2301_river_2.csv'))
data_rotate <- read.csv(paste0('DATA_ANA_1/ppa_2301_rotate.csv'))

#======================================

merged_data_f <- function(f_order){
  f_data_rce <- read.csv(paste0('DATA_ANA_1/ppa_2301_RCE_s',f_order,'_adj', adj_set, '_data.csv')) #to_be_set
  f_data_lsi <- read.csv(paste0('DATA_ANA_1/ppa_2301_lsi_', year_set, '_buf', f_order, '.csv')) 
  f_data_bh <- read.csv(paste0('DATA_ANA_1/ppa_2301_bh_bs', f_order, '.csv'))
  f_data_terrain <- read.csv(paste0('DATA_ANA_1/ppa_2301_terrain_', f_order, '.csv'))
  
  f_merged_data_1 <- merge(f_data_rce, f_data_lsi, by = "NUMBER", all = TRUE)
  f_merged_data_2 <- merge(f_merged_data_1, f_data_bh, by = "NUMBER", all = TRUE)
  f_merged_data_3 <- merge(f_merged_data_2, f_data_terrain, by = "NUMBER", all = TRUE)
  f_merged_data_4 <- merge(f_merged_data_3, data_rw, by = "NUMBER", all = TRUE)
  f_merged_data_5 <- merge(f_merged_data_4, data_rotate, by = "NUMBER", all = TRUE)
  f_merged_data_6 <- round(f_merged_data_5, 3)
  write.csv(f_merged_data_6, file = paste0('DATA_ANA_1/ppa_2301_ana_s', f_order, '.csv'), row.names = FALSE)
  
  f_res_list <- list()
  f_res_list[['data_rce']] <- f_data_rce
  f_res_list[['data_lsi']] <- f_data_lsi
  f_res_list[['data_bh']] <- f_data_bh
  f_res_list[['data_terrain']] <- f_data_terrain
  return(f_res_list)
}

#==========================================
order_set <- '5'  #to_be_set_key
merged_data_res <- merged_data_f(order_set)  #to_be_set_key


