library(sf)

merge_f <- function(f_year){
  setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/shp/outputs2');
  f_shp_1a <- st_read(paste0('2301_river_5b_', f_year,'.shp'))
  
  setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/shp/outputs');
  f_shp_2a <- st_read('2301_lines_1.shp')
  
  setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/shp/outputs2');
  f_shp_1b <- st_drop_geometry(f_shp_1a)
  f_shp_2b <- st_drop_geometry(f_shp_2a)
  
  f_merged_shp <- merge(f_shp_1b, f_shp_2b, by = 'NUMBER')
  write.csv(f_merged_shp, paste0('2301_river_6_',f_year,'.csv'), row.names = FALSE)
  
  f_shp_1a$XG_ANGLE_1 <- f_shp_2a$XG_ANGLE_1
  f_shp_1a$XG_ANGLE_2 <- f_shp_2a$XG_ANGLE_2
  f_shp_1a$NUMBER2 <- f_shp_2a$NUMBER
  st_write(f_shp_1a, paste0('2301_river_6_', c_year,'.shp'))
  
  res_list <- list();
  res_list[['shp_1a']] <- f_shp_1a
  res_list[['shp_2a']] <- f_shp_2a
  res_list[['shp_1b']] <- f_shp_1b
  res_list[['shp_2b']] <- f_shp_2b
  res_list[['merged_shp']] <- f_merged_shp
  return(res_list)
}

#=====================================

year_s <- 2021; #to_be_set
year_e <- 2021; #to_be_set

merge_res <- list();
ii <- 1;
for(c_year in year_s: year_e){
  merge_res[[ii]] <- merge_f(c_year)
  ii <- ii + 1
}