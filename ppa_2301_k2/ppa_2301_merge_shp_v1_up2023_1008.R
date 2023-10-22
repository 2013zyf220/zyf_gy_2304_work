library(sf)

merge_res <- function(f_year){
  setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/shp/outputs2');
  f_shp_1a <- st_read(paste0('2301_river_5_', f_year,'.shp'))
  
  setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/shp/outputs');
  f_shp_2a <- st_read("2301_lines_1.shp")
  
  setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/shp/outputs2');
  f_shp_1b <- st_drop_geometry(f_shp_1a)
  f_shp_2b <- st_drop_geometry(f_shp_2a)
  
  f_merged_shp <- merge(f_shp_1b, f_shp_2b, by = "NUMBER")
  write.csv(f_merged_shp, paste0('2301_river_6_',f_year,'.csv'), row.names = FALSE)
}

#=====================================

year_s <- 2019; #to_be_set
year_e <- 2022; #to_be_set
for(c_year in year_s: year_e){
  merge_res(c_year)
}