library(sf)

year <- 2021 #to_be_set
setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/shp');
shp_1a <- st_read(paste0('2301_river_5_',year,'.shp'))
shp_2a <- st_read("2301_lines_1.shp")
shp_1b <- st_drop_geometry(shp_1a)
shp_2b <- st_drop_geometry(shp_2a)

merged_shp <- merge(shp_1b, shp_2b, by = "NUMBER")
write.csv(merged_shp, paste0('2301_river_6_',year,'.csv'),row.names = FALSE)

