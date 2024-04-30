
library(raster)
library(sp)
library(sf)
library(rgdal)
library(ggplot2)

setwd('D:/zyf_gn/zyf_gn_2301_data')
#==================================================================================
#up2024_0430_17:31_s

cor_crs <- CRS('+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs')  #to_be_set

year_s <- 2010  #to_be_set
year_e <- 2016  #to_be_set
prov_1 <- 'sichuang' #to_be_set
shp_data_1 <- 'chengdu1' #to_be_set
type_set <- 8  #to_be_set

#up2024_0430_17:31_e
#==================================================================================
#up2024_0430_17:33_s

map_trans_f <- function(f_year_s, f_year_e, f_prov, f_shp_data, f_type){
  f_luse_s1 <- raster(paste0('landuse_k2/CLCD_v01_',f_year_s,'_albert_province/CLCD_v01_',f_year_s,'_albert_', f_prov,'.tif')); 
  f_luse_e1 <- raster(paste0('landuse_k2/CLCD_v01_',f_year_e,'_albert_province/CLCD_v01_',f_year_e,'_albert_', f_prov,'.tif')); 
  f_shp_1 <- shapefile(paste0('ppb_2302_k1/admini/', f_shp_data, '.shp'));
  f_shp_2 <- spTransform(f_shp_1, cor_crs);
  
  f_luse_s2 <- crop(f_luse_s1, extent(f_shp_2));
  f_luse_s3 <- mask(f_luse_s2, f_shp_2);
  f_luse_e2 <- crop(f_luse_e1, extent(f_shp_2));
  f_luse_e3 <- mask(f_luse_e2, f_shp_2);  
  
  plot(f_luse_s3);
  plot(f_shp_2, add = T);
  #plot(f_luse_e3);
  #plot(f_shp_2, add = T);  
  
  f_map_trans_1 <- (f_luse_s3 != f_type) & (f_luse_e3 == f_type)
  return(f_map_trans_1)
}

#up2024_0430_17:33_e
#==================================================================================
#up2024_0430_17:33_s

map_trans_res <- map_trans_f(year_s, year_e, prov_1, shp_data_1, type_set)   #to_be_set
plot(map_trans_res)
output_name <- paste0('ppb_2302_k1/outputs/map_trans_', type_set, '_', year_s, '_', year_e, '_', shp_data_1, '.tif') 
writeRaster(map_trans_res, filename = output_name, format = 'GTiff', overwrite = TRUE)

#up2024_0430_17:33_e
#==================================================================================