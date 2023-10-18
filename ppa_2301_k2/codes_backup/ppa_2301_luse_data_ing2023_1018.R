library(raster)
library(terra)

#==================================================
setwd('E:/zyf_gn/zyf_gn_2301_data');

clip_area_f <- function(f_1, f_lon_min, f_lon_max, f_lat_min, f_lat_max){
  if(f_1 == 1){
    f_2 = extent(f_lon_min, f_lon_max, f_lat_min, f_lat_max);
  } else {
    f_2 = raster(paste0('ppa_2301_k2/shp/outputs/ppa_2301_cq1_area.shp'));
  }
  return(f_2);
}


#luse_data <- function(f_year, f_lon_min, f_lon_max, f_lat_min, f_lat_max){
#  cat('Year:', f_year);
#  f_luse_1 <- raster(paste0('landuse_k2/CLCD_v01_', f_year, '_albert_province/CLCD_v01_', f_year, '_albert_chongqing.tif'));
#  f_luse_2 <- projectRaster(f_luse_1, crs = '+init=epsg:4326')
#  f_clip_extent <- extent(f_lon_min, f_lon_max, f_lat_min, f_lat_max);
#  f_luse_3 <- crop(f_luse_2, f_clip_extent);
  
#  plot(f_luse_1);
#  plot(f_luse_2);
#  plot(f_luse_3);
  
#  writeRaster(f_luse_3, filename = paste0('ppa_2301_k2/raster/ppa_2301_cq_lusec_', f_year, '.tif'))  #输出为单波段多个TIFF
#  return(f_luse_3);
#}

#year_s <- 2019; #to_be_set
#year_e <- 2019; #to_be_set
#output_1 <- list();

#for (ii in year_s: year_e){
#  output_1 <- luse_data(ii, 106.4, 106.7, 29.4, 29.75); #to_be_set
#}


clip_area_1 <- clip_area_f(1, 106.4, 106.7, 29.4, 29.75)


