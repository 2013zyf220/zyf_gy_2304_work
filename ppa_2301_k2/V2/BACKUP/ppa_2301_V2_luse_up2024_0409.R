library(raster)
library(terra)
library(rgdal)

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/V2');

#==================================================

clip_area_f <- function(f_1, f_lon_min, f_lon_max, f_lat_min, f_lat_max){
  if(f_1 == 1){
    f_2 = extent(f_lon_min, f_lon_max, f_lat_min, f_lat_max);
  } else {
    f_2 = shapefile(paste0('DATA_SHP_1/ppa_2301_cq_area_1pb.shp'));
  }
  return(f_2);
}

luse_data <- function(f_year, f_clip_extent){
  cat('Year:', f_year);
  f_luse_1 <- raster(paste0('DATA_LUSE_1/ppa_2301_', f_year, '_luse_1.tif'));
  f_luse_2 <- crop(f_luse_1, f_clip_extent);
  
  plot(f_luse_1);
  plot(f_luse_2);
  
  writeRaster(f_luse_2, filename = paste0('DATA_LUSE_1/ppa_2301_', f_year, '_luse_2.tif'))  #输出为单波段多个TIFF
  return(f_luse_2);
}

#==================================================

year_set <- 2022 #to_be_set

clip_area_1 <- clip_area_f(2, 106.4, 106.7, 29.4, 29.75) #to_be_set
output_1 <- luse_data(year_set, clip_area_1) #to_be_set



