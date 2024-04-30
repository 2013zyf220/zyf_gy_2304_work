#up2023_1020 13:19

library(raster)
library(sp)
library(sf)
library(rgdal)
library(ggplot2)

#==================================================================================
#up2024_0430_12:02_s

cor_crs <- CRS('+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs')  #to_be_set

years <- seq(2016, 2022, 3) #to_be_set
len_years <- length(years)
prov_1 <- 'chongqing' #to_be_set
shp_data_1 <- 'chongqing1' #to_be_set


setwd('D:/zyf_gn/zyf_gn_2301_data')

#up2024_0430_12:02_e
#==================================================================================

bu_area <- function(f_year, f_prov, f_shp_data){
  f_luse_1 <- raster(paste0('landuse_k2/CLCD_v01_',f_year,'_albert_province/CLCD_v01_',f_year,'_albert_', f_prov,'.tif')); 
  f_admini_1 <- st_read(paste0('ppb_2302_k1/admini/', f_shp_data, '.shp'))[ ,14]
  f_admini_2 <- st_transform(f_admini_1, cor_crs);
  
  plot(f_luse_1);
  plot(f_admini_2, add = T);
  
  f_luse_2 <- crop(f_luse_1, extent(f_admini_2));
  f_luse_3 <- mask(f_luse_2, f_admini_2);
  #f_luse_3b <- projectRaster(f_luse_3, crs = '+init=epsg:4326')
  #plot(f_luse_3b)
  
  f_luse_4 <- na.omit(getValues(f_luse_3));
  f_luse_5 <- sum(f_luse_4 == 8) * 900/(1000 * 1000) #to_be_set
  #writeRaster(f_luse_3b, filename = paste0('ppb_2302_k1/outputs/ppb_2302_luse_1_', f_shp_data, '_', f_year,'.tif'))
  
  return(f_luse_5)
}  

#==================================================================================

area_res <- rep(0, len_years)

ii <- 0
for (c_year in years){
  ii <- ii + 1
  cat('year:', c_year)
  area_res[ii] <- bu_area(c_year, prov_1, shp_data_1);
} 

#=================================


