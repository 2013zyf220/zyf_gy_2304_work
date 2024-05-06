
library(raster)
library(sp)
library(sf)
library(rgdal)
library(ggplot2)

#==================================================================================
#up2024_0430_17:31_s

cor_crs <- CRS('+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs')  #to_be_set

years <- seq(1998, 2022, 6)  #to_be_set
len_years <- length(years)
len_years_2 <- len_years - 1

years_start <- rep(0, len_years_2)
years_end <- rep(0, len_years_2)
for(ii in 1: len_years_2){
  years_start[ii] <- years[ii]
  years_end[ii] <- years[ii + 1]
}

prov_1 <- c('chongqing') #to_be_set
shp_data_1 <- c('chongqing_2') #to_be_set
len_shp_data_1 <- length(shp_data_1)
type_set <- 8  #to_be_set

#up2024_0430_17:31_e
#==================================================================================
#up2024_0430_17:33_s

map_trans_f <- function(f_year_s, f_year_e, f_prov, f_shp_data, f_type){
  f_luse_s1 <- raster(paste0('D:/zyf_gn/zyf_gn_2301_data/landuse_k2/CLCD_v01_',f_year_s,'_albert_province/CLCD_v01_',f_year_s,'_albert_', f_prov,'.tif')); 
  f_luse_e1 <- raster(paste0('D:/zyf_gn/zyf_gn_2301_data/landuse_k2/CLCD_v01_',f_year_e,'_albert_province/CLCD_v01_',f_year_e,'_albert_', f_prov,'.tif')); 
  f_shp_1 <- shapefile(paste0('E:/zyf_gn/zyf_gn_2301_data/ppb_2302_k1/admini/', f_shp_data, '.shp'));
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
  f_res <- list()
  f_res[['luse_s']] <- f_luse_s3
  f_res[['luse_e']] <- f_luse_e3
  f_res[['map_trans_1']] <- f_map_trans_1
  return(f_res)
}

#up2024_0430_17:33_e
#==================================================================================
#up2024_0504_14:23_s
map_trans_res_0 <- list()
map_trans_res_1 <- list()
output_names <- list()
output_name_start <- list()
for(ii in 1: len_shp_data_1){
  map_trans_res_0[[ii]] <- list()
  map_trans_res_1[[ii]] <- list()
  output_names[[ii]] <- list()
  for(jj in 1: len_years_2){
    cat('data', jj, '\n')
    map_trans_res_0[[ii]][[jj]] <- map_trans_f(years_start[jj], years_end[jj], prov_1[ii], shp_data_1[ii], type_set)   #to_be_set
    map_trans_res_1[[ii]][[jj]] <- map_trans_res_0[[ii]][[jj]][['map_trans_1']]
    plot(map_trans_res_1[[ii]][[jj]])
    output_names[[ii]][[jj]] <- paste0('E:/zyf_gn/zyf_gn_2301_data/ppb_2302_k1/outputs/map_trans_', type_set, '_', years_start[jj], '_', years_end[jj], '_', shp_data_1[ii], '.tif') 
    writeRaster(map_trans_res_1[[ii]][[jj]], filename = output_names[[ii]][[jj]], format = 'GTiff', overwrite = TRUE)
  }
  
  output_name_start[[ii]] <- paste0('E:/zyf_gn/zyf_gn_2301_data/ppb_2302_k1/outputs/map_start_', type_set, '_', years_start[1],  '_', shp_data_1[ii], '.tif') 
  writeRaster(map_trans_res_0[[ii]][[1]][['luse_s']], filename = output_name_start[[ii]], format = 'GTiff', overwrite = TRUE)
} 

#up2024_0504_14:23_e
#==================================================================================