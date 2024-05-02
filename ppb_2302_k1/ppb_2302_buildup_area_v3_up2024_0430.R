
library(raster)
library(sp)
library(sf)
library(rgdal)
library(ggplot2)

#==================================================================================
#up2024_0430_17:20_s

cor_crs <- CRS('+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs')  #to_be_set

year_s <- 1995  #to_be_set
year_e <- 2022  #to_be_set
year_gap <- 3  #to_be_set
years <- seq(year_s, year_e, year_gap)
len_years <- length(years)

prov_1 <- 'beijing' #to_be_set
shp_data_1 <- 'beijing1' #to_be_set
shp_data_2 <- paste0(shp_data_1, '_sum')

setwd('D:/zyf_gn/zyf_gn_2301_data')

#up2024_0430_17:20_e
#==================================================================================
#up2024_0430_17:25_s

bu_area_sub <- function(f_year, f_prov, f_shp_data, f_type){
  f_luse_1 <- raster(paste0('landuse_k2/CLCD_v01_',f_year,'_albert_province/CLCD_v01_',f_year,'_albert_', f_prov,'.tif')); 
  f_shp_1 <- st_read(paste0('ppb_2302_k1/admini/', f_shp_data, '.shp'))[ ,14]  #to_be_set
  f_shp_2 <- st_transform(f_shp_1, cor_crs);
  f_shp_size <- nrow(f_shp_2)
  f_shp_p <- f_shp_2$ENG_NAME   #to_be_set
  
  f_luse_3 <- list()
  #f_luse_3b <- list()
  f_luse_4 <- list()
  f_luse_5 <- rep(0, f_shp_size)
  
  for(ii in 1: f_shp_size){
    fc_luse_2b <- crop(f_luse_1, extent(f_shp_2[ii, ]))
    f_luse_3[[ii]] <- mask(fc_luse_2b, f_shp_2[ii, ]);
    
    f_luse_4[[ii]] <- na.omit(getValues(f_luse_3[[ii]]));
    f_luse_5[ii] <- sum(f_luse_4[[ii]] == f_type) * 900/(1000 * 1000) #to_be_set
    
    #f_luse_3b[[ii]] <- projectRaster(f_luse_3[[ii]], crs = '+init=epsg:4326')
    #writeRaster(f_luse_3b[[ii]], filename = paste0('ppb_2302_k1/outputs/ppb_2302_luse_', f_shp_data, '_', f_year, '_sub', f_shp_p[ii], '.tif'), overwrite=TRUE)
  }
  f_luse_5b <- cbind(f_shp_p, f_luse_5)
  f_luse_6 <- as.data.frame(f_luse_5b)
  colnames(f_luse_6) <- c('district', 'area')

  f_res <- list()
  f_res[['luse_5']] <- f_luse_5
  f_res[['luse_6']] <- f_luse_6
  f_res[['shp_size']] <- f_shp_size
  
  return(f_res)
}  

#up2024_0430_17:25_e
#==================================================================================
#up2024_0430_17:27_s

shp_loc <- paste0('ppb_2302_k1/admini/', shp_data_1, '.shp')  #to_be_set
shp_size <- nrow(st_read(shp_loc)[ ,14])
shp_name <- st_read(shp_loc)[ ,14]$ENG_NAME   #to_be_set

area_res <- matrix(0, nrow = len_years, ncol = shp_size, byrow = TRUE)
ii <- 0
for (c_year in years){
  ii <- ii + 1
  cat('year:', c_year)
  area_res[ii, ] <- bu_area_sub(c_year, prov_1, shp_data_1, 8)$luse_5  #to_be_set
} 

area_res2 <- area_res
colnames(area_res2) <- shp_name
rownames(area_res2) <- years

#up2024_0430_17:27_e
#=================================
#up2024_0430_17:28_s

area_sum_0 <- rep(0, len_years)
for (ii in 1: len_years){
  area_sum_0[ii] <-  sum(area_res[ii, ])
}

area_all <- cbind(area_res, area_sum_0)
area_all_df <- as.data.frame(area_all)
colnames(area_all_df) <- c(shp_name, shp_data_2)
rownames(area_all_df) <- years

write.csv(area_all_df, paste0('ppb_2302_k1/outputs/area_all_', shp_data_1, '_', year_s,'_',year_e, '.csv'))

#up2024_0430_17:28_e
