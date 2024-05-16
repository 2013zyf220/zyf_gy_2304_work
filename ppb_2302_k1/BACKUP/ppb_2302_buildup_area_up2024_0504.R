
library(raster)
library(sp)
library(sf)
library(rgdal)
library(ggplot2)

#==================================================================================
#up2024_0511_09:34_s

cor_crs <- CRS('+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs')  #to_be_set

prop_num <- 14  #to_be_set
year_s <- 1998  #to_be_set(start year)
year_e <- 2022  #to_be_set(end year)
year_gap <- 3  #to_be_set
years <- seq(year_s, year_e, year_gap)
len_years <- length(years)

#special province names: jining,niaoning,guangzhou,shaanxi(xian),sichuang
prov_1 <- c('chongqing') #to_be_set(province)
shp_data_1 <- c('chongqing_1') #to_be_set(shp data)
len_shp_data_1 <- length(shp_data_1)

#up2024_0511_09:34_e
#==================================================================================
#up2024_0511_09:48_s

bu_area_sub <- function(f_year, f_prov, f_shp_data, f_type){
  f_luse_1 <- raster(paste0('D:/zyf_gn/zyf_gn_2301_data/landuse_k2/CLCD_v01_',f_year,'_albert_province/CLCD_v01_',f_year,'_albert_', f_prov,'.tif')); 
  f_shp_1 <- st_read(paste0('E:/zyf_gn/zyf_gn_2301_data/ppb_2302_k1/admini/', f_shp_data, '.shp'))[ , prop_num]
  f_shp_2 <- st_transform(f_shp_1, cor_crs);
  f_shp_size <- nrow(f_shp_2)
  f_shp_p <- f_shp_2$ENG_NAME   #to_be_set
  
  f_luse_2 <- list()
  f_luse_3 <- list()
  f_luse_3b <- list()
  f_luse_4 <- list()
  f_luse_5 <- rep(0, f_shp_size)
  
  for(ii in 1: f_shp_size){
    f_luse_2[[ii]] <- crop(f_luse_1, extent(f_shp_2[ii, ]))
    f_luse_3[[ii]] <- mask(f_luse_2[[ii]], f_shp_2[ii, ]);
    
    f_luse_4[[ii]] <- na.omit(getValues(f_luse_3[[ii]]));
    f_luse_5[ii] <- sum(f_luse_4[[ii]] == f_type) * 900/(1000 * 1000) #to_be_set
    
    f_luse_3b[[ii]] <- projectRaster(f_luse_3[[ii]], crs = '+init=epsg:4326')
    writeRaster(f_luse_3b[[ii]], filename = paste0('E:/zyf_gn/zyf_gn_2301_data/ppb_2302_k1/outputs/ppb_2302_luse_', f_shp_data, '_', f_year, '_sub', f_shp_p[ii], '.tif'), overwrite=TRUE)
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

#up2024_0511_09:48_e
#==================================================================================
#up2024_0511_10:05_s

area_all_df <- list()
for(ii in 1: len_shp_data_1){
  c_shp_loc <- paste0('E:/zyf_gn/zyf_gn_2301_data/ppb_2302_k1/admini/', shp_data_1[ii], '.shp') 
  c_shp_size <- nrow(st_read(c_shp_loc)[ , prop_num]) #to_be_set
  c_shp_name <- st_read(c_shp_loc)[ , prop_num]$ENG_NAME   #to_be_set
  c_area_res <- matrix(0, nrow = len_years, ncol = c_shp_size, byrow = TRUE)
  
  c_area_sum_0 <- rep(0, len_years)
  for (jj in 1: len_years){
    cat('year:', years[jj], '\n')
    c_area_res[jj, ] <- bu_area_sub(years[jj], prov_1[ii], shp_data_1[ii], 8)$luse_5  #to_be_set
    c_area_sum_0[jj] <-  sum(c_area_res[jj, ])
  } 
  
  c_area_all <- cbind(c_area_res, c_area_sum_0)
  area_all_df[[ii]] <- as.data.frame(c_area_all)
  
  c_shp_data_2 <- paste0(shp_data_1[ii], '_sum')
  colnames(area_all_df[[ii]]) <- c(c_shp_name, c_shp_data_2)
  rownames(area_all_df[[ii]]) <- years
  write.csv(area_all_df[[ii]], paste0('E:/zyf_gn/zyf_gn_2301_data/ppb_2302_k1/outputs/area_all_', shp_data_1[ii], '_', year_s,'_',year_e, '.csv'))
}

#up2024_0511_10:05_e
#==================================================================================
