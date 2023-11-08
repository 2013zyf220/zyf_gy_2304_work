#up2023_1020 13:19

library(raster)
library(sp)
library(rgdal)
library(ggplot2)

#==================================================================================

cor_crs <- CRS('+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs')  #to_be_set

year_s <- 2022  #to_be_set
year_e <- 2022  #to_be_set
prov_1 <- 'yunnan' #to_be_set
shp_data_1 <- 'qujing_a1' #to_be_set

year_len <- year_e - year_s + 1;
years <- seq(year_s, year_e);
setwd('E:/zyf_gn/zyf_gn_2301_data');

#==================================================================================

bu_area <- function(f_year, f_prov, f_shp_data){
  f_luse_1 <- raster(paste0('landuse_k2/CLCD_v01_',f_year,'_albert_province/CLCD_v01_',f_year,'_albert_', f_prov,'.tif')); 
  f_admini_1 <- shapefile(paste0('ppb_2302_k2/admini/', f_shp_data, '.shp'));
  f_admini_2 <- spTransform(f_admini_1, cor_crs);
  
  plot(f_luse_1);
  plot(f_admini_2, add = T);
  
  f_luse_2 <- crop(f_luse_1, extent(f_admini_2));
  f_luse_3 <- mask(f_luse_2, f_admini_2);
  #f_luse_3b <- projectRaster(f_luse_3, crs = '+init=epsg:4326')
  #plot(f_luse_3b)
  
  f_luse_4 <- na.omit(getValues(f_luse_3));
  f_luse_5 <- sum(f_luse_4 == 8) * 900/(1000 * 1000); #to_be_set
  #writeRaster(f_luse_3b, filename = paste0('ppb_2302_k2/outputs/ppb_2302_luse_1_', f_shp_data, '_', f_year,'.tif'))
  
  return(f_luse_5)
}  

#==================================================================================

area_res <- rep(0, year_len)

for (ii in 1: year_len){
  c_year <- ii + year_s - 1;
  cat('Year_', c_year);
  area_res[ii] <- bu_area(c_year, prov_1, shp_data_1);
} 

#==================================================================================
data_1 <- data.frame(YEAR = years, DATA = area_res)
plot_1 <- ggplot(data_1, aes(YEAR, DATA)) +
  geom_line() + ggtitle(paste0('city: ', shp_data_1, '|  year:', year_s, '-', year_e)) +
  scale_x_continuous(
    breaks = seq(year_s, year_e, by = 1),  # Set custom breaks
    limits = c(year_s, year_e),  # Set limits for the x-axis
    expand = c(0.05, 0)  # Adjust axis expansion
  ) + labs(x = 'year', y = expression('Area (km'^2*')')) +  theme_minimal();

ggsave(paste0('ppb_2302_k2/outputs/ppb_2302_area_',shp_data_1,'_',year_s,'_',year_e,'.jpg'), 
       plot_1, width = 6, height = 4, units = 'in');

write.csv(data_1, file = paste0('ppb_2302_k2/outputs/bu_area_', shp_data_1,'.csv'), row.names = FALSE)
