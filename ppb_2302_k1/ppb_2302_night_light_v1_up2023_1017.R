library(raster)
library(sp)
library(rgdal)
library(ggplot2)

#==================================================================================

cor_crs <- CRS('+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs')  #to_be_set

year_s <- 2010  #to_be_set
year_e <- 2019  #to_be_set
shp_data_1 <- 'chengdu1' #to_be_set

year_len <- year_e - year_s + 1;
years <- seq(year_s, year_e);

setwd('E:/zyf_gn/zyf_gn_2301_data');
#shp_exa <- raster(paste0('night_light_k2/DMSP-like2020.tif')); 
#proj4string(shp_exa)
#==================================================================================

night_light <- function(f_year, f_shp_data){
  if (f_year > 2012) {
    f_data_1 <- raster(paste0('night_light_k2/DMSP-like',f_year,'.tif')); 
  } else {
    f_data_1 <- raster(paste0('night_light_k2/DMSP',f_year,'.tif')); 
  }
  
  f_admini_1 <- shapefile(paste0('ppb_2302_k2/admini/', f_shp_data, '.shp'));
  f_admini_2 <- spTransform(f_admini_1, cor_crs);
  
  plot(f_data_1);
  plot(f_admini_2, add = T);
  
  f_data_2 <- crop(f_data_1, extent(f_admini_2));
  f_data_3 <- mask(f_data_2, f_admini_2);
  f_data_3b <- projectRaster(f_data_3, crs = '+init=epsg:4326')
  plot(f_data_3b)
  
  f_data_4 = na.omit(getValues(f_data_3));
  f_data_5 = sum(f_data_4); 
  
  return(f_data_5)
}  

#==================================================================================

nl_res = rep(0, year_len)

for (ii in 1: year_len){
  c_year <- ii + year_s - 1;
  cat('Year_', c_year);
  nl_res[ii] <- night_light(c_year, shp_data_1);
} 

#==================================================================================
plot_data_1 <- data.frame(xx = years, yy = nl_res)
plot_1 <- ggplot(plot_data_1, aes(xx, yy)) +
  geom_line() +
  scale_x_continuous(
    breaks = seq(year_s, year_e, by = 1),  # Set custom breaks
    limits = c(year_s, year_e),  # Set limits for the x-axis
    expand = c(0.05, 0)  # Adjust axis expansion
  ) + labs(x = 'year', y = 'night light') +  theme_minimal();

ggsave(paste0('ppb_2302_k2/outputs/ppb_2302_nl_',shp_data_1,'_', year_s,'_', year_e,'.jpg'), 
       plot_1, width = 6, height = 4, units = 'in');

