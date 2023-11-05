library(rgdal)
library(raster)
library(sf)
library(sp)
library(dplyr)
library(tidyr)
library(landscapemetrics)
library(ggplot2)
#rm(list = ls())

#note: this code is to merge angle variable from 2301_lines_1.shp  with other variables from 2301_river_5_XXX.shp
#==========================================================================
#up2023_1025 14:33

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/shp');
grid_1 <- shapefile('outputs/2301_lines_1.shp');
grid_1b <- st_read('outputs/2301_lines_1.shp');
grid_len <- length(grid_1);

merge2_shp <- function(f_year){
  f_grid_2 <- st_read(paste0('outputs2/2301_river_5b_',f_year,'.shp'));
  f_grid_2$rxl_angle <- grid_1b$XG_ANGLE_2;
  
  f_line_0 <- list();
  f_line_x <- rep(0, grid_len);
  f_line_y <- rep(0, grid_len);
  
  for(ii in 1: grid_len){
    f_line_0[[ii]] <- grid_1@lines[[ii]];
    f_line_x[ii] <- mean(f_line_0[[ii]]@Lines[[1]]@coords[ ,1]);
    f_line_y[ii] <- mean(f_line_0[[ii]]@Lines[[1]]@coords[ ,2]);
  }
  
  f_grid_2$rxl_x <- f_line_x;
  f_grid_2$rxl_y <- f_line_y;
  
  st_write(f_grid_2, paste0('outputs3/2301_river_7_',f_year,'.shp'));
  return(f_grid_2);
}

#==========================================================================
#up2023_1025 14:33

year_s <- 2021; #to_be_set
year_e <- 2021; #to_be_set

merge2_shp_res <- list();

ii <- 1;
for(c_year in year_s: year_e){
  merge2_shp_res[[ii]] <- merge2_shp(c_year);
  ii <- ii + 1;
}
