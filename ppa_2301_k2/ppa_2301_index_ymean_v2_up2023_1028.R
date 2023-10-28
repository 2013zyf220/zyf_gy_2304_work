library(rgdal)
library(raster)
library(sf)
library(sp)
library(dplyr)
library(tidyr)
library(landscapemetrics)
library(ggplot2)
#rm(list = ls())

#get the average among multiple years
#=================================================================

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/shp');
grid_exa <- st_read(paste0('outputs3/2301_river_7_2022.shp')); #to_be_set

grid_ysta <- grid_exa['NUMBER'];
grid_len <- nrow(grid_exa);

attri_list <- list('area','XG_DIS_CEN'); #to_be_set_key
attri_len <- length(attri_list);

ymean_1 <- function(f_year_s, f_year_e, f_attri){
  f_year_len <- f_year_e - f_year_s + 1;
  f_index_1 <- matrix(0, nrow = f_year_len, ncol = grid_len);
  
  ii <- 1;
  for(c_year in f_year_s: f_year_e){
    f_grid_1 <- st_read(paste0('outputs3/2301_river_7_', c_year, '.shp'));
    
    if (f_attri %in% names(f_grid_1)) {
      f_index_2 <- f_grid_1[[f_attri]] # Extract the values of the specified attributes
    } else {
      stop(paste('Attribute ', f_attri, ' not found in the shapefile.'))
    };
    
    for(jj in 1: grid_len){
      f_index_1[ii,jj] <- f_index_2[jj];
    }
    ii <- ii + 1;
  }
  return(f_index_1)
}

#==================================================================

year_s <- 2019; #to_be_set_key
year_e <- 2022; #to_be_set_key
years <- year_s: year_e

ymean_res_1 <- list();
for(ii in 1: attri_len){
  c_attri <- attri_list[[ii]];
  ymean_res_1[[c_attri]] <- ymean_1(year_s, year_e, c_attri); #to_be_set_key
}

#==================================================================

ymean_2 <- function(f_attri){
  f_ymean_res_2 <- colMeans(ymean_res_1[[f_attri]]);
  return(f_ymean_res_2)
}

yrate_2 <- function(f_attri){
  f_r2 <- rep(0, grid_len)
  for(ii in 1: grid_len){
    f_1 <- ymean_res_1[[f_attri]][,ii];
    f_r1 <- lm(f_1 ~ years)
    f_r2[ii] <- f_r1$coefficients['f_1']
    return(f_r2)
  }
}
#==================================================================

ysta_res_1 <- list();
for(ii in 1: attri_len){
  c_attri <- attri_list[[ii]];
  c_attri_a <- paste0(c_attri,'_mean')
  c_attri_b <- paste0(c_attri,'_rate')
  ysta_res_1[[c_attri_a]] <- ymean_2(c_attri)
  ysta_res_1[[c_attri_b]] <- yrate_2(c_attri)
  grid_ysta[[c_attri_a]] <- ysta_res_1[[c_attri_a]];
  grid_ysta[[c_attri_b]] <- ysta_res_1[[c_attri_b]];
} 

st_write(grid_ysta, paste0('outputs3/2301_river_7_ysta.shp'));


