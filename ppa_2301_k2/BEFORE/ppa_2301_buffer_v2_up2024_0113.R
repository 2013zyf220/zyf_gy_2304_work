library(rgdal)
library(raster)
library(sf)
library(sp)
library(dplyr)
library(tidyr)
library(landscapemetrics)
library(ggplot2)
library(horizon)
library(rmapshaper)
#rm(list = ls())

setwd("E:/zyf_gn/zyf_gn_2301_data")

c_buf <- 'x050s4' #to_be_set
shp_1 <- st_read(paste0('ppa_2301_k2/shp/5/res1/2301_cq_water_b13_buf', c_buf, '.shp')) #to_be_set
buf_1 <- 100 #to_be_set
# Create a buffer of 500 meters around each polygon
shp_buf_1 <- st_buffer(shp_1, dist = buf_1) 

# Simplify the buffered polygons (optional, but can help in visualization)
shp_buf_2 <- ms_simplify(shp_buf_1)

# Plot the original and buffered polygons
plot(shp_1, main = "Original Polygons", col = "lightblue")
plot(shp_buf_2, col = "pink", main = "Buffered Polygons")
st_write(shp_buf_1, paste0('ppa_2301_k2/shp/5/res1/2301_cq_water_b13_buf', c_buf, '_buf', buf_1,'.shp'))
