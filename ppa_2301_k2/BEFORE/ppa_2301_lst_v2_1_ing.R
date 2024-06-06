library(terra)
library(data.table)
library(rgdal)
library(raster)
library(sf)
library(sp)
library(dplyr)
library(tidyr)
library(landscapemetrics)
library(ggplot2)
#rm(list = ls())
#=========================================================

grid_1 = raster('E:/zyf_gy/zyf_gy_temp/LC09_L2SP_128039_20230908_20230913_02_T1_ST_B10_rep.TIF'); #to_be_set
#plot(grid_1)

new_extent <- extent(106.3,107, 29.3, 30)
grid_2 <- crop(grid_1, new_extent)
grid_3 <- grid_2 * 0.00341802 +149 - 273.15
plot(grid_3) 
color_palette <- colorRampPalette(c("blue", "red"))
plot(grid_3, col = color_palette(100))
