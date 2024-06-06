library(sf)
setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/shp')
grid_a1 <- st_read('2301_cq_water_10_a3.shp')
grid_a2 <- st_read('2301_cq_water_07_mid3.shp')

grid_b1 <- grid_a1[order(grid_a1$NUMBER), ]
grid_b2 <- grid_a2[order(grid_a2$NUMBER), ]

st_write(grid_b1, '2301_cq_water_10_a4.shp')
st_write(grid_b2, '2301_cq_water_07_mid4.shp')