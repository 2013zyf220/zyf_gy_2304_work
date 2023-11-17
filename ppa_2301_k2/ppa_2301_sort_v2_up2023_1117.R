library(sf)
setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/shp/1')
grid_a1 <- st_read('2301_cq_water_b10_buf500_a19.shp')
grid_b1 <- grid_a1[order(grid_a1$NUMBER), ]

st_write(grid_b1, '2301_cq_water_b10_buf500_a20.shp')
