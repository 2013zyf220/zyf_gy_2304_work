library(sf)
setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/shp')

grid_paths_1 <- list();
grid_paths_1[[1]] <- '1/2301_cq_water_b10_buf500_a19.shp';
grid_paths_1[[2]] <- '2/2301_cq_water_b11_buf200_a05.shp';
grid_paths_1[[3]] <- '2/2301_cq_water_b11_buf400_a04.shp';
grid_paths_1[[4]] <- '2/2301_cq_water_b11_buf500_a01.shp';
grid_paths_1[[5]] <- '2/2301_cq_water_b11_buf600_a08.shp';
grid_paths_1[[6]] <- '2/2301_cq_water_b11_buf800_a09.shp';
grid_paths_1[[7]] <- '2/2301_cq_water_b11_buf1000_a16.shp';
grid_paths_1[[8]] <- '2/2301_cq_water_b12_buf1500_a07.shp';
grid_paths_1[[9]] <- '2301_cq_water_10_a3.shp'
grid_paths_1[[10]] <- '2301_cq_water_07_mid3.shp'
grid_paths_1[[11]] <- '4/2301_cq_water7_a7_buf800_5.shp';
grid_paths_1[[12]] <- '4/2301_cq_water7_a7_buf800_7b.shp';
grid_paths_1[[13]] <- '2/2301_cq_water_b12_buf1500_a09f.shp';
grid_paths_1[[14]] <- '5/res1/2301_cq_water_b13_bufx050s4c.shp';
grid_paths_1[[15]] <- '6/arcgis/cq_water_b06d.shp';
grid_paths_1[[16]] <- '6/arcgis/cq_water_c01s4_a03.shp';
grid_paths_1[[17]] <- '6/arcgis/cq_water_c01s5_a03.shp';
grid_paths_1[[18]] <- '6/arcgis/cq_water_rw3.shp';

grid_paths_2 <- list();
grid_paths_2[[1]] <- '1/2301_cq_water_b12_buft500_a01.shp';
grid_paths_2[[2]] <- '2/2301_cq_water_b12_buf200_a01.shp';
grid_paths_2[[3]] <- '2/2301_cq_water_b12_buf400_a01.shp';
grid_paths_2[[4]] <- '2/2301_cq_water_b12_buf500_a01.shp';
grid_paths_2[[5]] <- '2/2301_cq_water_b12_buf600_a01.shp';
grid_paths_2[[6]] <- '2/2301_cq_water_b12_buf800_a01.shp';
grid_paths_2[[7]] <- '2/2301_cq_water_b12_buf1000_a01.shp';
grid_paths_2[[8]] <- '2/2301_cq_water_b12_buf1500_a08.shp';
grid_paths_2[[9]] <- '2301_cq_water_10_a4.shp'
grid_paths_2[[10]] <- '2301_cq_water_07_mid4.shp'
grid_paths_2[[11]] <- '4/2301_cq_water7_a7_buf800_6.shp';
grid_paths_2[[12]] <- '4/2301_cq_water7_a7_buf800_7c.shp';
grid_paths_2[[13]] <- '2/2301_cq_water_b12_buf1500_a09g.shp';
grid_paths_2[[14]] <- '5/res1/2301_cq_water_b13_bufx050s4d.shp';
grid_paths_2[[15]] <- '6/arcgis/cq_water_b06e.shp';
grid_paths_2[[16]] <- '6/arcgis/cq_water_c01s4_a04.shp';
grid_paths_2[[17]] <- '6/arcgis/cq_water_c01s5_a04.shp';
grid_paths_2[[18]] <- '6/arcgis/cq_water_rw4.shp';

sort_f <- function(f_path){
  f_grid_1 <- st_read(f_path);
  f_grid_2 <- f_grid_1[order(f_grid_1$NUMBER), ]
  return(f_grid_2)
}

grid_res <- list();

orders <- 18 #to_be_set
for(ii in orders){
  grid_res[[ii]] <- sort_f(grid_paths_1[[ii]])
  st_write(grid_res[[ii]], grid_paths_2[[ii]])
}
