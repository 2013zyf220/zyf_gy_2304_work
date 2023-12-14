library(sf)
setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/shp/rotate')

grid_paths_1 <- list();
grid_paths_1[[1]] <- '2301_cq_water_rotate_3e.shp';


grid_paths_2 <- list();
grid_paths_2[[1]] <- '2301_cq_water_rotate_3f.shp';


sort_f <- function(f_path){
  f_grid_1 <- st_read(f_path);
  f_grid_2 <- f_grid_1[order(f_grid_1$NUMBER), ]
  return(f_grid_2)
}

grid_res <- list();

orders <- 1 #to_be_set
for(ii in orders){
  grid_res[[ii]] <- sort_f(grid_paths_1[[ii]])
  st_write(grid_res[[ii]], grid_paths_2[[ii]])
}

