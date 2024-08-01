library(sf)

setwd('D:/zyf_gn/zyf_gn_2301_data/ppa_2401_k1/ROI')

#===========================================

coord_1 <- list()
coord_1[[1]] <- c(103.95,104.15,30.6,30.75) #to_be_set
coord_1[[2]] <- c(103.95,104.15,30.45,30.6) #to_be_set
coord_1[[3]] <- c(103.98,104,30.65,30.67) #to_be_set

poly_f <- function(f_coord_set){
  f_coord_2 <- coord_1[[f_coord_set]]
  
  f_coord_3 <- matrix(c(
    f_coord_2[1], f_coord_2[3],
    f_coord_2[1], f_coord_2[4],
    f_coord_2[2], f_coord_2[4],
    f_coord_2[2], f_coord_2[3],
    f_coord_2[1], f_coord_2[3]  
  ), ncol = 2, byrow = TRUE) # 终点与起点相同，以关闭多边形
  
  # 创建多边形对象，指定坐标系为WGS 84 (EPSG:4326)
  f_poly_1 <- st_polygon(list(f_coord_3))
  f_poly_sf1 <- st_sfc(f_poly_1, crs = 4326)  # 创建sfc对象，指定投影
  f_poly_sf2 <- st_sf(geometry = f_poly_sf1, name = paste0('ROI_', f_coord_set))
  st_write(f_poly_sf2, paste0('ROI_', f_coord_set, '.shp'))
  
  f_res <- list()
  f_res[['poly_1']] <- f_poly_1
  f_res[['poly_sf1']] <- f_poly_sf1
  f_res[['poly_sf2']] <- f_poly_sf2
  return(f_res)
}

#===========================================

coords <- c(1,2,3) #to_be_set

poly_res <- list()
for(ii in coords){
  poly_res[[ii]] <- poly_f(ii) 
}
