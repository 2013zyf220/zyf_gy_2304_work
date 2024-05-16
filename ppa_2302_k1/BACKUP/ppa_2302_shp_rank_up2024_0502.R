library(sf)

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2302_k2/ARCGIS/LINES')

#===============================

loc_1 <- c('streets_4.shp', 'streets_5.shp')

loc_set <- loc_1  #to_be_set_key
input <- loc_set[1]
output <- loc_set[2] 
  
data_1 <- st_read(input)
#print(head(data_1)) 
data_2 <- st_zm(data_1, drop = TRUE)
data_2_order <- order(data_2$NUM_ADJ)
data_3 <- data_2[data_2_order, ] # 根据'number'属性对数据进行排序
rownames(data_3) <- as.character(1:nrow(data_3))
#print(head(data_3))
  
st_write(data_3, output)