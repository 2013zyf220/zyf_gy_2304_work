
library(sf)
setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/V2/DATA_SHP_1')

#===============================
loc_1 <- c("cq_water_b02c.shp", "cq_water_b02d.shp")
loc_2 <- c("cq_water_b07c.shp", "cq_water_b07d.shp")

loc_set <- loc_2  #to_be_set_key
input <- loc_set[1]
output <- loc_set[2] 
  
data_1 <- st_read(input)
#print(head(data_1)) # 检查数据，确保"number"属性存在
  
data_1_order <- order(data_1$NUMBER)
data_2 <- data_1[data_1_order, ] # 根据"number"属性对数据进行排序
rownames(data_2) <- as.character(1:nrow(data_2))
#print(head(data_2))
  
st_write(data_2, output)