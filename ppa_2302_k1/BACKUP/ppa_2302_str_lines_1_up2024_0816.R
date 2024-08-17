
library(sf)
library(dplyr)



buf_size <- 100 #to_be_set

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2302_k2/ARCGIS/BH')
data_1 <- st_read(paste0('str_lines3_b5_buf', buf_size, '.shp'))
data_b1 <- st_read('str_lines3_b0_c1.shp')

# 按 NUM 属性进行排序
data_2 <- data_1 %>%
  arrange(NUMBER)

data_b2 <- data_b1 %>%
  arrange(NUMBER)

st_write(data_2, paste0('str_lines3_b6_buf', buf_size, '.shp'))
st_write(data_b2, paste0('str_lines3_b0_c2.shp'))