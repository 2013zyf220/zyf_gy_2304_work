
library(sf)
library(dplyr)

TEST_NUM <- 4 #to_be_set_key

buf_size <- 100 #to_be_set

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2302_k2/ARCGIS/BH')
#data_1 <- st_read(paste0('str_lines3_b5_buf', buf_size, '.shp'))
#data_b1 <- st_read('str_lines3_b0_c1.shp')
data_c1 <- st_read(paste0('TEST' , TEST_NUM, '/str_lines4_buf', buf_size, '_test', TEST_NUM, '.shp'))
#data_d1 <- st_read(paste0('TEST' , TEST_NUM, '/str_lines3_c0_test', TEST_NUM, '.shp'))
# 按 NUM 属性进行排序
#data_2 <- data_1 %>%
  #arrange(NUMBER)

#data_b2 <- data_b1 %>%
  #arrange(NUMBER)

data_c2 <- data_c1 %>%
  arrange(NUMBER)

#data_d2 <- data_d1 %>%
#  arrange(NUMBER)
#st_write(data_2, paste0('str_lines3_b6_buf', buf_size, '.shp'))
#st_write(data_b2, paste0('str_lines3_b0_c2.shp'))
st_write(data_c2, paste0('TEST' , TEST_NUM, '/str_lines5_buf', buf_size, '_test', TEST_NUM, '.shp'))
#st_write(data_d2, paste0('TEST' , TEST_NUM, '/str_lines3_c1_test', TEST_NUM, '.shp'))