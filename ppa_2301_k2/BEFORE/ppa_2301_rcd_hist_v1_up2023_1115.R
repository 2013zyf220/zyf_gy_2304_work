
year_1 <- 2021; #to_be_set
setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/shp/outputs2')
data_1 <- read.csv(paste0('2301_river_6_', year_1,'.csv')) 
hist(data_1$rx_rcd) 