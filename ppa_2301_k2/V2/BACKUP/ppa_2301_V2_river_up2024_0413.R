library(sf)

angle_f <- function(f_1){
  f_2 <- min(f_1,360 - f_1)
  return(f_2)
}

#=================

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/V2')

data_1 <- read.csv(paste0('DATA_ANA_1/ppa_2301_river_1.csv'))
data_2 <- data_1[c("NUMBER", "length")]

data_3a <- data_1$BEARING
data_3b <- sapply(data_3a, angle_f)
data_3 <- cbind(data_2, data_3b)
colnames(data_3)[3] <- "BEARING"
data_4 <- round(data_3, 3)
write.csv(data_4, file = paste0('DATA_ANA_1/ppa_2301_river_2.csv'), row.names = FALSE)