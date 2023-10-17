library(raster)
library(sp)
library(rgdal)
library(ggplot2)

#==================================================================================
setwd('E:/zyf_gn/zyf_gn_2301_data/ppb_2302_k2/outputs');
shp_data_all <- list('chongqing1', 'chengdu1') #to_be_set
shp_data_len = length(shp_data_all);

year_s <- 1995  #to_be_set
year_e <- 2022  #to_be_set
year_len <- year_e - year_s + 1;

list_data <- list();
area_1 <- matrix(0, nrow = year_len, ncol = shp_data_len)

for (ii in 1: shp_data_len){
  f_shp <- shp_data_all[[ii]];
  list_data[[ii]] <- read.csv(paste0('bu_area_', f_shp, '.csv'));
  area_1[,ii] <- list_data[[ii]][ ,2]
}

years <- list_data[[1]][,1]
rownames(area_1) <- years
colnames(area_1) <- shp_data_all
#==================================================================================

year_1 <- rep(year_s: year_e, times = shp_data_len);
shp_1 <- rep(c(shp_data_all[[1]], shp_data_all[[2]]), each = year_len); #to_be_set
value_1 <- c(area_1[ ,1], area_1[ ,2]);  #to_be_set
df <- data.frame(year_1 = year_1, value_1 = value_1, shp_1 = shp_1);

plot_1 <- ggplot(df, mapping = aes(x = year_1, y = value_1, group = shp_1, color = shp_1)) + geom_line() + 
  scale_color_manual(values = c('green',  'orange')) +
  labs(x = 'Year', y = expression('area (km'^2*')')) + theme_minimal()

ggsave(paste0('ppb_2302_area_sum.jpg'), plot_1, width = 6, height = 4, units = 'in');
write.csv(area_1, file = paste0('bu_area_sum.csv'), row.names = TRUE)
