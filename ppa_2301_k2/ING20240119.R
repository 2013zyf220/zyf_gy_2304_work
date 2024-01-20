library(MASS)
library(gbm)
library(randomForest)
library(skimr)
library(DataExplorer)
library(tidyverse)
library(caret)
library(pROC)
library(ggplot2)
library(pdp)

setwd("E:/zyf_gn/zyf_gn_2301_data/ppa_2301_k2/shp/6")

order_1 <- 5 #to_be_set
buffer_1 <- 'd01s5'  #to_be_set
vari_1 <- 'XY_rci' #to_be_set
vari_len <- 13 #to_be_set
data_1 <- read.csv(paste0('res3/2301_brt_impor_1_', order_1, '_buf', buffer_1, vari_1, '.csv'))
data_2 <- data.frame(variables = 1:vari_len, values = data_1[,2])

vari_names <- data_1[,1]

data_bar <- ggplot(data_2, aes(x = variables, y = values)) + 
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Importance", x = "variables", y = "values") + 
  theme(axis.text.x = element_text(size = 10)) + scale_x_discrete(labels = vari_names) 
print(data_bar)
