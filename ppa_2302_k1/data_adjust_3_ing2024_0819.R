setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2302_k2/ARCGIS')

vari_set <- 'RH' #to_be_set
time_set <- 3 #to_be_set
data_1 <- as.matrix(read.csv(paste0('RES2/recb_1_', vari_set, '_time', time_set, '.csv')))
data_2 <- as.matrix(read.csv(paste0('RES3/recb_2_', vari_set, '_', time_set, '_df.csv')))
cols <- colnames(data_1)

data_3 <- matrix(0, nrow = 400, ncol = 6)

data_3[301:400,] <- data_1[301:400,]
data_3[1:100,] <- data_1[301:400,] + data_2[1:100,]
data_3[101:200,] <- data_1[301:400,] + data_2[101:200,]
data_3[201:300,] <- data_1[301:400,] + data_2[201:300,]
data_3_df <- as.data.frame(data_3)
colnames(data_3_df) <- cols
write.csv(data_3_df, paste0('RES2/recb_1_', vari_set, '_time', time_set, '_temp1.csv'), row.names = FALSE)