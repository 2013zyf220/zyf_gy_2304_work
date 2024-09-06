setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2302_k2/ARCGIS/RES3')

#============================

vari_set <- 'TP' #to_be_set_key
time_set <- 3 #to_be_set_key

len_sites <- 50 #to_be_set_key
len_days_ori <- 6 #to_be_set_key
len_strs_mo <- 6 #to_be_set_key

days_ori <- c(1,2,3,4,5,6)
strs_mo <- c(1,2,3,4,5,6)

days_name <- c('day1','day2','day3','day4','day5','day6')
#data_5a <- read.csv(paste0('Fig_z2_df_ORI_', vari_set, '_time', time_set, '_ADJ5a.csv'))
data_5a <- read.csv(paste0('Fig_z2_df_ORI_', vari_set, '_time', time_set, '_ADJB5a.csv'))
data_5am <- as.matrix(data_5a)

data_5b <- matrix(0, nrow = len_strs_mo * len_sites, ncol = len_days_ori)

for(kk in days_ori){
  for(jj in strs_mo){
    c_1 <- (jj - 1) * len_sites + 1
    c_2 <- jj * len_sites
    
    c_3 <- (kk - 1) * len_days_ori + jj
    data_5b[c_1: c_2, kk] <- data_5a[, c_3]
  }
}

data_5b_df  <- as.data.frame(data_5b)
colnames(data_5b_df) <- days_name

#write.csv(data_5b_df, paste0('Fig_z2_df_ORI_', vari_set, '_time', time_set, '_ADJ5b.csv'), row.names = FALSE)
write.csv(data_5b_df, paste0('Fig_z2_df_ORI_', vari_set, '_time', time_set, '_ADJB5b.csv'), row.names = FALSE)
#=================================
data_5c1 <- as.matrix(read.csv(paste0('recb_1_', vari_set, '_', time_set, '_df.csv')))
data_5c2 <- data_5c1[301,]
data_5c3 <- matrix(0, nrow = 300, ncol = 6)
for(mm in 1:300){
  data_5c3[mm,] <-  data_5c2 + data_5b[mm,]
}

data_5c_df  <- as.data.frame(data_5c3)
colnames(data_5c_df) <- days_name
#write.csv(data_5c_df, paste0('Fig_z2_df_ORI_', vari_set, '_time', time_set, '_ADJ5c.csv'), row.names = FALSE)
write.csv(data_5c_df, paste0('Fig_z2_df_ORI_', vari_set, '_time', time_set, '_ADJB5c.csv'), row.names = FALSE)