setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2302_k2/ARCGIS/RES3')

#============================

vari_set <- 'TP' #to_be_set_key
time_set <- 3 #to_be_set_key
data_1 <- read.csv(paste0('VER2024_0814/Fig_z2_df_ORI_', vari_set, '_time', time_set,'_ADJ1.csv'))
data_2 <- as.matrix(data_1)

days_ori <- c(1,2,3,4,5,6) #to_be_set
strs_mo <- c(1,2,3,4,5,6) #to_be_set
len_days_ori <- 6 #to_be_set
len_strs_mo <- 6 #to_be_set

len_bydis_itv <- 10 #to_be_set
len_bydis_num <- 5 #to_be_set
len_sites <- len_bydis_itv * len_bydis_num
len_sites2 <- len_bydis_itv * len_strs_mo
#============================


data_3 <- matrix(0, nrow = len_sites * len_strs_mo, ncol = len_days_ori)
for(ii in days_ori){
  for(jj in strs_mo){
    for(kk in 1:len_bydis_num){
      c_1 <- (jj - 1) * len_sites + (kk - 1) * len_bydis_itv + 1
      c_2 <- (jj - 1) * len_sites + kk * len_bydis_itv
      
      c_3 <- (ii - 1) * len_sites2 + (jj - 1) * len_bydis_itv + 1
      c_4 <- (ii - 1) * len_sites2 + jj * len_bydis_itv
      data_3[c_1: c_2, ii] <- data_2[c_3: c_4,kk]
    }
  }
}

write.csv(data_3, paste0('VER2024_0814/Fig_z2_df_ORI_', vari_set, '_time', time_set,'_ADJ2.csv'))