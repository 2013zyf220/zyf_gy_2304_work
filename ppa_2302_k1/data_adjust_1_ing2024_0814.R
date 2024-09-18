setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2302_k2/ARCGIS/RES3')
library(readxl)
#============================

vari_set <- 'TP' #to_be_set_key
time_set <- 3 #to_be_set_key
data_1 <- read_excel(paste0('REVISE1c_Fig_z2_df_ORI_', vari_set, '_time', time_set, '.xlsx'), sheet = 'SUMMARY')
data_2 <- as.matrix(data_1)

days_ori <- c(1,2,3,4,5,6) #to_be_set
strs_mo <- c(1,2,3,4,5,6) #to_be_set
len_days_ori <- 6 #to_be_set
len_strs_mo <- 6 #to_be_set

len_bydis_itv <- 10 #to_be_set
len_bydis_num <- 5 #to_be_set
len_sites <- len_bydis_itv * len_bydis_num
len_sites2 <- len_bydis_itv * len_strs_mo

names1 <- c('day1','day2','day3','day4','day5','day6')
names2b <- c('day1-str1','day1-str2','day1-str3','day1-str4','day1-str5','day1-str6',
            'day2-str1','day2-str2','day2-str3','day2-str4','day2-str5','day2-str6',
            'day3-str1','day3-str2','day3-str3','day3-str4','day3-str5','day3-str6',
            'day4-str1','day4-str2','day4-str3','day4-str4','day4-str5','day4-str6',
            'day5-str1','day5-str2','day5-str3','day5-str4','day5-str5','day5-str6',
            'day6-str1','day6-str2','day6-str3','day6-str4','day6-str5','day6-str6'
            )

names2c <- c('day1-str1','day2-str1','day3-str1','day4-str1','day5-str1','day6-str1',
             'day1-str2','day2-str2','day3-str2','day4-str2','day5-str2','day6-str2',
             'day1-str3','day2-str3','day3-str3','day4-str3','day5-str3','day6-str3',
             'day1-str4','day2-str4','day3-str4','day4-str4','day5-str4','day6-str4',
             'day1-str5','day2-str5','day3-str5','day4-str5','day5-str5','day6-str5',
             'day1-str6','day2-str6','day3-str6','day4-str6','day5-str6','day6-str6'
)
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
data_3_df <- as.data.frame(data_3)
colnames(data_3_df) <- names1
#write.csv(data_3_df, paste0('Fig_z2_df_ORI_', vari_set, '_time', time_set,'_ADJ2.csv'), row.names = FALSE)
write.csv(data_3_df, paste0('Fig_z2_df_ORI_', vari_set, '_time', time_set,'_ADJC2.csv'), row.names = FALSE)
#============================


data_3b <- matrix(0, nrow = len_sites, ncol = len_days_ori * len_strs_mo)
for(ii in days_ori){
  for(jj in strs_mo){
    c_1 <- (jj - 1) * len_sites + 1
    c_2 <- jj * len_sites
    c_3 <- (ii - 1) * len_strs_mo + jj
    data_3b[, c_3] <- data_3[c_1: c_2, ii]
  }
}

data_3b_df <- as.data.frame(data_3b)
colnames(data_3b_df) <- names2b
#write.csv(data_3b_df, paste0('Fig_z2_df_ORI_', vari_set, '_time', time_set,'_ADJ2b.csv'), row.names = FALSE)
write.csv(data_3b_df, paste0('Fig_z2_df_ORI_', vari_set, '_time', time_set,'_ADJC2b.csv'), row.names = FALSE)
#============================

data_3c <- matrix(0, nrow = len_sites, ncol = len_days_ori * len_strs_mo)

for(jj in strs_mo){
  for(ii in days_ori){
    c_1 <- (ii - 1) * len_strs_mo + jj
    c_2 <- (jj - 1) * len_days_ori + ii
    data_3c[, c_2] <- data_3b[, c_1]
  }
}

data_3c_df <- as.data.frame(data_3c)
colnames(data_3c_df) <- names2c
#write.csv(data_3c_df, paste0('Fig_z2_df_ORI_', vari_set, '_time', time_set,'_ADJ2c.csv'), row.names = FALSE)
write.csv(data_3c_df, paste0('Fig_z2_df_ORI_', vari_set, '_time', time_set,'_ADJC2c.csv'), row.names = FALSE)