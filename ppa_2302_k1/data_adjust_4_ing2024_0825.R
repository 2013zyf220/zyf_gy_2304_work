setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2302_k2/ARCGIS/RES3')

#============================

vari_set <- 'TP' #to_be_set_key
time_set <- 3 #to_be_set_key
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

#==============================

data_4a <- read.csv(paste0('recb_2_', vari_set, '_', time_set, '_df.csv'))
data_4b <- matrix(0, nrow = len_sites, ncol = len_days_ori * len_strs_mo)
for(ii in days_ori){
  for(jj in strs_mo){
    c_1 <- (ii - 1) * len_days_ori + jj
    c_2 <- (jj - 1) * len_sites + 1
    c_3 <- jj * len_sites
    data_4b[, c_1] <- data_4a[c_2: c_3,ii]
  }
}

data_4b_df <- as.data.frame(data_4b)
colnames(data_4b_df) <- names2b
#write.csv(data_4b_df, paste0('Fig_z2_df_ORI_', vari_set, '_time', time_set,'_ADJ4b.csv'), row.names = FALSE)
write.csv(data_4b_df, paste0('Fig_z2_df_ORI_', vari_set, '_time', time_set,'_ADJB4b.csv'), row.names = FALSE)