setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2302_k2/ARCGIS/RES3')

#====================================

bydis <- list()
bydis[[1]] <- 1:10
bydis[[2]] <- 11:20
bydis[[3]] <- 21:30
bydis[[4]] <- 31:40
bydis[[5]] <- 41:50

vari_set <- 'TP' #to_be_set
time_set <- 3 #to_be_set
days_ori <- c(1,2,3,4,5,6)  #to_be_set
strs_mo <- c(1,2,3,4,5,6) #to_be_set
len_days_ori <- 6 #to_be_set
data_5a <- as.matrix(read.csv(paste0('Fig_z2_df_ORI_', vari_set, '_time', time_set, '_ADJ5a.csv')))
data_6a <- matrix(0, nrow = 360, ncol = 5)

for(mm in 1:5){
  for(kk in days_ori){
    for(jj in strs_mo){
      c_1 <- (kk - 1) * 60 + (jj - 1) * 10 + 1
      c_2 <- (kk - 1) * 60 + jj * 10
      
      c_3a <- (mm - 1) * 10 + 1
      c_3b <-  mm * 10
      c_4 <- (kk - 1) * 6 + jj
      data_6a[c_1: c_2,mm] <- data_5a[c_3a:c_3b, c_4]
    }
  }
}

data_6a_df <- as.data.frame(data_6a)
colnames(data_6a_df) <- c('dis1','dis2','dis3','dis4','dis5')
write.csv(data_6a_df, paste0('Fig_z2_df_ORI_', vari_set, '_time', time_set, '_ADJ6a.csv'))