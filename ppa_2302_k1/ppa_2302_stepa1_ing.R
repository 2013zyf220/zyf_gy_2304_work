library(ppcor)
library(relaimpo)
library(readxl)

times_set <- c(2,3) #to_be_set
varis <- c('TP','RH','DI') #to_be_set
  
file_path1 <- "E:/zyf_gn/zyf_gn_2301_data/ppa_2302_k2/ARCGIS/RES3/PREPARE/PROCESS_2/V17D/"
file_path2 <- list()
data_1 <- list()
data_2 <- list()
data_3 <- list()
data_4 <- list()
for(c_time in times_set){
  file_path2[[c_time]] <- paste0(file_path1, 'REVISE2d1_Fig_z2_df_ORI_time', c_time,'_V16C.xlsx')
  data_1[[c_time]] <- as.matrix(read_excel(path = file_path2[[c_time]], sheet = "Sheet1", range = "E1:M109"))
  data_2[[c_time]] <- as.matrix(read_excel(path = file_path2[[c_time]], sheet = "Sheet1", range = "R1:R109"))
  data_3[[c_time]] <- as.matrix(read_excel(path = file_path2[[c_time]], sheet = "Sheet1", range = "Y1:Y109"))
  data_4[[c_time]] <- cbind(data_1[[c_time]],data_2[[c_time]],data_3[[c_time]])
}


data_5 <- list()
for(c_time in times_set){
  data_5[[c_time]] <- list()
  c_varin <- 0
  for(c_vari in varis){
    c_1 <- c_varin * 36 + 1
    c_2 <- (c_varin + 1) * 36
    c_varin <- c_varin + 1
    data_5[[c_time]][[c_vari]] <- as.data.frame(as.matrix(data_4[[c_time]])[c_1:c_2,])
  }
}


#===========================
relimp_rcd1 <- list()
relimp_rci1 <- list()
relimp_rcd2 <- list()
relimp_rci2 <- list()
pcor_1 <- list()
for(c_time in times_set){
  relimp_rcd1[[c_time]] <- list()
  relimp_rci1[[c_time]] <- list()
  relimp_rcd2[[c_time]] <- list()
  relimp_rci2[[c_time]] <- list()
  pcor_1[[c_time]] <- list()
  for(c_vari in varis){
    relimp_rcd1[[c_time]][[c_vari]] <- lm(RCD_estimate1 ~ PS_BLD + PS_VEG + SW + BH_4, data = data_5[[c_time]][[c_vari]])
    relimp_rci1[[c_time]][[c_vari]] <- lm(RCI_estimate1 ~ PS_BLD + PS_VEG + SW + BH_4, data = data_5[[c_time]][[c_vari]])
    relimp_rcd2[[c_time]][[c_vari]] <- calc.relimp(relimp_rcd1[[c_time]][[c_vari]], type = c("lmg"), rela = TRUE)
    relimp_rci2[[c_time]][[c_vari]] <- calc.relimp(relimp_rci1[[c_time]][[c_vari]], type = c("lmg"), rela = TRUE)
    pcor_1[[c_time]][[c_vari]] <- pcor(data_5[[c_time]][[c_vari]])
  }
}







