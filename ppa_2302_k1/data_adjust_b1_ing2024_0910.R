library(readxl)
library(openxlsx)
library(MASS)
setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2302_k2/ARCGIS/RES3')

#=========================================
#up2024_0917

bydis <- c(1,2,3,4,5)   #to_be_set
days_ori <- c(1,2,3,4,5,6)   #to_be_set
strs_mo <- c(1,2,3,4,5,6)   #to_be_set
len_strs_mo <- 6   #to_be_set
len_days_ori <- 6   #to_be_set
len_sites <- 50   #to_be_set
time_set <- 3  #to_be_set_key
vari_set <- 'TP'  #to_be_set_key
data_1 <- read_excel(paste0('PREPARE/REVISE2d1_Fig_z2_df_ORI_time', time_set, '.xlsx'), sheet = 'THEORY_3')

if(vari_set == 'RH'){
  RCI_1 <- data_1$RCI_RH_1 
  RCD_1 <- data_1$RCD_RH_1 
}else if(vari_set == 'TP'){
  RCI_1 <- data_1$RCI_TP_1
  RCD_1 <- data_1$RCD_TP_1
}else{
  print('ERROR')
}

if(vari_set == 'RH'){
  RCI_2 <- RCI_1
}else{
  RCI_2 <- -RCI_1
}


RCD_2 <- RCD_1/10

SLP_1 <- RCI_1/RCD_2
RCD_2_len <- length(RCD_2)

#=========================================
#up2024_0917

rce_curve_1 <- function(f_inter_x, f_inter_y){
  f_a <- matrix(c(f_inter_x^2, f_inter_x, 0, 0), nrow=2, byrow=TRUE)
  f_b <- matrix(c(-f_inter_y, f_inter_y), ncol=1)
  f_coef <- ginv(f_a) %*% f_b
  f_coef_1 <- f_coef[1]
  f_coef_2 <- f_coef[2]
  f_z1 <- rep(0, len_sites)
  for(ii in 1: f_inter_x){
    f_z1[ii] <- f_coef_1 * ii^2 + f_coef_2 * ii + f_inter_y
  }
  
  f_res <- list()
  f_res[['a']] <- f_a
  f_res[['b']] <- f_b
  f_res[['coef_1']] <- f_coef_1
  f_res[['coef_2']] <- f_coef_2
  f_res[['z1']] <- f_z1
  return(f_res)
}

#=========================================
#up2024_0917
values_1A <- matrix(0, ncol = RCD_2_len, nrow = len_sites + 2) #to_be_set
for(ii in 1: RCD_2_len){
  values_1A[1: len_sites,ii] <- rce_curve_1(RCD_2[ii], RCI_2[ii])$z1
  values_1A[51,ii] <- RCD_2[ii]
  values_1A[52,ii] <- RCI_2[ii]
}
values_1 <- values_1A[1:len_sites,]
image(values_1, main = "values_1", xlab = "列", ylab = "行", col = heat.colors(10))
#=========================================
#up2024_0917

values_1B <- matrix(0, ncol = RCD_2_len, nrow = len_sites) #to_be_set
for(ii in 1: RCD_2_len){
  for(jj in 1: RCD_2[ii]){
    values_1B[jj,ii] <- RCI_2[ii] + SLP_1[ii] * (jj - 1)
  }
  values_1B[49,ii] <- RCD_1[ii]
  values_1B[50,ii] <- RCI_1[ii]
}

#=========================================
#up2024_0917

strs_mo2 <- matrix(0, ncol = 6, nrow = 6)
kk <- 0 
for(ii in 1:6){
  for(jj in 1:6){
    kk <- kk + 1
    strs_mo2[ii,jj] <- kk
  }
}

#=========================================
#up2024_0917

values_2 <- list()
values_2_mean <- matrix(0, ncol = len_strs_mo, nrow = len_sites)
for(ii in 1: len_strs_mo){
  c_1 <- strs_mo2[,ii]
  values_2[[ii]] <- values_1[, c_1]
  values_2_mean[,ii] <- rowMeans(values_2[[ii]])
}

#=======================
#up2024_0917

values_3 <- matrix(0, ncol = 4, nrow = 10 * len_strs_mo)
for(ii in 1:4){
  for(jj in strs_mo){
    c_1 <- (jj - 1) * 10 + 1
    c_2 <- jj * 10
    
    c_3 <- (ii - 1) * 10 + 1
    c_4 <- ii * 10
    values_3[c_1: c_2,ii] <- round(values_2_mean[c_3: c_4, jj],1)
  }
}

image(values_2_mean, main = "values_2_mean", xlab = "列", ylab = "行", col = heat.colors(10))
image(values_2[[1]], main = "values_2_example", xlab = "列", ylab = "行", col = heat.colors(10)) #to_be_set
#=======================
#up2024_0917

values_4 <-  matrix(0, ncol = 4, nrow = 10 * len_strs_mo * len_days_ori)
for(ii in 1:4){
  for(jj in days_ori){
    for(kk in strs_mo){
      c_1 <- (jj - 1) * 60 + (kk - 1) * 10 + 1
      c_2 <- (jj - 1) * 60 + kk * 10
      
      c_3 <- (ii - 1) * 10 + 1
      c_4 <- ii * 10
      values_4[c_1: c_2,ii] <- round(values_2[[kk]][c_3: c_4,jj],1)
    }
  }
}


values_3_df <- as.data.frame(values_3)
values_4_df <- as.data.frame(values_4)
#write.xlsx(values_3_df, paste0('PREPARE/RCE_THEORY_EXPORT_VALUE3_time', time_set,'_', vari_set, '.xlsx'))
write.xlsx(values_4_df, paste0('PREPARE/RCE_THEORY_EXPORT_VALUE4_time', time_set,'_', vari_set, '.xlsx'))
