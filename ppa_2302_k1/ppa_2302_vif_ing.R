library(readxl)
library(car)
setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2302_k2/ARCGIS/RES3')

#=======================================================================
vif_1f <- function(f_time,f_dis){
  f_loc_1 <- paste0('PREPARE/PROCESS_2/V8/REVISE1d1_Fig_z2_df_ORI_time', f_time, '_V8B.xlsx')
  f_dis2 <- paste0('DIS', f_dis)
  f_data_1_svf <- as.vector(read_excel(f_loc_1, sheet = f_dis2, range = 'A1:A61'))[[1]]
  f_data_1_bld <- as.vector(read_excel(f_loc_1, sheet = f_dis2, range = 'B1:B61'))[[1]]
  f_data_1_veg <- as.vector(read_excel(f_loc_1, sheet = f_dis2, range = 'C1:C61'))[[1]]
  f_data_1_bh <- as.vector(read_excel(f_loc_1, sheet = f_dis2, range = 'E1:E61'))[[1]]
  f_data_1_sw <- as.vector(read_excel(f_loc_1, sheet = f_dis2, range = 'I1:I61'))[[1]]
  
  f_data_2_TP1 <- as.vector(read_excel(f_loc_1, sheet = f_dis2, range = 'BD1:BD61'))[[1]]
  f_data_2_RH1 <- as.vector(read_excel(f_loc_1, sheet = f_dis2, range = 'DH1:DH61'))[[1]]
  
  f_data_3_TP <- data.frame(f_data_1_svf, f_data_1_bld, f_data_1_veg, f_data_1_bh, f_data_1_sw, f_data_2_TP1)
  f_data_3_RH <- data.frame(f_data_1_svf, f_data_1_bld, f_data_1_veg, f_data_1_bh, f_data_1_sw, f_data_2_RH1)
  
  f_model_TP <- lm(f_data_2_TP1 ~ f_data_1_svf + f_data_1_bld + f_data_1_veg + f_data_1_bh + f_data_1_sw, data = f_data_3_TP)
  f_model_RH <- lm(f_data_2_RH1 ~ f_data_1_svf + f_data_1_bld + f_data_1_veg + f_data_1_bh + f_data_1_sw, data = f_data_3_RH)

  f_vif_TP <- vif(f_model_TP)
  f_vif_RH <- vif(f_model_RH)
  
  f_res <- matrix(0, nrow = 2, ncol = 5)
  f_res[1,] <- f_vif_TP
  f_res[2,] <- f_vif_RH
  return(f_res)
}

a1 =vif_1f(3,1)
#=======================================================================

vif_res <- matrix(0, nrow = 10, ncol = 10)
for(c_time in 2:3){
  for(c_dis in 1:5){
    c_1 <- (c_dis - 1) * 2 + 1
    c_2 <- (c_dis - 1) * 2 + 2
    c_3 <- (c_time - 2) * 5 + 1
    c_4 <- (c_time - 2) * 5 + 5
    vif_res[c_1: c_2, c_3: c_4] <- vif_1f(c_time, c_dis)
  }
}



