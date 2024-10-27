library(readxl)
library(car)
setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2302_k2/ARCGIS/RES3')

#=======================================================================
time_set_1 <- 2 #to_be_set
loc_1 <- paste0('PREPARE/PROCESS_2/V8/REVISE1d1_Fig_z2_df_ORI_time', time_set_1, '_V8B.xlsx')

data_1_svf <- c()
data_1_bld <- c()
data_1_veg <- c()
data_1_bh <- c()
data_1_sw <- c()

for(c_dis in 1:5){
  c_dis2 <- paste0('DIS', c_dis)
  data_2_svf <- as.vector(read_excel(loc_1, sheet = c_dis2, range = 'A1:A61'))[[1]]
  data_2_bld <- as.vector(read_excel(loc_1, sheet = c_dis2, range = 'B1:B61'))[[1]]
  data_2_veg <- as.vector(read_excel(loc_1, sheet = c_dis2, range = 'C1:C61'))[[1]]
  data_2_bh <- as.vector(read_excel(loc_1, sheet = c_dis2, range = 'E1:E61'))[[1]]
  data_2_sw <- as.vector(read_excel(loc_1, sheet = c_dis2, range = 'I1:I61'))[[1]]
  data_1_svf <- c(data_1_svf, data_2_svf)
  data_1_bld <- c(data_1_bld, data_2_bld)
  data_1_veg <- c(data_1_veg, data_2_veg)
  data_1_bh <- c(data_1_bh, data_2_bh)
  data_1_sw <- c(data_1_sw, data_2_sw)
}

#=======================================================================
vif_1f <- function(f_time, f_vari){
  
  if(f_vari == 'TP'){
    f_range <- 'BD1:BD61'
  }else if(f_vari == 'RH'){
    f_range <- 'DH1:DH61'
  }else{
    print('ERROR')
  }
  
  f_data_1 <- c()
  f_loc_1 <- paste0('PREPARE/PROCESS_2/V8/REVISE1d1_Fig_z2_df_ORI_time', f_time, '_V8B.xlsx')
  for(c_dis in 1:5){
    c_dis2 <- paste0('DIS', c_dis)
    f_data_2 <-  as.vector(read_excel(f_loc_1, sheet = c_dis2, range = f_range))[[1]]
    f_data_1 <- c(f_data_1, f_data_2)
  }
  f_data_3 <- data.frame(data_1_svf, data_1_bld, data_1_veg, data_1_bh, f_data_1)
  f_model <- lm(f_data_1 ~ data_1_svf + data_1_bld + data_1_veg + data_1_bh, data = f_data_3)
  f_vif <- vif(f_model)
  return(f_vif)
}
#=======================================================================
res_1 <- matrix(0, nrow = 2, ncol = 8)

varis <- c('TP','RH')
for(c_time in 2:3){
  for(c_vari in varis){
    c_1 <- c_time - 1
    if(c_vari == 'TP'){
      c_2 <- 1
      c_3 <- 4
    }else if(c_vari == 'RH'){
      c_2 <- 5
      c_3 <- 8
    }else{
      print('ERROR')
    }
    res_1[c_1,c_2:c_3] <- vif_1f(c_time, c_vari)
  }
}

