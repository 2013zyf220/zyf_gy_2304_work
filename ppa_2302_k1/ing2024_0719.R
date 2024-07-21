#up2024_0720_22:57

regreb_6f <- function(f_sub, f_vari, f_time, f_bydis_num, f_reg_se){
  f_days <- days_trans_f(f_sub)
  f_order <- bydis[[f_bydis_num]]
  f_y_1 <- c()
  for(f_day in f_days){
    f_name <- paste0('ORI_', f_vari, '_time', f_time,'_day',f_day)
    f_y_1 <- c(f_y_1, index_2[[f_name]][f_order])
  }
  
  f_r2a <- rep(0, length(f_reg_se))
  f_x_2 <- matrix(0, nrow = bydis_itv * len_strs_mo * length(f_days), ncol = length(f_reg_se))
  for(mm in 1:length(f_reg_se)){
    c_reg <- f_reg_se[mm] 
    f_x_1 <- rep(index_2[, c_reg][f_order], length(f_days))
    f_model_1 <- lm(f_y_1 ~ f_x_1)
    f_r2a[mm] <- summary(f_model_1)$r.squared
    f_x_2[,mm] <- f_x_1
  }
  
  f_z_1 <- cbind(f_x_2, f_y_1)
  f_z_2 <- as.data.frame(f_z_1)
  f_colnames <- append(cname_index_2, 'y_regreb_6')
  colnames(f_z_2) <- f_colnames
  f_model_for <- as.formula(paste0('y_regreb_6', ' ~ ', cname_index_3)) 
  f_model_res <- lm(f_model_for, data = f_z_2)
  f_model_sum <- summary(f_model_res)
  f_r2b <- f_model_sum$r.squared
  f_res <- list()
  f_res[['r2a']] <- f_r2a
  f_res[['r2b']] <- f_r2b
  f_res[['x_2']] <- f_x_2
  return(f_res)
}



#========================================
#ing

regre_6f_sub <- 'ORI'

regre_6fb_r2a <- list()
for(c_vari in varis){
  regre_6fb_r2a[[c_vari]] <- list()
  for(ii in times_set){
    regre_6fb_r2a[[c_vari]][[ii]] <- matrix(0, nrow = bydis_num, ncol = length(reg_se))
    for(mm in 1: bydis_num){
      regre_6fb_r2a[[c_vari]][[ii]][mm,] <- regreb_6f(regre_6f_sub, c_vari, ii, mm, reg_se)$r2a
    }
    colnames(regre_6fb_r2a[[c_vari]][[ii]]) <- cname_index_2
    write.csv(regre_6fb_r2a[[c_vari]][[ii]], paste0('RES3/Fig_r2a_', regre_6f_sub, '_', c_vari, '_time', ii, '.csv'), row.names = FALSE)
  }
}


#========================================
rce_r1_sub <- 'ORI'
rce_r1_days <- days_trans_f(rce_r1_sub)
rce_r1_a1 <- list()
for(c_vari in varis){
  rce_r1_a1[[c_vari]] <- list()
  for(ii in times_set){
    rce_r1_a1[[c_vari]] <- matrix(0, nrow = 6, ncol = 6)
    for(jj in strs_mo){
      for(kk in 1: length(rce_r1_days)){
        rce_r1_a1[[c_vari]][jj,kk] <- rce_r1[[c_vari]][[ii]][[jj]][[rce_r1_days[kk]]]$model_rcd_1
      }
    }
  }
}

#========================================
rce_r2_sub <- 'ORI'

rce_r2_a1 <- list()
for(c_vari in varis){
  rce_r2_a1[[c_vari]] <- matrix(0, nrow = 3, ncol = 6)
  for(ii in times_set){
    for(jj in strs_mo){
        rce_r2_a1[[c_vari]][ii,jj] <- rce_r2[[rce_r2_sub]][[c_vari]][[ii]][[jj]]$model_rcd_1
    }
  }
}
