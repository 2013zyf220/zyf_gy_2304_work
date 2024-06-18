library(MASS)
library(skimr)
library(DataExplorer)
library(tidyverse)
library(caret)
library(pROC)
library(ggplot2)
library(readxl)
library(gridExtra)

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2302_k2/ARCGIS')

#=================================================================

#result 1: each variable, each time, each street, each day
#result 2: each variable, each time, streets together, each day
#result 3: each sub, each variable, each time, each street, days merged
#result 4: each sub, each variable, each time, streets together, days merged
#result 5: each sub, each variable, each time, each street, days together
#result 6: each sub, each variable, each time, streets together, days together
#=================================================================
#up2024_0528_17:00

load('RES1/ANA1_data_1_paras.RData')

times_set <- data_1_paras[['times_set']]
strs_co <- data_1_paras[['strs_co']]
strs_mo <- data_1_paras[['strs_mo']]
days_ori <- data_1_paras[['days_ori']]
days_nor <- data_1_paras[['days_nor']]
days_hot <- data_1_paras[['days_hot']]
varis <- data_1_paras[['varis']]
subs1 <- data_1_paras[['subs1']]
subs1_name <- data_1_paras[['subs1_name']]

subs <- c(ORI = list(days_ori), subs1)
subs_name <- c('ORI', subs1_name)

days_ori_name <- c('day1','day2','day3','day4','day5','day6')  #to_be_set
days_nor_name <- c('day1','day2','day3','day5')  #to_be_set
days_hot_name <- c('day4','day6')  #to_be_set
times_set_name <- c('time1','time2','time3')

len_times_set <- length(times_set)
len_strs_co <- length(strs_co)
len_strs_mo <- length(strs_mo)
len_days_ori <- length(days_ori)
len_days_nor <- length(days_nor)
len_days_hot <- length(days_hot)
len_varis <- length(varis)
len_subs <- length(subs)
len_sites <- 50 #to_be_set

ele_2a <- read.csv('ele_2_table.csv')
ele_2b <- as.matrix(ele_2a)
ele_2c <- matrix(ele_2b, nrow = len_sites * len_strs_mo, ncol = 1)

buf_set <- 100 #to_be_set
index_name1 <- paste0('index_1m_df3_buf', buf_set, '.csv')
index_1 <- read.csv(index_name1)[1:300,]
index_1$ele_2 <- ele_2c
index_2 <- index_1

indep_set <- 20 #to_be_set_key

#=================================================================
#up2024_0531_17:00

reg_se_sum <- list()
reg_se_sum[[1]] <- c(1,3,4)
reg_se_sum[[2]] <- c(12)

reg_set <- 1 #to_be_set_key
reg_se <- reg_se_sum[[reg_set]]
#=================================================================
#up2024_0528_17:00
#set independent variables for regression

cname_index_1 <- colnames(index_1)
cname_index_2 <- cname_index_1[reg_se] #to_be_set_key
cname_index_3 <- paste(cname_index_2, collapse = ' + ')

#============================define functions=====================================
#up2024_0528_17:00
#define function: from subs_name to subs

days_trans_f <- function(f_1){
  if(f_1 == 'ORI'){
    f_2 <- days_ori
  }else if(f_1 == 'NOR'){
    f_2 <- days_nor
  }else if(f_1 == 'HOT'){
    f_2 <- days_hot
  }
  return(f_2)
}

#======================================================
#up2024_0528_17:00

dis_1 <- seq(10, 500, 10) #to_be_set
dis_2 <- rep(dis_1, len_strs_mo)
dis_3 <- list()
dis_3[['ORI']] <- rep(dis_1, len_days_ori)
dis_3[['NOR']] <- rep(dis_1, len_days_nor)
dis_3[['HOT']] <- rep(dis_1, len_days_hot)

#==============================================
#up2024_0617_20:30

days_array_f <- function(f_days){
  f_array <- c()
  for (mm in f_days){
    f_s <- (mm - 1) * 50 + 1
    f_e <- mm * 50
    f_array <- c(f_array, f_s:f_e)
  }
  return(f_array)
}

days_array_list <- list()
for(c_sub_name in subs_name){
  days_array_list[[c_sub_name]] <- days_array_f(days_trans_f(c_sub_name))
}
#==============================================
#up2024_0617_20:30

bydis_itv <- 10 #to_be_set
bydis_num <- len_sites/bydis_itv

bydis <- list()
for(mm in 1: bydis_num){
  c_res <- c()
  for(nn in 1: len_strs_mo){
    c_s <- (nn - 1) * 50 + (mm - 1) * 10 + 1
    c_e <- (nn - 1) * 50 + mm * 10
    c_res <- c(c_res, c_s:c_e)
  }
  bydis[[mm]] <- c_res
}

bydis2 <- list()
for(mm in 1: bydis_num){
  c_s <- (mm - 1) * 10 + 1
  c_e <- mm * 10
  bydis2[[mm]] <- c_s: c_e
}

cat('========================step 1: basic data(data2_2)==========================\n')
#up2024_0528_17:00

data2_2_ori <- list()
for(c_vari in varis){
  data2_2_ori[[c_vari]] <- list()
  for(ii in times_set){
    data2_2_ori[[c_vari]][[ii]] <- as.matrix(read.csv(paste0('RES2/recb_2_', c_vari, '_', ii,'_df.csv'))) #to_be_set
  } 
}

#=========================================================
#up2024_0528_18:00

data2_2_nor <- list()
data2_2_hot <- list()
for(c_vari in varis){
  data2_2_nor[[c_vari]] <- list()
  data2_2_hot[[c_vari]] <- list()
  for(ii in times_set){
    data2_2_nor[[c_vari]][[ii]] <- data2_2_ori[[c_vari]][[ii]][ ,days_nor]
    data2_2_hot[[c_vari]][[ii]] <- data2_2_ori[[c_vari]][[ii]][ ,days_hot]
  }
}

data2_2 <- list()
data2_2[['ORI']] <- data2_2_ori
data2_2[['NOR']] <- data2_2_nor
data2_2[['HOT']] <- data2_2_hot

cat('========================step 2: calculate mean==========================\n')

#up2024_0528_18:00
#function: input weather data(mean)

d2_mean_1f <- function(f_data, f_sub, f_vari){
  f_1 <- f_data[[f_sub]][[f_vari]]
  f_mean_1 <- matrix(0, nrow = len_sites * len_strs_mo, ncol = len_times_set)
  for(ii in times_set){
    f_mean_1[ ,ii] <- rowMeans(f_1[[ii]])
  }
  f_res <- list()
  f_res[['r1']] <- f_mean_1
  return(f_res)
}

#==============================================
#up2024_0528_18:00
#summarize data by averaging days

data2_2_mean <- list()
for(c_sub_name in subs_name){
  data2_2_mean[[c_sub_name]] <- list()
  for(c_vari in varis){
    data2_2_mean[[c_sub_name]][[c_vari]] <- d2_mean_1f(data2_2,c_sub_name, c_vari)$r1
    c_data2_2_mean <- data2_2_mean[[c_sub_name]][[c_vari]]
    c_data2_2_mean_df <- as.data.frame(c_data2_2_mean)
    colnames(c_data2_2_mean_df) <- times_set_name
    write.csv(c_data2_2_mean_df, paste0('RES2/data2_2_mean_', c_sub_name, '_', c_vari,'.csv'), row.names = FALSE)
  }
}

cat('========================step 3: data for regression==========================\n')
#up2024_0528_18:00
#get index_1

for(c_sub_name in subs_name){
  for(c_vari in varis){
    for(ii in times_set){
      c_name <- paste0('mean1_', c_sub_name, '_', c_vari, '_times_', ii)
      index_1[[c_name]] <- data2_2_mean[[c_sub_name]][[c_vari]][,ii]
    }
  }
}

write.csv(index_1, paste0('RES2/index_1b.csv'), row.names = FALSE)

index_1_set_1 <- c(12,15)
index_1c <- index_1[index_1_set_1]
write.csv(index_1c, paste0('RES2/index_1c.csv'), row.names = FALSE)

#======================================================
#up2024_0528_18:00
#get index_2

for(c_vari in varis){
  for(ii in times_set){
    for(kk in days_ori){
      c_name <- paste0('ORI_', c_vari, '_time', ii, '_day', kk)
      index_2[[c_name]] <- data2_2_ori[[c_vari]][[ii]][,kk]
    }
  }
}

write.csv(index_2, paste0('RES2/index_2.csv'), row.names = FALSE)

#==============================================
#up2024_0528_18:00
#separate index_1/index_2 by streets

index_1s <- list()
index_2s <- list()
for(ii in 1: len_strs_mo){
  c_1 <- (ii - 1) * len_sites + 1
  c_2 <- ii * len_sites
  index_1s[[ii]] <- index_1[c_1:c_2,]
  index_2s[[ii]] <- index_2[c_1:c_2,]
}


cat('========================step 6: figures==========================\n')
#up2024_0531_21:00

time_set <- 1 #to_be_set
str_set <- 1 #to_be_set
day_set <- 1 #to_be_set


#==============================================
#up2024_0601_21:30

fig1b_list_f <- function(f_vari, f_time, f_indep){
  f_fig1b_list <- list()
  nn <- 0
  for(jj in strs_mo){
    fc_1 <- (jj - 1) * len_sites + 1
    fc_2 <- jj * len_sites
    for(kk in days_ori){
      nn <- nn + 1
      f_fig1b_list[[nn]] <- data.frame(
        xx = index_1[[f_indep]][fc_1:fc_2],
        yy = data2_2_ori[[f_vari]][[f_time]][fc_1:fc_2,kk]
      )
    }
  }
  return(f_fig1b_list)
}

#==============================================
#up2024_0601_21:30

fig1b_plot_f <- function(f_data, f_index, f_vari, f_indep){
  f_str <- (f_index - 1)%/% len_days_ori + 1
  f_day <- (f_index - 1)%% len_days_ori + 1
  f_model <- lm(yy ~ xx, data = f_data)
  f_interc <- coef(f_model)[1]
  f_slope <- coef(f_model)[2]
  f_eq <- paste0('y = ', round(f_interc, 2), ' + ', round(f_slope, 2), ' * x')
  
  f_p <- ggplot(f_data, aes(x = xx, y = yy)) +
    geom_point() +
    geom_smooth(method = 'lm', se = FALSE, col = 'blue') +
    annotate('text', x = Inf, y = Inf, label = f_eq, hjust = 1.1, vjust = 1.5, size = 5, color = 'red') +
    ggtitle(paste('str:', f_str,'_day:',f_day)) +
    xlab(colnames(index_1)[f_indep]) + 
    ylab(f_vari) + 
    theme_minimal()
  
  return(f_p)
}

#==============================================
#up2024_0602_11:30

fig1b_res_f <- function(f_vari, f_time, f_indep){
  f_data_fig1b_list_1 <- fig1b_list_f(f_vari, f_time, f_indep)
  f_plots_fig1b_1 <- lapply(seq_along(f_data_fig1b_list_1), function(nn) fig1b_plot_f(f_data_fig1b_list_1[[nn]], nn, f_vari, f_indep))
  f_comb_plots_fig1b_1 <- grid.arrange(grobs = f_plots_fig1b_1, ncol = len_days_ori, nrow = len_strs_mo)
  ggsave(paste0('FIG2/comb_plots_fig1b_1', f_vari, '_', f_time, '_', f_indep,'.jpg'), plot = f_comb_plots_fig1b_1, width = 15, height = 15, dpi = 300)
  return(f_comb_plots_fig1b_1)
}

#fig1b_res <- list()
#for(c_vari in varis){
#  fig1b_res[[c_vari]] <- list()
#  for(ii in times_set){
#    fig1b_res[[c_vari]][[ii]] <- fig1b_res_f(c_vari,ii, indep_set)
#  }
#}

#check_1 <- fig1b_res_f('TP',1, indep_set) #to_be_set
#==============================================
#up2024_0617_23:00

fig1c_list_f <- function(f_vari, f_time, f_str, f_indep){
  f_fig1c_list <- list()
  nn <- 0
  for(kk in days_ori){
    for(pp in 1:bydis_num){
      nn <- nn + 1
      fc_1 <- (f_str - 1) * len_sites + (pp - 1) * bydis_itv + 1
      fc_2 <- (f_str - 1) * len_sites + pp * bydis_itv
      f_fig1c_list[[nn]] <- data.frame(
        xx = index_1[[f_indep]][fc_1:fc_2],
        yy = data2_2_ori[[f_vari]][[f_time]][fc_1:fc_2,kk]
      )
    }
  }
  return(f_fig1c_list)
}

#==============================================
#up2024_0617_23:00

fig1c_plot_f <- function(f_data, f_index, f_vari, f_indep){
  f_day <- (f_index - 1)%/% bydis_num + 1
  f_bydis <- (f_index - 1)%% bydis_num + 1
  f_model <- lm(yy ~ xx, data = f_data)
  f_interc <- coef(f_model)[1]
  f_slope <- coef(f_model)[2]
  f_eq <- paste0('y = ', round(f_interc, 2), ' + ', round(f_slope, 2), ' * x')
  
  f_p <- ggplot(f_data, aes(x = xx, y = yy)) +
    geom_point() +
    geom_smooth(method = 'lm', se = FALSE, col = 'blue') +
    annotate('text', x = Inf, y = Inf, label = f_eq, hjust = 1.1, vjust = 1.5, size = 5, color = 'red') +
    ggtitle(paste('day:',f_day,'_dis', f_bydis)) +
    xlab(colnames(index_1)[f_indep]) + 
    ylab(f_vari) + 
    theme_minimal()
  
  return(f_p)
}

#==============================================
#up2024_0617_23:00

fig1c_res_f <- function(f_vari, f_time, f_str, f_indep){
  f_data_fig1c_list_1 <- fig1c_list_f(f_vari, f_time, f_str, f_indep)
  f_plots_fig1c_1 <- lapply(seq_along(f_data_fig1c_list_1), function(nn) fig1c_plot_f(f_data_fig1c_list_1[[nn]], nn, f_vari, f_indep))
  f_comb_plots_fig1c_1 <- grid.arrange(grobs = f_plots_fig1c_1, ncol = bydis_num, nrow = len_days_ori)
  ggsave(paste0('FIG2/comb_plots_fig1c_1', f_vari, '_', f_time, '_str', f_str, '_indep', f_indep,'.jpg'), plot = f_comb_plots_fig1c_1, width = 15, height = 15, dpi = 300)
  return(f_comb_plots_fig1c_1)
}

#fig1c_res <- list()
#for(c_vari in varis){
#  fig1c_res[[c_vari]] <- list()
#  for(ii in times_set){
#    fig1c_res[[c_vari]][[ii]] <- list()
#    for(jj in strs_mo){
#      fig1c_res[[c_vari]][[ii]][[jj]] <- fig1c_res_f(c_vari,ii, jj, indep_set)
#    }
#  }
#}

check_1c <- fig1c_res_f('TP',1, 1, 20) #to_be_set

#==============================================
#up2024_0601_21:30

fig2b_list_f <- function(f_vari, f_indep){
  f_fig2b_list <- list()
  nn <- 0
  for(ii in times_set){
    for(kk in days_ori){
      nn <- nn + 1
      f_fig2b_list[[nn]] <- data.frame(
        xx = index_1[[f_indep]],
        yy = data2_2_ori[[f_vari]][[ii]][,kk]
      )     
    }
  }
  return(f_fig2b_list)
}

#==============================================
#up2024_0601_21:30

fig2b_plot_f <- function(f_data, f_index, f_vari, f_indep){
  f_time <- (f_index - 1)%/% len_days_ori + 1
  f_day <- (f_index - 1)%% len_days_ori + 1
  f_model <- lm(yy ~ xx, data = f_data)
  f_interc <- coef(f_model)[1]
  f_slope <- coef(f_model)[2]
  f_eq <- paste0('y = ', round(f_interc, 2), ' + ', round(f_slope, 2), ' * x')
  
  f_p <- ggplot(f_data, aes(x = xx, y = yy)) +
    geom_point() +
    geom_smooth(method = 'lm', se = FALSE, col = 'blue') +
    annotate('text', x = Inf, y = Inf, label = f_eq, hjust = 1.1, vjust = 1.5, size = 5, color = 'red') +
    ggtitle(paste('time:', f_time, 'day:',f_day)) +
    xlab(colnames(index_1)[f_indep]) + 
    ylab(f_vari) + 
    theme_minimal()
  
  return(f_p)
}

#==============================================
#up2024_0602_11:30

fig2b_res_f <- function(f_vari, f_indep){
  cat(f_vari, '\n')
  f_data_fig2b_list_1 <- fig2b_list_f(f_vari, f_indep)
  f_plots_fig2b_1 <- lapply(seq_along(f_data_fig2b_list_1), function(nn) fig2b_plot_f(f_data_fig2b_list_1[[nn]], nn, f_vari, f_indep))
  f_comb_plots_fig2b_1 <- grid.arrange(grobs = f_plots_fig2b_1, ncol = len_days_ori, nrow = len_times_set)
  ggsave(paste0('FIG2/comb_plots_fig2b_1', f_vari, '_', f_indep, '.jpg'), plot = f_comb_plots_fig2b_1, width = 15, height = 15, dpi = 300)
  return(f_comb_plots_fig2b_1)
}

#fig2b_res <- list()
#for(c_vari in varis){
#  fig2b_res[[c_vari]] <- fig2b_res_f(c_vari, indep_set)
#}

check_2 <- fig2b_res_f('TP', 10)
#==============================================
#up2024_0617_23:00

fig2c_list_f <- function(f_vari, f_time, f_indep){
  f_fig2c_list <- list()
  nn <- 0
  for(kk in days_ori){
    for(pp in 1: bydis_num){
      nn <- nn + 1
      f_fig2c_list[[nn]] <- data.frame(
        xx = index_1[[f_indep]][bydis[[pp]]],
        yy = data2_2_ori[[f_vari]][[f_time]][bydis[[pp]],kk]
      )     
    }
  }
  return(f_fig2c_list)
}

#==============================================
#up2024_0617_23:00

fig2c_plot_f <- function(f_data, f_index, f_vari, f_indep){
  f_day <- (f_index - 1)%/% bydis_num + 1
  f_bydis <- (f_index - 1)%% bydis_num + 1
  f_model <- lm(yy ~ xx, data = f_data)
  f_interc <- coef(f_model)[1]
  f_slope <- coef(f_model)[2]
  f_eq <- paste0('y = ', round(f_interc, 2), ' + ', round(f_slope, 2), ' * x')
  
  f_p <- ggplot(f_data, aes(x = xx, y = yy)) +
    geom_point() +
    geom_smooth(method = 'lm', se = FALSE, col = 'blue') +
    annotate('text', x = Inf, y = Inf, label = f_eq, hjust = 1.1, vjust = 1.5, size = 5, color = 'red') +
    ggtitle(paste('day:',f_day,'_dis',f_bydis)) +
    xlab(colnames(index_1)[f_indep]) + 
    ylab(f_vari) + 
    theme_minimal()
  
  return(f_p)
}

#==============================================
#up2024_0617_16:30

fig2c_res_f <- function(f_vari, f_time, f_indep){
  cat(f_vari, '\n')
  f_data_fig2c_list_1 <- fig2c_list_f(f_vari, f_time, f_indep)
  f_plots_fig2c_1 <- lapply(seq_along(f_data_fig2c_list_1), function(nn) fig2c_plot_f(f_data_fig2c_list_1[[nn]], nn, f_vari, f_indep))
  f_comb_plots_fig2c_1 <- grid.arrange(grobs = f_plots_fig2c_1, ncol = bydis_num, nrow = len_days_ori)
  ggsave(paste0('FIG2/comb_plots_fig2c_1', f_vari, '_', f_time, '_', f_indep, '.jpg'), plot = f_comb_plots_fig2c_1, width = 15, height = 15, dpi = 300)
  return(f_comb_plots_fig2c_1)
}

#fig2c_res <- list()
#for(c_vari in varis){
#  fig2c_res[[c_vari]] <- list()
#  for(ii in times_set){
#    fig2c_res[[c_vari]][[ii]] <- fig2c_res_f(c_vari, ii, indep_set)
#  }
#}

check_2c <- fig2c_res_f('TP',1,20)
#==============================================
#up2024_0601_21:30

fig3b_list_f <- function(f_sub, f_vari, f_indep){
  f_fig3b_list <- list()
  nn <- 0
  for(ii in times_set){
    for(jj in strs_mo){
      nn <- nn + 1
      fc_1 <- (jj - 1) * len_sites + 1
      fc_2 <- jj * len_sites
      f_fig3b_list[[nn]] <- data.frame(
        xx = index_1[[f_indep]][fc_1:fc_2],
        yy = data2_2_mean[[f_sub]][[f_vari]][fc_1:fc_2,ii]
      )
    }
  }
  return(f_fig3b_list)
}

#==============================================
#up2024_0601_21:30

fig3b_plot_f <- function(f_data, f_index, f_vari, f_indep){
  f_time <- (f_index - 1)%/% len_strs_mo + 1
  f_str <- (f_index - 1)%% len_strs_mo + 1
  f_model <- lm(yy ~ xx, data = f_data)
  f_interc <- coef(f_model)[1]
  f_slope <- coef(f_model)[2]
  f_eq <- paste0('y = ', round(f_interc, 2), ' + ', round(f_slope, 2), ' * x')
  
  f_p <- ggplot(f_data, aes(x = xx, y = yy)) +
    geom_point() +
    geom_smooth(method = 'lm', se = FALSE, col = 'blue') +
    annotate('text', x = Inf, y = Inf, label = f_eq, hjust = 1.1, vjust = 1.5, size = 5, color = 'red') +
    ggtitle(paste('time:', f_time,'_str:',f_str)) +
    xlab(colnames(index_1)[f_indep]) + 
    ylab(f_vari) + 
    theme_minimal()
  
  return(f_p)
}

#==============================================
#up2024_0601_21:30

fig3b_res_f <- function(f_sub_name, f_vari, f_indep){
  cat(f_sub_name, '_', f_vari, '\n')
  f_data_fig3b_list_1 <- fig3b_list_f(f_sub_name, f_vari, f_indep)
  f_plots_fig3b_1 <- lapply(seq_along(f_data_fig3b_list_1), function(nn) fig3b_plot_f(f_data_fig3b_list_1[[nn]], nn, f_vari, f_indep))
  f_comb_plots_fig3b_1 <- grid.arrange(grobs = f_plots_fig3b_1, ncol = len_strs_mo, nrow = len_times_set)
  ggsave(paste0('FIG2/comb_plots_fig3b_1', f_sub_name, '_', f_vari, '_', f_indep,'.jpg'), plot = f_comb_plots_fig3b_1, width = 15, height = 15, dpi = 300)
  return(f_comb_plots_fig3b_1)
}

#fig3b_res <- list()
#for(c_sub_name in subs_name){
#  fig3b_res[[c_sub_name]] <- list()
#  for(c_vari in varis){
#    fig3b_res[[c_sub_name]][[c_vari]] <- fig3b_res_f(c_sub_name, c_vari, indep_set)
#  }
#}

check_3 <- fig3b_res_f('ORI', 'TP', 10)
#==============================================
#up2024_0618_08:00

fig3c_list_f <- function(f_sub, f_vari, f_time, f_indep){
  f_fig3c_list <- list()
  nn <- 0
  for(jj in strs_mo){
    for(pp in 1:bydis_num){
      nn <- nn + 1
      fc_1 <- (jj - 1) * len_sites + (pp - 1) * bydis_itv + 1
      fc_2 <- (jj - 1) * len_sites + pp * bydis_itv
      f_fig3c_list[[nn]] <- data.frame(
        xx = index_1[[f_indep]][fc_1:fc_2],
        yy = data2_2_mean[[f_sub]][[f_vari]][fc_1:fc_2,f_time]
      )
    }
  }
  return(f_fig3c_list)
}

#==============================================
#up2024_0618_08:00

fig3c_plot_f <- function(f_data, f_index, f_vari, f_indep){
  f_str <- (f_index - 1)%/% bydis_num + 1
  f_bydis <- (f_index - 1)%% bydis_num + 1
  f_model <- lm(yy ~ xx, data = f_data)
  f_interc <- coef(f_model)[1]
  f_slope <- coef(f_model)[2]
  f_eq <- paste0('y = ', round(f_interc, 2), ' + ', round(f_slope, 2), ' * x')
  
  f_p <- ggplot(f_data, aes(x = xx, y = yy)) +
    geom_point() +
    geom_smooth(method = 'lm', se = FALSE, col = 'blue') +
    annotate('text', x = Inf, y = Inf, label = f_eq, hjust = 1.1, vjust = 1.5, size = 5, color = 'red') +
    ggtitle(paste('str:',f_str,'_bydis', f_bydis)) +
    xlab(colnames(index_1)[f_indep]) + 
    ylab(f_vari) + 
    theme_minimal()
  
  return(f_p)
}

#==============================================
#up2024_0618_08:00

fig3c_res_f <- function(f_sub_name, f_vari, f_time, f_indep){
  cat(f_sub_name, '_', f_vari, '\n')
  f_data_fig3c_list_1 <- fig3c_list_f(f_sub_name, f_vari, f_time, f_indep)
  f_plots_fig3c_1 <- lapply(seq_along(f_data_fig3c_list_1), function(nn) fig3c_plot_f(f_data_fig3c_list_1[[nn]], nn, f_vari, f_indep))
  f_comb_plots_fig3c_1 <- grid.arrange(grobs = f_plots_fig3c_1, ncol = bydis_num, nrow = len_strs_mo)
  ggsave(paste0('FIG2/comb_plots_fig3c_1', f_sub_name, '_', f_vari, '_', f_time, '_', f_indep,'.jpg'), plot = f_comb_plots_fig3c_1, width = 15, height = 15, dpi = 300)
  return(f_comb_plots_fig3c_1)
}

#fig3c_res <- list()
#for(c_sub_name in subs_name){
#  fig3c_res[[c_sub_name]] <- list()
#  for(c_vari in varis){
#    for(ii in times_set){
#      fig3c_res[[c_sub_name]][[c_vari]][[ii]] <- fig3c_res_f(c_sub_name, c_vari, ii, indep_set)
#    }
#  }
#}

check_3c <- fig3c_res_f('ORI', 'TP', 1, 20)

#==============================================
#up2024_0601_21:30

fig4b_list_f <- function(f_sub, f_indep){
  f_fig4b_list <- list()
  nn <- 0
  for(c_vari in varis){
    for(ii in times_set){
      nn <- nn + 1
      f_fig4b_list[[nn]] <- data.frame(
        xx = index_1[[f_indep]],
        yy = data2_2_mean[[f_sub]][[c_vari]][,ii]
      )
    }
  }
  return(f_fig4b_list)
}

#==============================================
#up2024_0601_21:30

fig4b_plot_f <- function(f_data, f_index, f_indep){
  f_vari <- (f_index - 1) %/% len_times_set + 1
  f_time <- (f_index - 1) %% len_times_set + 1
  f_model <- lm(yy ~ xx, data = f_data)
  f_interc <- coef(f_model)[1]
  f_slope <- coef(f_model)[2]
  f_eq <- paste0('y = ', round(f_interc, 2), ' + ', round(f_slope, 2), ' * x')
  
  f_p <- ggplot(f_data, aes(x = xx, y = yy)) +
    geom_point() +
    geom_smooth(method = 'lm', se = FALSE, col = 'blue') +
    annotate('text', x = Inf, y = Inf, label = f_eq, hjust = 1.1, vjust = 1.5, size = 5, color = 'red') +
    ggtitle(paste('vari:', varis[f_vari],'_time:',f_time)) +
    xlab(colnames(index_1)[f_indep]) + 
    ylab(f_vari) + 
    theme_minimal()
  
  return(f_p)
}

#==============================================
#up2024_0602_11:30

fig4b_res_f <- function(f_sub_name, f_indep){
  cat(f_sub_name, '\n')
  f_data_fig4b_list_1 <- fig4b_list_f(f_sub_name, f_indep)
  f_plots_fig4b_1 <- lapply(seq_along(f_data_fig4b_list_1), function(nn) fig4b_plot_f(f_data_fig4b_list_1[[nn]], nn, f_indep))
  f_comb_plots_fig4b_1 <- grid.arrange(grobs = f_plots_fig4b_1, ncol = len_times_set, nrow = len_varis)
  ggsave(paste0('FIG2/comb_plots_fig4b_1', f_sub_name, '_', f_indep,'.jpg'), plot = f_comb_plots_fig4b_1, width = 15, height = 15, dpi = 300)  
  return(f_comb_plots_fig4b_1)
}

#fig4b_res <- list()
#for(c_sub_name in subs_name){
#  fig4b_res[[c_sub_name]] <- fig4b_res_f(c_sub_name, indep_set)
#}

#==============================================
#check

check_4 <- fig4b_res_f('ORI', indep_set)

#=====================
#up2024_0618_08:00

fig4c_list_f <- function(f_sub, f_vari, f_indep){
  f_fig4c_list <- list()
  nn <- 0
  for(ii in times_set){
    for(pp in 1: bydis_num){
      nn <- nn + 1
      f_fig4c_list[[nn]] <- data.frame(
        xx = index_1[[f_indep]][bydis[[pp]]],
        yy = data2_2_mean[[f_sub]][[f_vari]][bydis[[pp]],ii]
      )
    }
  }
  return(f_fig4c_list)
}
#==============================================
#up2024_0618_08:00

fig4c_plot_f <- function(f_data, f_index, f_vari, f_indep){
  f_time <- (f_index - 1) %/% bydis_num + 1
  f_bydis <- (f_index - 1) %% bydis_num + 1
  f_model <- lm(yy ~ xx, data = f_data)
  f_interc <- coef(f_model)[1]
  f_slope <- coef(f_model)[2]
  f_eq <- paste0('y = ', round(f_interc, 2), ' + ', round(f_slope, 2), ' * x')
  
  f_p <- ggplot(f_data, aes(x = xx, y = yy)) +
    geom_point() +
    geom_smooth(method = 'lm', se = FALSE, col = 'blue') +
    annotate('text', x = Inf, y = Inf, label = f_eq, hjust = 1.1, vjust = 1.5, size = 5, color = 'red') +
    ggtitle(paste('time:',f_time,'_dis', f_bydis)) +
    xlab(colnames(index_1)[f_indep]) + 
    ylab(f_vari) + 
    theme_minimal()
  
  return(f_p)
}

#==============================================
#up2024_0618_08:00

fig4c_res_f <- function(f_sub, f_vari, f_indep){
  cat(f_sub, '\n')
  f_data_fig4c_list_1 <- fig4c_list_f(f_sub, f_vari, f_indep)
  f_plots_fig4c_1 <- lapply(seq_along(f_data_fig4c_list_1), function(nn) fig4c_plot_f(f_data_fig4c_list_1[[nn]], nn, f_vari, f_indep))
  f_comb_plots_fig4c_1 <- grid.arrange(grobs = f_plots_fig4c_1, ncol = bydis_num, nrow = len_times_set)
  ggsave(paste0('FIG2/comb_plots_fig4c_1', f_sub, '_', f_vari, '_', f_indep,'.jpg'), plot = f_comb_plots_fig4c_1, width = 15, height = 15, dpi = 300)  
  return(f_comb_plots_fig4c_1)
}

#fig4c_res <- list()
#for(c_sub_name in subs_name){
#  fig4c_res[[c_sub_name]] <- list()
#  for(c_vari in varis){
#    fig4c_res[[c_sub_name]][[c_vari]] <- fig4c_res_f(c_sub_name, c_vari, indep_set)
#  }
#}

check_4c <- fig4c_res_f('ORI', 'TP', 20)
#=====================
#up2024_0617 11:05

fig5b_list_f <- function(f_sub, f_vari, f_indep){
  f_days_len <- length(days_trans_f(f_sub))
  f_fig5b_list <- list()
  nn <- 0
  for(ii in times_set){
    for(jj in strs_mo){
      nn <- nn + 1
      f_1 <- (jj - 1) * len_sites + 1
      f_2 <- jj * len_sites
      f_fig5b_list[[nn]] <- data.frame(
        xx = rep(index_1s[[jj]][[f_indep]], f_days_len), 
        yy = as.vector(data2_2_ori[[f_vari]][[ii]][f_1:f_2,days_trans_f(f_sub)])
      )
    }
  }
  return(f_fig5b_list)
}

#==============================================
#up2024_0617 11:05

fig5b_plot_f <- function(f_data, f_index, f_vari, f_indep){
  f_time <- (f_index - 1)%/% len_strs_mo + 1
  f_str <- (f_index - 1)%% len_strs_mo + 1
  f_model <- lm(yy ~ xx, data = f_data)
  f_interc <- coef(f_model)[1]
  f_slope <- coef(f_model)[2]
  f_eq <- paste0('y = ', round(f_interc, 2), ' + ', round(f_slope, 2), ' * x')
  
  f_p <- ggplot(f_data, aes(x = xx, y = yy)) +
    geom_point() +
    geom_smooth(method = 'lm', se = FALSE, col = 'blue') +
    annotate('text', x = Inf, y = Inf, label = f_eq, hjust = 1.1, vjust = 1.5, size = 5, color = 'red') +
    ggtitle(paste('time:', f_time,'_str:',f_str)) +
    xlab(colnames(index_1)[f_indep]) + 
    ylab(f_vari) + 
    theme_minimal()
  
  return(f_p)
}

#==============================================
#up2024_0617 11:05

fig5b_res_f <- function(f_sub_name, f_vari, f_indep){
  cat(f_sub_name, '_', f_vari, '\n')
  f_data_fig5b_list_1 <- fig5b_list_f(f_sub_name, f_vari, f_indep)
  f_plots_fig5b_1 <- lapply(seq_along(f_data_fig5b_list_1), function(nn) fig5b_plot_f(f_data_fig5b_list_1[[nn]], nn, f_vari, f_indep))
  f_comb_plots_fig5b_1 <- grid.arrange(grobs = f_plots_fig5b_1, ncol = len_strs_mo, nrow = len_times_set)
  ggsave(paste0('FIG2/comb_plots_fig5b_1', f_sub_name, '_', f_vari, '_', f_indep,'.jpg'), plot = f_comb_plots_fig5b_1, width = 15, height = 15, dpi = 300)
  return(f_comb_plots_fig5b_1)
}

#fig5b_res <- list()
#for(c_sub_name in subs_name){
#  fig5b_res[[c_sub_name]] <- list()
#  for(c_vari in varis){
#    fig5b_res[[c_sub_name]][[c_vari]] <- fig5b_res_f(c_sub_name, c_vari, indep_set)
#  }
#}

check_5 <- fig5b_res_f('ORI', 'TP', 10)

#==============================================
#up2024_0617 11:05

fig6b_list_f <- function(f_sub, f_indep){
  f_days_len <- length(days_trans_f(f_sub))
  f_fig6b_list <- list()
  nn <- 0
  for(c_vari in varis){
    for(ii in times_set){
      nn <- nn + 1
      f_fig6b_list[[nn]] <- data.frame(
        xx = rep(index_1[[f_indep]], f_days_len),
        yy = as.vector(data2_2_ori[[c_vari]][[ii]][,days_trans_f(f_sub)])
      )
    }
  }
  return(f_fig6b_list)
}

#==============================================
#up2024_0617 11:05

fig6b_plot_f <- function(f_data, f_index, f_indep){
  f_vari <- (f_index - 1) %/% len_times_set + 1
  f_time <- (f_index - 1) %% len_times_set + 1
  f_model <- lm(yy ~ xx, data = f_data)
  f_interc <- coef(f_model)[1]
  f_slope <- coef(f_model)[2]
  f_eq <- paste0('y = ', round(f_interc, 2), ' + ', round(f_slope, 2), ' * x')
  
  f_p <- ggplot(f_data, aes(x = xx, y = yy)) +
    geom_point() +
    geom_smooth(method = 'lm', se = FALSE, col = 'blue') +
    annotate('text', x = Inf, y = Inf, label = f_eq, hjust = 1.1, vjust = 1.5, size = 5, color = 'red') +
    ggtitle(paste('vari:', varis[f_vari],'_time:',f_time)) +
    xlab(colnames(index_1)[f_indep]) + 
    ylab(f_vari) + 
    theme_minimal()
  
  return(f_p)
}

#==============================================
#up2024_0617 11:05

fig6b_res_f <- function(f_sub_name, f_indep){
  cat(f_sub_name, '\n')
  f_data_fig6b_list_1 <- fig6b_list_f(f_sub_name, f_indep)
  f_plots_fig6b_1 <- lapply(seq_along(f_data_fig6b_list_1), function(nn) fig6b_plot_f(f_data_fig6b_list_1[[nn]], nn, f_indep))
  f_comb_plots_fig6b_1 <- grid.arrange(grobs = f_plots_fig6b_1, ncol = len_times_set, nrow = len_varis)
  ggsave(paste0('FIG2/comb_plots_fig6b_1', f_sub_name, '_', f_indep,'.jpg'), plot = f_comb_plots_fig6b_1, width = 15, height = 15, dpi = 300)  
  return(f_comb_plots_fig6b_1)
}

#fig6b_res <- list()
#for(c_sub_name in subs_name){
#  fig6b_res[[c_sub_name]] <- fig6b_res_f(c_sub_name, indep_set)
#}

check_6 <- fig6b_res_f('ORI', 10)
#==============================================
#up2024_0618 08:00

fig6c_list_f <- function(f_sub, f_vari, f_indep){
  f_fig6c_list <- list()
  nn <- 0
  for(ii in times_set){
    for(pp in 1: bydis_num){
      nn <- nn + 1
      c_bydis <- bydis[[pp]]
      f_fig6c_list[[nn]] <- data.frame(
        xx = rep(index_1[[f_indep]][c_bydis], length(days_trans_f(f_sub))),
        yy = as.vector(data2_2_ori[[f_vari]][[ii]][c_bydis,days_trans_f(f_sub)])
      )
    }
  }
  return(f_fig6c_list)
}

#==============================================
#up2024_0618 08:00

fig6c_plot_f <- function(f_data, f_index, f_vari, f_indep){
  f_time <- (f_index - 1) %/% bydis_num + 1
  f_bydis <- (f_index - 1) %% bydis_num + 1
  f_model <- lm(yy ~ xx, data = f_data)
  f_interc <- coef(f_model)[1]
  f_slope <- coef(f_model)[2]
  f_eq <- paste0('y = ', round(f_interc, 2), ' + ', round(f_slope, 2), ' * x')
  
  f_p <- ggplot(f_data, aes(x = xx, y = yy)) +
    geom_point() +
    geom_smooth(method = 'lm', se = FALSE, col = 'blue') +
    annotate('text', x = Inf, y = Inf, label = f_eq, hjust = 1.1, vjust = 1.5, size = 5, color = 'red') +
    ggtitle(paste('time:',f_time, '_bydis', f_bydis)) +
    xlab(colnames(index_1)[f_indep]) + 
    ylab(f_vari) + 
    theme_minimal()
  
  return(f_p)
}

#==============================================
#up2024_0618 08:00

fig6c_res_f <- function(f_sub, f_vari, f_indep){
  cat(f_sub, '\n')
  f_data_fig6c_list_1 <- fig6c_list_f(f_sub, f_vari, f_indep)
  f_plots_fig6c_1 <- lapply(seq_along(f_data_fig6c_list_1), function(nn) fig6c_plot_f(f_data_fig6c_list_1[[nn]], nn, f_vari, f_indep))
  f_comb_plots_fig6c_1 <- grid.arrange(grobs = f_plots_fig6c_1, ncol = bydis_num, nrow = len_times_set)
  ggsave(paste0('FIG2/comb_plots_fig6c_1', f_sub, '_', f_indep,'.jpg'), plot = f_comb_plots_fig6c_1, width = 15, height = 15, dpi = 300)  
  return(f_comb_plots_fig6c_1)
}

#fig6c_res <- list()
#for(c_sub_name in subs_name){
#  fig6c_res[[c_sub_name]] <- list()
#  for(c_vari in varis){
#    fig6c_res[[c_sub_name]][[c_vari]] <- fig6c_res_f(c_sub_name, c_vari, indep_set)
#  }
#}

check_6c <- fig6c_res_f('ORI', 'TP', 20)