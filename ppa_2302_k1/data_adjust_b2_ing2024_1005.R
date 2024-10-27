library(MASS)
library(skimr)
library(DataExplorer)
library(tidyverse)
library(caret)
library(pROC)
library(ggplot2)
library(readxl)
library(gridExtra)
library(openxlsx)

setwd('E:/zyf_gn/zyf_gn_2301_data/ppa_2302_k2/ARCGIS')
#========================
basic_1 <- list()
basic_1[['TP']] <- list()
basic_1[['RH']] <- list()

basic_1[['TP']][[1]] <-  c(35.1, 36.55, 36.7, 33.05, 35.1, 28.7)
basic_1[['TP']][[2]] <-  c(35.1, 36.55, 36.7, 33.05, 35.1, 28.7)
basic_1[['TP']][[3]] <-  c(33, 33.35,33.65,29.1,32.9,27.3)
basic_1[['RH']][[1]] <-  c(43.55,41.2,44.1,60.55,47.15,63.45)
basic_1[['RH']][[2]] <-  c(43.55,41.2,44.1,60.55,47.15,63.45)
basic_1[['RH']][[3]] <-  c(49.8,54.8,51.75,76,42.4,63.15)
#========================
basic_2 <- list()
basic_2[['TP']] <- list()
basic_2[['RH']] <- list()

basic_2[['TP']][[1]] <- matrix(rep(basic_1[['TP']][[1]], 300), nrow = 300, byrow = TRUE)
basic_2[['TP']][[2]] <- matrix(rep(basic_1[['TP']][[2]], 300), nrow = 300, byrow = TRUE)
basic_2[['TP']][[3]] <- matrix(rep(basic_1[['TP']][[3]], 300), nrow = 300, byrow = TRUE)
basic_2[['RH']][[1]] <- matrix(rep(basic_1[['RH']][[1]], 300), nrow = 300, byrow = TRUE)
basic_2[['RH']][[2]] <- matrix(rep(basic_1[['RH']][[2]], 300), nrow = 300, byrow = TRUE)
basic_2[['RH']][[3]] <- matrix(rep(basic_1[['RH']][[3]], 300), nrow = 300, byrow = TRUE)

basic_2b <- list()
basic_2b[['TP']] <- list()
basic_2b[['RH']] <- list()

basic_2b[['TP']][[1]] <- matrix(rep(basic_1[['TP']][[1]], 400), nrow = 400, byrow = TRUE)
basic_2b[['TP']][[2]] <- matrix(rep(basic_1[['TP']][[2]], 400), nrow = 400, byrow = TRUE)
basic_2b[['TP']][[3]] <- matrix(rep(basic_1[['TP']][[3]], 400), nrow = 400, byrow = TRUE)
basic_2b[['RH']][[1]] <- matrix(rep(basic_1[['RH']][[1]], 400), nrow = 400, byrow = TRUE)
basic_2b[['RH']][[2]] <- matrix(rep(basic_1[['RH']][[2]], 400), nrow = 400, byrow = TRUE)
basic_2b[['RH']][[3]] <- matrix(rep(basic_1[['RH']][[3]], 400), nrow = 400, byrow = TRUE)
#========================
basic_b1 <- list()
basic_b1[['TP']] <- list()
basic_b1[['RH']] <- list()

basic_b1[['TP']][[1]] <- as.matrix(read.csv('RES3/recb_2b_TP_1_df.csv'))
basic_b1[['TP']][[2]] <- as.matrix(read.csv('RES3/recb_2b_TP_2_df.csv'))
basic_b1[['TP']][[3]] <- as.matrix(read.csv('RES3/recb_2b_TP_3_df.csv'))
basic_b1[['RH']][[1]] <- as.matrix(read.csv('RES3/recb_2b_RH_1_df.csv'))
basic_b1[['RH']][[2]] <- as.matrix(read.csv('RES3/recb_2b_RH_2_df.csv'))
basic_b1[['RH']][[3]] <- as.matrix(read.csv('RES3/recb_2b_RH_3_df.csv'))

#========================
basic_c1 <- list()
basic_c1[['TP']] <- list()
basic_c1[['RH']] <- list()

basic_c1[['TP']][[1]] <- basic_b1[['TP']][[1]] - basic_2[['TP']][[1]]
basic_c1[['TP']][[2]] <- basic_b1[['TP']][[2]] - basic_2[['TP']][[2]]
basic_c1[['TP']][[3]] <- basic_b1[['TP']][[3]] - basic_2[['TP']][[3]]
basic_c1[['RH']][[1]] <- basic_b1[['RH']][[1]] - basic_2[['RH']][[1]]
basic_c1[['RH']][[2]] <- basic_b1[['RH']][[2]] - basic_2[['RH']][[2]]
basic_c1[['RH']][[3]] <- basic_b1[['RH']][[3]] - basic_2[['RH']][[3]]

basic_c0 <- matrix(0, nrow = 100, ncol = 6)
#========================
basic_c2 <- list()
basic_c2[['TP']] <- list()
basic_c2[['RH']] <- list()

basic_c2[['TP']][[1]] <- rbind(basic_c1[['TP']][[1]], basic_c0)
basic_c2[['TP']][[2]] <- rbind(basic_c1[['TP']][[2]], basic_c0)
basic_c2[['TP']][[3]] <- rbind(basic_c1[['TP']][[3]], basic_c0)
basic_c2[['RH']][[1]] <- rbind(basic_c1[['RH']][[1]], basic_c0)
basic_c2[['RH']][[2]] <- rbind(basic_c1[['RH']][[2]], basic_c0)
basic_c2[['RH']][[3]] <- rbind(basic_c1[['RH']][[3]], basic_c0)

#========================
basic_d1 <- list()
basic_d1[['TP']] <- list()
basic_d1[['RH']] <- list()

basic_d1[['TP']][[1]] <- basic_c2[['TP']][[1]] + basic_2b[['TP']][[1]]
basic_d1[['TP']][[2]] <- basic_c2[['TP']][[2]] + basic_2b[['TP']][[2]]
basic_d1[['TP']][[3]] <- basic_c2[['TP']][[3]] + basic_2b[['TP']][[3]]
basic_d1[['RH']][[1]] <- basic_c2[['RH']][[1]] + basic_2b[['RH']][[1]]
basic_d1[['RH']][[2]] <- basic_c2[['RH']][[2]] + basic_2b[['RH']][[2]]
basic_d1[['RH']][[3]] <- basic_c2[['RH']][[3]] + basic_2b[['RH']][[3]]

#==========================

write.csv(basic_d1[['TP']][[1]], 'RES2/recb_1_TP_time1.csv', row.names = FALSE)
write.csv(basic_d1[['TP']][[2]], 'RES2/recb_1_TP_time2.csv', row.names = FALSE)
write.csv(basic_d1[['TP']][[3]], 'RES2/recb_1_TP_time3.csv', row.names = FALSE)
write.csv(basic_d1[['RH']][[1]], 'RES2/recb_1_RH_time1.csv', row.names = FALSE)
write.csv(basic_d1[['RH']][[2]], 'RES2/recb_1_RH_time2.csv', row.names = FALSE)
write.csv(basic_d1[['RH']][[3]], 'RES2/recb_1_RH_time3.csv', row.names = FALSE)