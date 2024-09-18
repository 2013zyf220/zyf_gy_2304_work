data_1 <- as.matrix(read.csv('D:/zyf_gn/zyf_gn_2301_data/ppa_2302_k1/SVF_PHOTOS/SVF_DATA2.csv'))


data_2 <- matrix(0, ncol = 5, nrow = 60)
for(ii in 1:5){
  for(jj in 1:6){
    c_1 <- (jj - 1) * 50 + (ii - 1) * 10 + 1
    c_2 <- (jj - 1) * 50 + ii * 10
    c_3 <- (jj - 1) * 10 + 1
    c_4 <- jj * 10
    data_2[c_3: c_4, ii] <- data_1[c_1: c_2]
  }
}

data_3 <- data_2[rep(1:nrow(data_2), times = 6), ]

write.csv(data_3, 'D:/zyf_gn/zyf_gn_2301_data/ppa_2302_k1/SVF_PHOTOS/SVF_DATA2_ing.csv', row.names = FALSE)
