


# 创建一个包含100个随机整数的数组，范围在-100到100之间
random_numbers <- sample(-100:100, 7200, replace = TRUE)

random_numbers2 <- matrix(random_numbers, nrow = 360, ncol = 20)
# 输出结果
print(random_numbers2)

write.csv(random_numbers2, file = "E:/zyf_gn/zyf_gn_2301_data/ppa_2302_k2/ARCGIS/random/random_array_7.csv", row.names = FALSE) #to_be_set

#========================


random2 <- matrix(sample(c(-1, 0, 1), size = 3600, replace = TRUE), nrow = 360, ncol = 10)
write.csv(random2, file = "E:/zyf_gn/zyf_gn_2301_data/ppa_2302_k2/ARCGIS/random/random2_array_1.csv", row.names = FALSE) #to_be_set