


# 创建一个包含100个随机整数的数组，范围在-100到100之间
random_numbers <- sample(-100:100, 100, replace = TRUE)

# 输出结果
print(random_numbers)

write.csv(random_numbers, file = "E:/zyf_gn/zyf_gn_2301_data/ppa_2302_k2/ARCGIS/random/random_array_1.csv", row.names = FALSE) #to_be_set