
array_len <- 50 * 2 * 6 # 设置数组的总长度

num_1 <- sample(120:150, 1)  # 随机选择1的个数（10到15之间）
num_2 <- sample(120:150, 1)  
num_zeros <- array_len - num_1 - num_2 # 计算0的个数

array_1 <- c(rep(0.1, num_1), rep(0, num_zeros), rep(-0.1, num_2))  # 创建一个包含num_ones个1和num_zeros个0的数组
array_2 <- sample(array_1) # 随机打乱数组
array_3 <- matrix(array_2, nrow = 100, ncol = 6, byrow = TRUE)

print(array_3)
df_1 <- data.frame(array_3)

# 导出为CSV文件
write.csv(df_1, file = "E:/zyf_gn/zyf_gn_2301_data/ppa_2302_k2/ARCGIS/random/random_array_1.csv", row.names = FALSE) #to_be_set

