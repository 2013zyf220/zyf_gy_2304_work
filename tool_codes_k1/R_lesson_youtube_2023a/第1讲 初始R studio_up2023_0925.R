#read data
student_data <- read.csv('E:/zyf_gy/zyf_gy_2304_work/tool_codes_k1/R_lesson_youtube_2023a/student_data.csv')
data_1 <- student_data[, c(2,3,4)]
#data_1 <- student_data[,-1]
#View(data_1)

data_1_tail <- tail(data_1,10) #get the last 10 lines of 'data_1'
data_1_sum <- summary(data_1)

data_2 = na.omit(data_1) #delete rows with NA values
data_2_sum <- summary(data_2)

hist(data_2$height, col = 'red') #对身高进行直方图可视化 histogram
plot(data_2$height, data_2$weight, main = 'scatter plot', col = 'red') #用散点图可视化身高和体重的关系
boxplot(data_2$height, main = 'boxplot of height') #对身高进行箱线图可视化
boxplot(data_2$height ~ data_2$gender, main = 'boxplot of height by gender') #对男女生身高分别进行箱线图可视化

