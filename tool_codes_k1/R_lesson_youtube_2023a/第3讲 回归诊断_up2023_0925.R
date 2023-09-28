#数据预处理
student_data <- read.csv("E:/zyf_gy/zyf_gy_2304_work/tool_codes_k1/R_lesson_youtube_2023a/student_data.csv")
data <- student_data[, c(2,3,4)]
lm_data = na.omit(data)

#散点图可视化身高体重线性关系

plot(lm_data$height, lm_data$weight ,main="scatter plot")



#最小二乘法拟合简单线性回归
model =lm(log(weight)~height,data=lm_data) #to_be_set
#model =lm(weight~height,data=lm_data) #to_be_set
abline(model,col="red")

#residual plot 残差图
par(mfrow = c(1,2)) #make multiple figures in a page

#method 1 of residual plot
plot(model$fitted.values,model$residuals)
abline(h = 0, col = "red")

#method 2 of residual plot
plot(model,which = 1)

#Scale-Location Graph 位置尺度图

#Scale-Location Graph: method 1
plot(model$fitted.values, sqrt(abs(scale(model$residuals))))

#Scale-Location Graph: method 2
plot(model, which = 3)

#QQ图
hist(lm_data$weight)
hist(log(lm_data$weight))

qqnorm(lm_data$weight)
qqline(lm_data$weight)
plot(model,which = 2)


#异常点处理
plot(model,which = 4)
plot(model,which = 5)


