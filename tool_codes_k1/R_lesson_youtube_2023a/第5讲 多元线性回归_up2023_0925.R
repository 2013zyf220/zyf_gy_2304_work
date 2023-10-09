library(PerformanceAnalytics)
library(car)

#input data
setwd("E:/zyf_gy/zyf_gy_2304_work/tool_codes_k1/R_lesson_youtube_2023a")
data_1 <- read.csv("housing_data.csv")
data_2 = data_1[,-1]

#correlation table
cor_1 <- cor(data_2) 

#plot data
plot(data_2)
chart.Correlation(data_2, method = "pearson", pch = 19, col = "blue", tl.cex = 1.2)

#build multiple linear model
model = lm(log(price)~sqft.living+bedrooms+bathrooms,data = data_2)
model

#回归诊断
par(mfrow=c(2,2))
plot(model)
hist(log(data_2$price))

#解释beta
par(mfrow=c(1,1))
bathrooms <- 1:10
bedrooms <- rep(mean(data_2$bedrooms), 10) 
sqft.living <- rep(mean(data_2$sqft.living), 10)
dat.new <- data.frame(bedrooms, bathrooms, sqft.living)
plot(bathrooms, exp(predict(model, newdata = dat.new)), type="l", lwd=3, ylab="price", xlab="bathrooms", cex.lab=2)
#type="l": specifies the type of plot - line plot. lwd=3: sets the line width for the plot to 3.
#ylab="price": sets the label for the y-axis to "price. "xlab="bedrooms": sets the label for the x-axis to "bathrooms"
#cex.lab=2: adjusts the size of the axis labels.

#研究X变量是否都显著
model_sum <- summary(model)
model_anova <- anova(model)

#overall F test:
MSR = (model_anova['sqft.living','Sum Sq'] + model_anova['bedrooms','Sum Sq'] + model_anova['bathrooms','Sum Sq'])/3 #to_be_set
MSE = model_anova['Residuals','Sum Sq'] / model_anova['Residuals','Df']
F = MSR/MSE
pv2 <- 1 - pf(F,df1 = 3,df2 = model_anova['Residuals','Df']) #to_be_set

#预测
x_1 = data.frame(sqft.living=1000, bedrooms=3, bathrooms=3)
#exp(predict(model,x_1)) #点估计
exp(predict(model,x_1,interval="confidence", level =0.95)) #区间估计

#处理多重共线性-方法1：pearson 相关系数 
cor_1 <- cor(data_2)

#处理多重共线性-方法2：VIF 方差膨胀系数(variance inflation factor)
partial_regrs =lm(sqft.living ~ bedrooms + bathrooms,data = data_2 )
pr_sum <- summary(partial_regrs)

VIF_1 <- 1/(1 - pr_sum$r.squared)
VIF_2 <- vif(model)

#偏残差图-方法1
par(mfrow=c(1,3))
#判断X变量和Y的线性关系
plot(data_2$bedrooms,log(data_2$price))
plot(data_2$bathrooms,log(data_2$price))
plot(data_2$sqft.living,log(data_2$price))
#判断在bedrooms变量和bathrooms存在的情况下，sqft.living和log(price)是否呈线性关系
par(mfrow=c(1,1))
#sqft.living为X轴，不含sqft.living变量拟合出回归模型的残差为Y轴，做散点图
partial.model = lm(log(price)~bathrooms+bedrooms,data=data_2)
plot(data_2$sqft.living,partial.model$residuals)

#偏残差图-方法2
model = lm(log(price)~sqft.living+bedrooms,data = data_2)
crPlots(model)

