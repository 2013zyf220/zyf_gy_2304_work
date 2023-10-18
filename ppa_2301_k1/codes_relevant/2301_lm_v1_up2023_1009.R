library(PerformanceAnalytics)
library(car)

#input data
setwd('E:/zyf_gy/zyf_gy_2304_work/ppa_2301_k1/codes_relevant')
data_1 <- read.csv('housing_data.csv')
data_2 = data_1[,c(2,3,4,5,6)]

#correlation table
cor_1 <- cor(data_2) 

#plot data
plot(data_2)
chart.Correlation(data_2, method = 'pearson', pch = 19, col = 'blue', tl.cex = 1.2)

#build multiple linear model
model = lm(log(price) ~ sqft.living + bedrooms + bathrooms, data = data_2)
model

#回归诊断
par(mfrow = c(2,2))
plot(model)
hist(log(data_2$price))

#解释beta
par(mfrow = c(1,1))
bathrooms <- 1:10
bedrooms <- rep(mean(data_2$bedrooms), 10) 
sqft.living <- rep(mean(data_2$sqft.living), 10)
dat.new <- data.frame(bedrooms, bathrooms, sqft.living)
plot(bathrooms, exp(predict(model, newdata = dat.new)), type='l', lwd=3, ylab='price', xlab='bathrooms', cex.lab=2)

#研究X变量是否都显著
model_sum <- summary(model)
model_anova <- anova(model)
