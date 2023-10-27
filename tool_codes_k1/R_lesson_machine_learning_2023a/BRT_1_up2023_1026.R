
library(MASS)
library(gbm)

data(Boston)
str(Boston)

set.seed(123)
split <- sample(nrow(Boston), nrow(Boston) * 0.7)
train <- Boston[split, ]
test <- Boston[-split, ]

set.seed(123)
fit_1 <- gbm(medv~., data = train, verbose = TRUE, shrinkage = 0.01, interaction.depth = 3, 
             n.minobsinnode = 5, n.trees = 5000, cv.folds = 10)

print(fit_1) 
summary(fit_1)

perf_gbm1 = gbm.perf(fit_1, method = "cv")
perf_gbm1

predict1 <- predict(fit_1, test, perf_gbm1) #生成预测数据
plot(test$medv, predict1, main = 'Test dataset', xlab = 'original data', ylab = 'Predicted data')
#将原数据值和预测值作散点图
abline(1, 1)

plot.gbm(fit_1, i.var = 1) #生成第1个变量crim的偏依赖图
plot.gbm(fit_1, i.var = c(1,5)) #生成第1个和第5个变量的偏依赖图
