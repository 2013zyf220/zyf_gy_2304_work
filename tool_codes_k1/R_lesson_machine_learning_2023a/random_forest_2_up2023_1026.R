library(randomForest)
library(skimr)
library(DataExplorer)
library(tidyverse)
library(caret)
library(pROC)

boston <- read.csv('E:/zyf_gy/zyf_gy_2304_work/tool_codes_k1/R_lesson_machine_learning_2023a/randon_forest_2/data-Boston4Reg.csv')
#as.data.frame()
skim(boston) #鸟瞰数据
plot_missing(boston) #数据缺失状况
#set_missing() #基于指定值填充
#na.roughfix() #基于代表值（如中位数、众数）填充
boston$chas <- factor(boston$chas) #转换为类型变量
skim(boston) #再次鸟瞰数据
hist(boston$medv, breaks = 50) #对因变量做直方图

#=====================================

#分类数据
set.seed(1)
trains <- createDataPartition(y = boston$medv, p = 0.75, list = F); #基于因变量拆分
traindata <- boston[trains, ]
testdata <- boston[-trains, ]
hist(traindata$medv, breaks = 50)
hist(testdata$medv, breaks = 50)
colnames(boston) #get the column names of boston

#构建公式
form_reg <- as.formula(paste0('medv ~', paste(colnames(traindata)[1:13], collapse = ' + '))) #as.formula(): 将字符串转换成公式
form_reg

#modelling
set.seed(2)
fit_rf_reg <- randomForest(form_reg, data = traindata, ntree = 500, mtry = 6, importance = T) 
#ntree:决策树棵树，比较大就可以. mtry: 每个节点可供选择的变量数目，2到10之间就可以
fit_rf_reg #模型概要
plot(fit_rf_reg, main = "树的棵树与袋外MSE") # ntree参数与error之间的关系图示

# 变量重要性图示，默认最多显示30个变量
importance(fit_rf_reg) #变量重要性
varImpPlot(fit_rf_reg, main = "随机森林变量重要性")
#varImpPlot(fit_rf_reg, main = "随机森林变量重要性", type = 1) #只显示左边
#varImpPlot(fit_rf_reg, main = "随机森林变量重要性", type = 2) #只显示右边

# 偏依赖图: 因变量随特定自变量变化的变化
partialPlot(x = fit_rf_reg, pred.data = traindata, x.var = crim)  
#确保pred.data数据为data.frame, x.var: 某个自变量名称
plot(medv ~ crim, data = traindata) #对应散点图

#======================================
#预测

trainpred <- predict(object = fit_rf_reg, newdata = traindata) # 训练集预测结果
defaultSummary(data = data.frame(obs = traindata$medv, pred = trainpred)) # 训练集预测误差指标

plot(x = traindata$medv, y = trainpred, xlab = "Actual", ylab = "Prediction", 
     main = "随机森林—实际值与预测值比较", sub = "训练集") # 图示训练集预测结果

trainlinmod <- lm(trainpred ~ traindata$medv)
abline(trainlinmod, col = 'blue', lwd = 2.5, lty = 'solid')
abline(a = 0, b = 1, col = 'red', lwd = 2.5, lty = 'dashed')
legend('topleft', legend = c('Model', 'Base'), col = c('blue', 'red'), lwd = 2.5, lty = c('solid','dashed'))

#======================================
#训练集预测结果的说服力不足，因而需要测试集预测

testpred <- predict(fit_rf_reg, newdata = testdata)  #测试集预测结果
defaultSummary(data.frame(obs = testdata$medv, pred = testpred))  #测试集预测误差指标

plot(x = testdata$medv, y = testpred, xlab = "Actual", ylab = "Prediction", 
     main = "随机森林——实际值与预测值比较", sub = "测试集") # 图示测试集预测结果
testlinmod <- lm(testpred ~testdata$medv)
abline(testlinmod, col = 'blue', lwd = 2.5, lty = 'solid')
abline(a = 0, b = 1, col = 'red', lwd = 2.5, lty = 'dashed')
legend('topleft', legend = c('Model', 'Base'), col = c('blue', 'red'), lwd = 2.5, lty = c('solid','dashed'))

#=================================

predresult <- data.frame(obs = c(traindata$medv, testdata$medv), pred = c(trainpred, testpred),
             group = c(rep('Train', length(trainpred)), rep('Test', length(testpred))))

ggplot(predresult, aes(x = obs, y = pred, fill = group, colour = group)) +
  geom_point(shape = 21, size = 3) +
  geom_smooth(method = 'lm', se = F, size = 1.2) + 
  geom_abline(intercept = 0, slope = 1, size = 1.2) +
  labs(fill = NULL, colour = NULL) +
  theme(legend.position = 'bottom')

