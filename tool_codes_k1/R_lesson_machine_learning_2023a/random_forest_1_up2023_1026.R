#refer to: https://www.bilibili.com/video/BV1tV41117cU/?spm_id_from=333.337.search-card.all.click&vd_source=5ead28bbf00e6798e790cb439bf6f631

library(pacman)
p_load(randomForest, caret, pROC)

setwd('E:/zyf_gy/zyf_gy_2304_work/tool_codes_k1/R_lesson_machine_learning_2023a')

#input data
data('iris')
summary(iris)
#dim(iris)

#seperate data into trainset and testset
trainlist <- createDataPartition(iris$Species, p = 0.8, list = FALSE) #list: 逻辑值。true时，返回结果为列表形式，否则，为floor(p * length(y))行 times列的矩阵
trainset <- iris[trainlist,]
testset <- iris[-trainlist,]
#dim(trainset)
#dim(testset)

#=================================================================

#set model
set.seed(1)
rf.train <- randomForest(as.factor(Species) ~., data = trainset, importance = TRUE, na.action = na.pass)
#importance:显示变量的重要性排序; na.action = na.pass: 略过缺失值
rf.train
plot(rf.train, main = 'randomforest origin');

#predict
set.seed(2)
rf.test <- predict(rf.train, newdata = testset, type = 'class') #该分析预测种类，因此type为class
rf.test

#结果统计
rf.cf <- confusionMatrix(as.factor(rf.test), as.factor(testset$Species)) 
#as.factor(rf.test)：预测结果；as.factor(testset$Species)：原结果
rf.cf 
#结果需查看：Confusion Matrix, Reference表格, Accuracy, Kappa(值越接近1，效果越好), 95% CI, Balanced Accuracy

#ROC曲线和AUC值
rf.test2 <- predict(rf.train, newdata = testset, type = 'prob');
head(rf.test2)
roc.rf <- multiclass.roc(testset$Species, rf.test2)
roc.rf #值越接近1越好
