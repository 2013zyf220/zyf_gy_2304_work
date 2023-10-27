library(leaps)

#load and check data
data(swiss)
str(swiss)

#全子集回归
r_1 <- regsubsets(Fertility ~ Agriculture + Examination + Education + Catholic + Infant.Mortality, data = swiss, nbest = 6, method = 'exhaustive') #to_be_set
plot(r_1, scale = 'adjr2')
r_2 <- summary(r_1)
r_3 <- cbind(r_2$outmat, r_2$adjr2, r_2$cp, r_2$bic)
r_3 

#cp越小越好，大了就可能会导致overfitting
#bic, aic, dic越小越好
#在r_3中基于上述规则选择一个合适的选择