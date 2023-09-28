#数据预处理
student_data <- read.csv("E:/zyf_gy/zyf_gy_2304_work/tool_codes_k1/R_lesson_youtube_2023a/student_data.csv")
data <- student_data[, c(2,3,4)]
lm_data = na.omit(data) #delete data with NA values

plot(lm_data$height, lm_data$weight ,main = "scatter plot")



#use least square method to fit simple linear regression
model =lm(weight ~ height, data = lm_data)
model_anova <- anova(model)
model_res <- model$residuals
model_coe <- model$coefficients
model_rank <- model$rank
model_df_res <-model$df.residual
model_fitv <-model$fitted.values

#export fitting result
print('------model------')
print(model)
print('------summary(model)------')
print(summary(model))

#draw lines
abline(h = mean(lm_data$weight), col = "red")    #draw a horizontal line
abline(v = 170,col ="red")    #draw a vertical line
abline(model,col="red")    #draw model regression line

#假设检验-身高和体重是否呈线性关系-1.区间估计法
model_coe_slope <- model_coe[2]
model_coe_slope_std <- summary(model)$coefficients["height", "Std. Error"]
tv1 = -qt(0.025, df = model$df.residual) #to_be_set
tv1b <- c(model_coe_slope - tv1 * model_coe_slope_std,model_coe_slope + tv1 * model_coe_slope_std)
#此处tv1b不包括0，所以拒绝零假设，认为两者有线性关系
# Assuming 'model' is your linear regression model

#假设检验-身高和体重是否呈线性关系-2.p值法
model_coe_slope_tv <- summary(model)$coefficients["height", "t value"]
tv2 <- (1-pt(model_coe_slope_tv, df = model$df.residual))*2
tv2b <- summary(model)$coefficients["height", "Pr(>|t|)"] # the calculation of tv2b refers to tv2
#此处tv2小于0.05，所以拒绝零假设，认为两者有线性关系

#假设检验-身高和体重是否呈线性关系-3.F值法
anova_f <- model_anova['height','F value']
anova_df1 <- model_anova['height','Df']
anova_df2 <- model_anova['Residuals','Df']
tv3 <- 1 - pf(anova_f,df1 <- anova_df1, df2 <- anova_df2)
#此处tv3小于0.05，所以拒绝零假设，认为两者有线性关系

#1.点估计
model$coefficients

#2.区间估计
summary(model)
esti_1 <- confint(model) #the same as tv1b

plot(lm_data$height, lm_data$weight ,main = "scatter plot")
abline(model, col = "red")

x_1 = data.frame(height = seq(150, 200, by = 1))
predict_v1 <- predict(model, newdata = x_1 , interval = "predict") #预测区间
predict_v2 <- predict(model, newdata = x_1 , interval = "confidence") #置信区间

lines(x_1$height, predict_v1[,2], col="blue", lty=2) #to_be_set
lines(x_1$height, predict_v1[,3], col="blue", lty=2) #to_be_set