#refer to: https://www.statology.org/stepwise-regression-r/

intercept_only <- lm(mpg ~ 1, data=mtcars) #define intercept-only model
all <- lm(mpg ~ ., data=mtcars) #define model with all predictors

#perform forward stepwise regression
forward <- step(intercept_only, direction='forward', scope=formula(all), trace=0)
forward$anova #view results of forward stepwise regression
forward$coefficients #view final model

#perform backward stepwise regression
backward <- step(all, direction='backward', scope=formula(all), trace=0)
backward$anova #view results of backward stepwise regression
backward$coefficients

#perform both-direction stepwise regression
both <- step(intercept_only, direction='both', scope=formula(all), trace=0)
both$anova
both$coefficients

