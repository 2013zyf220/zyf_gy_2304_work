#refer to: Lai, Jiangshan, et al. 'glmm. hp: an R package for computing individualeffect of predictors in generalized linear mixed models.' Journal of Plant Ecology 15.6 (2022): 1302-1307.
library(PerformanceAnalytics)
library(car)
library(glmm.hp)

#input data
setwd('E:/zyf_gy/zyf_gy_2304_work/tool_codes_k1/R_lesson_youtube_2023a')
data_1 <- read.csv('housing_data.csv')
data_2 <- data_1[,-1]

#build multiple linear model
mod1 <- lm(log(price) ~ sqft.living + bedrooms + bathrooms, data = data_2)
mod1

#glmm.hp analysis
rsq_mod <- r.squaredGLMM(mod1)
glmm_hp_mod <- glmm.hp(mod1)
glmm_hp_mod
plot(glmm.hp(mod1))
