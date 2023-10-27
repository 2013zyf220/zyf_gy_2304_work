library(relaimpo)
library(PerformanceAnalytics)

data(swiss)
rc_1 <- calc.relimp(swiss, type = c('lmg', 'last', 'first', 'betasq', 'pratt', 'genizi', 'car'))
rc_1$lmg['Agriculture']

lm_1 <- lm(Fertility ~ Agriculture + Examination + Education + Catholic + Infant.Mortality, swiss)
rc_2 <- calc.relimp(lm_1, type = c('lmg', 'last', 'first', 'betasq', 'pratt', 'genizi', 'car'), rela = TRUE)
rc_2$lmg['Agriculture']

rc_3 <- calc.relimp(swiss, type = c('lmg', 'last', 'first', 'betasq', 'pratt', 'genizi', 'car'), rela = TRUE)
rc_3$lmg['Agriculture']

rc_4 <- calc.relimp(swiss, type = c('lmg', 'last', 'first', 'betasq', 'pratt'), always = 'Education')
rc_4$lmg['Agriculture']

rc_5 <- calc.relimp(Fertility ~ Agriculture + Examination + Education + Catholic + Infant.Mortality, swiss, type = c('lmg', 'last', 'first', 'betasq', 'pratt', 'genizi', 'car'), subset = Catholic > 40)
rc_5$lmg['Agriculture']

rc_6 <- calc.relimp(swiss, type = c('lmg', 'last', 'first'), groups = c('Education','Examination'))
rc_6$lmg['Agriculture']



