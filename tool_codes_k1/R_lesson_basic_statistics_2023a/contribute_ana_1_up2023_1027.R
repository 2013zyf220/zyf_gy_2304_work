library(relaimpo)
library(PerformanceAnalytics)

data(mtcars) # Load the dataset

model <- lm(mpg ~ wt + hp + disp, data = mtcars) # Create a linear regression model
rel_contributions <- calc.relimp(model, type = 'lmg') # Calculate relative variable importance
rel_contributions$lmg['wt']

