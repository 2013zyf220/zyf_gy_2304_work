data(mtcars)
full_model <- lm(mpg ~ ., data = mtcars)
step_model <- step(full_model) # Perform stepwise regression
summary(step_model)  # Print the results

predicted_values <- predict(step_model, newdata = mtcars)  # Make predictions on the training data
residuals <- mtcars$mpg - predicted_values  # Calculate residuals
squared_residuals <- residuals^2  # Calculate squared residuals
mse <- mean(squared_residuals)  # Calculate mean squared error (MSE)
rmse <- sqrt(mse)   # Calculate root mean squared error (RMSE)
cat("Root Mean Squared Error (RMSE):", rmse, "\n") # Print RMSE
