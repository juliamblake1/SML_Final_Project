#importing required packages
library(caret)
library(glmnet)
library(MASS)
library(Matrix)
library(car)
set.seed(123)

#Load full dataset for renters in 2022

renters_2022 <- read.csv("renters_2022.csv")
head(renters_2022)

# Load train, test, and validation datasets
train_data <- read.csv("train.csv")
test_data <- read.csv("test.csv")
validation_data <- read.csv("validation.csv")

# Define predictors and response variable
predictors <- c("STATEFIP", "SEX", "AGE", "MARST", "BPL", "LANGUAGE", "DENSITY",
                "INCTOT", "RACE", "CITIZEN", "EDUC", "EMPSTAT")
response <- "RENTGRS"

########################################################################################################
### GLM

glm_model <- glm(RENTGRS ~ STATEFIP + SEX + AGE + MARST + BPL + LANGUAGE + DENSITY + 
                   INCTOT + RACE + CITIZEN + EDUC + EMPSTAT, data = renters_2022, family = gaussian)
summary(glm_model)

# Evaluate the model
predictions_glm <- predict(glm_model, newdata = test_data[, predictors])
rmse_glm <- sqrt(mean((predictions_glm - test_data[, response])^2))
cat("RMSE:", rmse_glm, "\n")

# Calculate AIC and BIC
AIC_value <- AIC(glm_model)
BIC_value <- BIC(glm_model)
cat("AIC:", AIC_value, "\n")
cat("BIC:", BIC_value, "\n")

# Create a scatter plot of actual vs. predicted values
plot(test_data[, response], predictions_glm, 
     xlab = "Actual Values", ylab = "Predicted Values", 
     main = "Actual vs. Predicted Values (GLM)")
abline(0, 1, col = "red", lty = 2)
legend("topleft", legend = "Perfect Prediction", col = "red", lty = 2, bty = "n")

# Residuals Analysis
residuals_glm <- test_data[, response] - predictions_glm

# Plot residuals against predicted values
plot(predictions_glm, residuals_glm, 
     xlab = "Predicted Values", ylab = "Residuals", 
     main = "Residuals vs. Predicted Values (GLM)")
abline(h = 0, col = "red", lty = 2)

# Check for homoscedasticity (constant variance)
plot(sqrt(abs(residuals_glm)) ~ predictions_glm,
     xlab = "Fitted values",
     ylab = "Square root of absolute residuals",
     main = "Scale-Location Plot (GLM)")
smoothScatter(predictions_glm, sqrt(abs(residuals_glm)), colramp = colorRampPalette(c("blue", "red")))

# Evaluate Cook's distance for identifying influential points
cooksd_glm <- cooks.distance(glm_model)
influential_points_glm <- which(cooksd_glm > 4 / length(cooksd_glm))
points(predictions_glm[influential_points_glm], residuals_glm[influential_points_glm], col = "red", pch = 16) # Highlight influential points on the scatter plot
legend("topright", legend = "Influential Points", col = "red", pch = 16)

# Check for normality of residuals
qqPlot(residuals_glm)


########################################################################################################

### Fit the ridge regression model using cross-validation
ridge_model <- cv.glmnet(as.matrix(train_data[, predictors]), train_data[, response],
                  alpha = 0, lambda = NULL)  # Default lambda values will be chosen

best_lambda <- ridge_model$lambda.min
final_ridge_model <- glmnet(as.matrix(train_data[, predictors]), train_data[, response],
                            alpha = 0, lambda = best_lambda)
summary(final_ridge_model)

# Evaluate the model
predictions_ridge <- predict(final_ridge_model, newx = as.matrix(test_data[, predictors]))
rmse <- sqrt(mean((predictions_ridge - test_data[, response])^2))
cat("RMSE:", rmse, "\n")

plot(test_data[, response], predictions_ridge, 
     xlab = "Actual Values", ylab = "Predicted Values", 
     main = "Actual vs. Predicted Values")
abline(0, 1, col = "red", lty = 2)
legend("topleft", legend = "Perfect Prediction", col = "red", lty = 2, bty = "n")

# Residuals Analysis
residuals_ridge <- test_data[, response] - predictions_ridge
plot(predictions_ridge, residuals_ridge, 
     xlab = "Predicted Values", ylab = "Residuals", 
     main = "Residuals vs. Predicted Values")
abline(h = 0, col = "red", lty = 2)

# Check for homoscedasticity (constant variance)
plot(sqrt(abs(residuals_ridge)) ~ predictions_ridge,
     xlab = "Fitted values",
     ylab = "Square root of absolute residuals",
     main = "Scale-Location Plot")
smoothScatter(predictions_ridge, sqrt(abs(residuals_ridge)), colramp = colorRampPalette(c("blue", "red")))

# Evaluate Cook's distance for identifying influential points
cooksd <- cooks.distance(final_ridge_model)
influential_points <- which(cooksd > 4 / length(cooksd))
points(predictions[influential_points], residuals[influential_points], col = "red", pch = 16) # Highlight influential points on the scatter plot
legend("topright", legend = "Influential Points", col = "red", pch = 16)

# Check for normality of residuals
qqPlot(residuals_ridge)

########################################################################################################
### Lasso
lasso_model <- cv.glmnet(as.matrix(train_data[, predictors]), train_data[, response],
                         alpha = 1, lambda = NULL)  # Use alpha = 1 for Lasso regression

best_lambda_lasso <- lasso_model$lambda.min
final_lasso_model <- glmnet(as.matrix(train_data[, predictors]), train_data[, response],
                            alpha = 1, lambda = best_lambda_lasso)
summary(final_lasso_model)

# Evaluate the model
predictions_lasso <- predict(final_lasso_model, newx = as.matrix(test_data[, predictors]))
rmse_lasso <- sqrt(mean((predictions_lasso - test_data[, response])^2))
cat("RMSE:", rmse_lasso, "\n")

# Create a scatter plot of actual vs. predicted values
plot(test_data[, response], predictions_lasso, 
     xlab = "Actual Values", ylab = "Predicted Values", 
     main = "Actual vs. Predicted Values (Lasso)")
abline(0, 1, col = "red", lty = 2)
legend("topleft", legend = "Perfect Prediction", col = "red", lty = 2, bty = "n")

# Residuals Analysis
residuals_lasso <- test_data[, response] - predictions_lasso

# Plot residuals against predicted values
plot(predictions_lasso, residuals_lasso, 
     xlab = "Predicted Values", ylab = "Residuals", 
     main = "Residuals vs. Predicted Values (Lasso)")
abline(h = 0, col = "red", lty = 2)

# Check for homoscedasticity (constant variance)
plot(sqrt(abs(residuals_lasso)) ~ predictions_lasso,
     xlab = "Fitted values",
     ylab = "Square root of absolute residuals",
     main = "Scale-Location Plot (Lasso)")
smoothScatter(predictions_lasso, sqrt(abs(residuals_lasso)), colramp = colorRampPalette(c("blue", "red")))

# Evaluate Cook's distance for identifying influential points
cooksd_lasso <- cooks.distance(final_lasso_model)
influential_points_lasso <- which(cooksd_lasso > 4 / length(cooksd_lasso))
points(predictions_lasso[influential_points_lasso], residuals_lasso[influential_points_lasso], col = "red", pch = 16) # Highlight influential points on the scatter plot
legend("topright", legend = "Influential Points", col = "red", pch = 16)

# Check for normality of residuals using qqPlot
qqPlot(residuals_lasso)

########################################################################################################

# Bayesian estimation
library(brms)

#this takes an eternity so beware
brm_model <- brm(RENTGRS ~ STATEFIP + SEX + AGE + MARST + BPL + LANGUAGE + DENSITY + 
                   INCTOT + RACE + CITIZEN + EDUC + EMPSTAT,
                 data = train_data) # can add MCMC settings

# Summary of the Bayesian regression model
summary(brm_model)
plot(brm_model)
stancode(brm_model)
mcmc_diag(brm_model)















