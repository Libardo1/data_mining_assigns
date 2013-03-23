# This R script pertains to question 6 and other types of regression
#install.packages("glmnet")
library ("glmnet")

# Load the boston housing
boston_housing <- read.delim("~/development/data_mining/BostonHousing.txt")
boston_housing.matrix <- model.matrix(~. - 1, data=boston_housing)

# Run the LASSO regression with the specified values of lambda
boston_housing.medv_lasso_regression <- glmnet(x=boston_housing.matrix , y=boston_housing$MEDV, alpha=1, lambda=c(1, 0.5, 0.1, 0.01))

# Run the ridge regression with the specified values of lambda
boston_housing.medv_ridge_regression <- glmnet(x=boston_housing.matrix , y=boston_housing$MEDV, alpha=0, lambda=c(1, 0.5, 0.1, 0.01))

# Print coefs
coef(boston_housing.medv_lasso_regression)
coef(boston_housing.medv_ridge_regression)
