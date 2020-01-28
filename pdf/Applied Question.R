#### Applied Exercise 8 ####

#a. Use the rnorm() function to generate a predictor X of length n = 100, as well as a noise vector eps of length n = 100.
set.seed(5)
X = rnorm(100)
eps = rnorm(100)

#b. Generate a response vector Y of length n = 100 according to the model \(Y = B0 +B1X +B2X^2 +B3X^3 +eps), 
#where B0, B1, B2, and B3 are constants of your choice.

b0 = 2
b1 = 3
b2 = 4
b3 = 5

Y = b0 + b1*X + b2*X^2 + b3*X^3 + eps

#c. Use the regsubsets() function to perform best subset selection in order to choose the best model containing the 
#predictors X, X^2...X^10. What is the best model obtained according to Cp, BIC, and adjusted R^2? 
#Show some plots to provide evidence for your answer, and report the coefficients of the best model obtained. 
#Note you will need to use the data.frame() function to create a single data set containing both X and Y.

library(ISLR)
library(leaps)

df <- data.frame(Y = Y, X = X)
fit <- regsubsets(Y ~ poly(X, 10, raw = T), data = df, nvmax = 10)
fit_sum <- summary(fit)
fit_sum

par(mfrow = c(2,2)) # Display graphs in two rows, two columns
x_axis_label = '# of variables'


plot(fit_sum$rss,xlab = x_axis_label, ylab = 'RSS', type = 'l')

plot (fit_sum$cp,xlab = x_axis_label, ylab = 'Cp', type = 'l')
points(which.min(fit_sum$cp), fit_sum$cp[which.min(fit_sum$cp)], col = 'orange', pch = 15, cex = 1)

plot(fit_sum$bic,xlab = x_axis_label,  ylab = 'BIC', type = 'l')
points(which.min(fit_sum$bic), fit_sum$bic[which.min(fit_sum$bic)], col = 'orange', pch = 15, cex = 1)

plot(fit_sum$adjr2,xlab = x_axis_label, ylab = 'R2', type = 'l')
points(which.max(fit_sum$adjr2), fit_sum$adjr2[which.max(fit_sum$adjr2)], col = 'orange', pch = 15, cex = 1)

coef(fit, 3) #Very close to the number we choose

#d. Repeat c using forward stepwise selection and also using backwards stepwise selection. 
#How does your answer compare to the results in c?

fit <- regsubsets(Y~poly(X, 10, raw = T), data = df, nvmax = 10, method = 'forward')
fit_sum <- summary(fit)
par(mfrow = c(2, 2))

plot(fit_sum$rss, xlab = 'Number of Variables', ylab = 'RSS', type = 'l')

plot(fit_sum$cp, xlab = 'Number of Variables', ylab = 'Cp', type = 'l')
points(which.min(fit_sum$cp), fit_sum$cp[which.min(fit_sum$cp)], col = 'orange', pch = 15, cex = 1)

plot(fit_sum$bic, xlab = 'Number of Variables', ylab = 'BIC',  type = 'l')
points(which.min(fit_sum$bic), fit_sum$bic[which.min(fit_sum$bic)], col = 'orange', pch = 15, cex = 1)

plot(fit_sum$adjr2, xlab = 'Number of Variables', ylab = 'Adjusted R2', type = 'l')
points(which.max(fit_sum$adjr2), fit_sum$adjr2[which.max(fit_sum$adjr2)], col = 'orange', pch = 15, cex = 1)

coef(fit, 3) # Cp, BIC, R2

#Backwards
fit <- regsubsets(Y~poly(X, 10, raw = T), data = df, nvmax = 10, method = 'backward')
fit_sum <- summary(fit)

par(mfrow = c(2, 2))

plot(fit_sum$rss, xlab = 'Number of Variables', ylab = 'RSS', type = 'l')

plot(fit_sum$cp, xlab = 'Number of Variables', ylab = 'Cp', type = 'l')
points(which.min(fit_sum$cp), fit_sum$cp[which.min(fit_sum$cp)], col = 'orange', pch = 15, cex = 1)

plot(fit_sum$bic, xlab = 'Number of Variables', ylab = 'BIC',  type = 'l')
points(which.min(fit_sum$bic), fit_sum$bic[which.min(fit_sum$bic)], col = 'orange', pch = 15, cex = 1)

plot(fit_sum$adjr2, xlab = 'Number of Variables', ylab = 'Adjusted R2', type = 'l')
points(which.max(fit_sum$adjr2), fit_sum$adjr2[which.max(fit_sum$adjr2)], col = 'orange', pch = 15, cex = 1)

coef(fit, 3) 

#e. Now fit a lasso model to the simulated data, again using X,X^2,..., X^10 as predictors. 
#Use cross-validation to select the optimal value of lambda. Create plots of the cross-validation error 
#as a function of lambda. Report the resulting coefficient estimates, and discuss the results obtained.

library(glmnet)

matrix = model.matrix(Y ~ poly(X, 10, raw = T), data = df)[, -1]
cv_results = cv.glmnet(matrix, Y, alpha = 1)
plot(cv_results)
min_lambda = cv_results$lambda.min
lasso_model = glmnet(matrix, Y, alpha = 1, lambda = min_lambda)
coef(lasso_model)

#6. Now generate a response vector Y according to the model \(Y = β0 + β7X_7 + ε\), and perform best subset selection and the lasso. Discuss the results obtained.
set.seed(5)
X = rnorm(100, mean = 0, sd = 1)
e = rnorm(100, mean = 0, sd = 0.5)
b0 = 8
b7 = 77


Y = b0 + b7*X^7 + e


df = data.frame(Y = Y, X = X)
fit = regsubsets(Y~poly(X, 10, raw = T), data = df, nvmax = 10)
fit_sum = summary(fit)


par(mfrow = c(2, 2))
plot(fit_sum$rss,   xlab = 'Number of Variables', ylab = 'RSS',          type = 'l')


plot(fit_sum$cp,    xlab = 'Number of Variables', ylab = 'Cp',           type = 'l')
points(which.min(fit_sum$cp), fit_sum$cp[which.min(fit_sum$cp)], col = 'orange', pch = 15, cex = 2)


plot(fit_sum$bic,   xlab = 'Number of Variables', ylab = 'BIC',          type = 'l')
points(which.min(fit_sum$bic), fit_sum$bic[which.min(fit_sum$bic)], col = 'orange', pch = 15, cex = 2)


plot(fit_sum$adjr2, xlab = 'Number of Variables', ylab = 'Adjusted R2',  type = 'l')
points(which.max(fit_sum$adjr2), fit_sum$adjr2[which.max(fit_sum$adjr2)], col = 'orange', pch = 15, cex = 2)

coef(fit, 2) # Cp
coef(fit, 1) # BIC
coef(fit, 4) # R2
matrix = model.matrix(Y~poly(X, 10, raw = T), data = df)[, -1]
