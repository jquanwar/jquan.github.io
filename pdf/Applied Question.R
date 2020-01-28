#### Applied Exercise 8 ####

#a. Use the rnorm() function to generate a predictor X of length n = 100, as well as a noise vector eps of length n = 100.
set.seed(1)
X = rnorm(100)
eps = rnorm(100)

#b. Generate a response vector Y of length n = 100 according to the model \(Y = B0 +B1X +B2X^2 +B3X^3 +eps), 
#where B0, B1, B2, and B3 are constants of your choice.

B0 = 2
B1 = 3
B2 = 4
B3 = 5

Y = B0 + B1*X + B2*X^2 + B3*X^3 + eps

#c. Use the regsubsets() function to perform best subset selection in order to choose the best model containing the 
#predictors X, X^2...X^10. What is the best model obtained according to Cp, BIC, and adjusted R^2? 
#Show some plots to provide evidence for your answer, and report the coefficients of the best model obtained. 
#Note you will need to use the data.frame() function to create a single data set containing both X and Y.

library(ISLR)
library(leaps)

df <- data.frame(Y = Y, X = X)
#regfit.full <- regsubsets(Y ~ poly(X, 10, raw = T), data = df, nvmax = 10)
regfit.full <- regsubsets(Y ~ X + I(X^2) + I(X^3) + I(X^4) + I(X^5) + I(X^6) + I(X^7) + I(X^8) + I(X^9) + I(X^10), data = df, nvmax = 10)
(reg.summary <- summary(regfit.full))


par(mfrow = c(2,2)) # Display graphs in two rows, two columns
x_axis_label = '# of variables'


plot(reg.summary$rss,xlab = x_axis_label, ylab = 'RSS', type = 'l')

plot (reg.summary$cp,xlab = x_axis_label, ylab = 'Cp', type = 'l')
points(which.min(reg.summary$cp), reg.summary$cp[which.min(reg.summary$cp)], col = 'orange', pch = 15, cex = 1)

plot(reg.summary$bic,xlab = x_axis_label,  ylab = 'BIC', type = 'l')
points(which.min(reg.summary$bic), reg.summary$bic[which.min(reg.summary$bic)], col = 'orange', pch = 15, cex = 1)

plot(reg.summary$adjr2,xlab = x_axis_label, ylab = 'R2', type = 'l')
points(which.max(reg.summary$adjr2), reg.summary$adjr2[which.max(reg.summary$adjr2)], col = 'orange', pch = 15, cex = 1)

coef(regfit.full, which.min(reg.summary$cp))
coef(regfit.full, which.min(reg.summary$bic))
coef(regfit.full, which.max(reg.summary$adjr2))

#d. Repeat c using forward stepwise selection and also using backwards stepwise selection. 
#How does your answer compare to the results in c?

#Forward
regfit.fwd <- regsubsets(Y ~ X + I(X^2) + I(X^3) + I(X^4) + I(X^5) + I(X^6) + I(X^7) + I(X^8) + I(X^9) + I(X^10), data = df, nvmax = 10, method = 'forward')
(fwd.summary <- summary(regfit.fwd))

plot(fwd.summary$rss, xlab = 'Number of Variables', ylab = 'RSS', type = 'l')

plot(fwd.summary$cp, xlab = 'Number of Variables', ylab = 'Cp', type = 'l')
points(which.min(fwd.summary$cp), fwd.summary$cp[which.min(fwd.summary$cp)], col = 'orange', pch = 15, cex = 1)

plot(fwd.summary$bic, xlab = 'Number of Variables', ylab = 'BIC',  type = 'l')
points(which.min(fwd.summary$bic), fwd.summary$bic[which.min(fwd.summary$bic)], col = 'orange', pch = 15, cex = 1)

plot(fwd.summary$adjr2, xlab = 'Number of Variables', ylab = 'Adjusted R2', type = 'l')
points(which.max(fwd.summary$adjr2), fwd.summary$adjr2[which.max(fwd.summary$adjr2)], col = 'orange', pch = 15, cex = 1)

coef(regfit.fwd, which.min(fwd.summary$cp))
coef(regfit.fwd, which.min(fwd.summary$bic))
coef(regfit.fwd, which.max(fwd.summary$adjr2))

#Backwards

regfit.bwd <- regsubsets(Y ~ X + I(X^2) + I(X^3) + I(X^4) + I(X^5) + I(X^6) + I(X^7) + I(X^8) + I(X^9) + I(X^10), data = df, nvmax = 10, method = 'backward')
(bwd.summary <- summary(regfit.bwd))


plot(bwd.summary$rss, xlab = 'Number of Variables', ylab = 'RSS', type = 'l')

plot(bwd.summary$cp, xlab = 'Number of Variables', ylab = 'Cp', type = 'l')
points(which.min(bwd.summary$cp), bwd.summary$cp[which.min(bwd.summary$cp)], col = 'orange', pch = 15, cex = 1)

plot(bwd.summary$bic, xlab = 'Number of Variables', ylab = 'BIC',  type = 'l')
points(which.min(bwd.summary$bic), bwd.summary$bic[which.min(bwd.summary$bic)], col = 'orange', pch = 15, cex = 1)

plot(bwd.summary$adjr2, xlab = 'Number of Variables', ylab = 'Adjusted R2', type = 'l')
points(which.max(bwd.summary$adjr2), bwd.summary$adjr2[which.max(bwd.summary$adjr2)], col = 'orange', pch = 15, cex = 1)

coef(regfit.bwd, which.min(bwd.summary$cp))
coef(regfit.bwd, which.min(bwd.summary$bic))
coef(regfit.bwd, which.max(bwd.summary$adjr2))

par(mfrow = c(1, 1))
