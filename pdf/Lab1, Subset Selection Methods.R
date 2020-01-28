###########################################
### 6.5 Lab 1: Subset Selection Methods ###
###########################################

#### 6.5.1 Best Subset Selection #####

#We apply the best subset selection approach to the Hitters data. We
#wish to predict a baseball player's Salary on the basis of various statistics
#associated with performance in the previous year.

rm(list=ls())

library(ISLR)

?Hitters
names(Hitters)
dim(Hitters)

sum(is.na(Hitters$Salary))
#counting all NAs in the Salary column (if any)

HittersData <- na.omit(Hitters)
dim(HittersData)
sum(is.na(HittersData$Salary))
#with the na.omit() function we removed NAs and we are double checking if NAs are present now

library(leaps)

regfit.full <- regsubsets(Salary ~ .,HittersData)
#The regsubsets() function (part of the leaps library) performs best subset selection 
#by identifying the best model that contains a given number
#of predictors, where best is quantified using RSS.

summary(regfit.full)
#The summary() command outputs the best set of variables for each model size.
#An asterisk indicates that a given variable is included in the corresponding model.

regfit.full <- regsubsets(Salary~.,data=Hitters ,nvmax=19)
# nvmax option can be used in order to return as many variables as are desired

reg.summary <- summary(regfit.full)
#The summary() function also returns R^2, RSS, adjusted R^2, Cp, and BIC.
#We can examine these to try to select the best overall model.
reg.summary

par(mfrow=c(2,2))

plot(reg.summary$rss ,xlab="Number of Variables ",ylab="RSS",
       type="l")

plot(reg.summary$adjr2 ,xlab="Number of Variables ",
       ylab="Adjusted R^2",type="l")
adjr2max <- which.max(reg.summary$adjr2)
points(adjr2max,reg.summary$adjr2[adjr2max], col="red",cex=2,pch =20)

plot(reg.summary$cp ,xlab="Number of Variables ",ylab="Cp",
     type="l")
cpmin <- which.min(reg.summary$cp )
points(cpmin,reg.summary$cp [cpmin], col ="red",cex=2,pch =20)

plot(reg.summary$bic ,xlab="Number of Variables ",ylab="BIC",
     type="l")
bicmin <- which.min(reg.summary$bic )
points(bicmin,reg.summary$bic [bicmin],col="red",cex=2,pch =20)

?plot.regsubsets
plot(regfit.full ,scale="r2")
plot(regfit.full ,scale="adjr2")
plot(regfit.full ,scale="Cp")
plot(regfit.full ,scale="bic")

coef(regfit.full, bicmin)

par(mfrow=c(1,1))

#### 6.5.2 Forward and Backward Stepwise Selection ####

#We can also use the regsubsets() function to perform forward stepwise or 
#backward stepwise selection, using the argument method="forward" or method="backward"

regfit.fwd <- regsubsets(Salary~., data=HittersData,nvmax=19, 
                        method = "forward")
summary(regfit.fwd)

regfit.bwd <- regsubsets(Salary~.,data=HittersData , nvmax=19,
                         method = "backward")
summary(regfit.bwd)

#Note that the 3 methods does not always choose the same predictors 
coef(regfit.full, 3)
coef(regfit.fwd, 3)
coef(regfit.bwd, 3)

coef(regfit.full, 7)
coef(regfit.fwd, 7)
coef(regfit.bwd, 7)


#### 6.5.3 Choosing Among Models Using the Validation Set Approach and Cross-Validation ####

set.seed(1)

train <- sample(c(T,F), nrow(HittersData), rep = T)
test <- (!train)

regfit.best <- regsubsets(Salary ~ ., data = HittersData[train,], nvmax = 19)
#We apply regsubsets() to the training set in order to perform best subset selection. 

test.mat <- model.matrix(Salary ~ ., data = HittersData[test,])
#We now compute the validation set error for the best model of each model size. 
#We ???rst make a model matrix from the test data. 

val.errors <- rep(0,19)
for(i in 1:19){
  coefi <- coef(regfit.best, id = i)
  pred <- test.mat[,names(coefi)]%*%coefi
  val.errors[i] <- mean((HittersData$Salary[test]-pred)^2)
}
# Now we run a loop, and for each size i, we extract the coeficients from regfit.best for 
#the best model of that size, multiply them into the appropriate columns of the test model matrix 
#to form the predictions, and compute the test MSE.


val.error.min <- which.min(val.errors)
#Here we find which model works better that contains the lowest validation errors
val.error.min

coef(regfit.best,val.error.min)
#shows the number of variables for the best model


regfit.best <- regsubsets(Salary ~ .,data=HittersData ,nvmax=19)
coef(regfit.best ,10)
#We are now performing the best subset selection on the full data set, and select the best ten-variable model (arbitrarily) 


#We now try to choose among the models of different sizes using cross-validation.
#This approach is somewhat involved, as we must perform best
#subset selection within each of the k training sets.
k=10
#A k-10 fold validation is a common choice
set.seed(1)
folds=sample(1:k,nrow(HittersData),replace=TRUE)
cv.errors =matrix(0,k,19, dimnames =list(NULL,paste (1:19)))

for(j in 1:k){
  best.fit <- regsubsets(Salary ~ .,data=HittersData[folds!=j,],
                         nvmax=19)
  for(i in 1:19){
    pred <- predict(best.fit ,HittersData[folds ==j,],id=i)
    cv.errors[j,i] <- mean((HittersData$Salary[folds==j]-pred)^2)
    }
}


mean.cv.errors <- apply(cv.errors ,2, mean)
mean.cv.errors

par(mfrow=c(1,1))
plot(mean.cv.errors ,type="b")

reg.best <- regsubsets(Salary~.,data=Hitters , nvmax=19)

coef(reg.best ,which.min(mean.cv.errors))
