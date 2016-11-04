
## Remove Categorical variables for now
# 
#http://blog.minitab.com/blog/adventures-in-statistics/regression-analysis-tutorial-and-examples
# https://rpubs.com/ryankelly/reg (Linear Model Selection & Regularization)

#####Part 3:
##Testing data http://r-statistics.co/Linear-Regression.html for ML (AIC:BIC:Cheat sheet)
## http://www.statmethods.net/stats/regression.html 
## exhaustive search to select the best feature for the Model
## https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3478798/  --> Weather Prediction

## Taking input from our merged data set
setwd("C:/Users/Samy/Dropbox/ADS/Assignment2")
mergeData <- read.csv("MergedData.csv",header = TRUE)
str(mergeData)

## Removing columns which are not necessary and will not be of any importance for the Linear regression that we want to perform
require(dplyr)
mergeData <- mergeData %>% select(-c(X,Date,Account,Channel,Units,year))

## Converting non numeric features to numeric
mergeData$hour <- as.numeric(mergeData$hour)
mergeData$month <- as.numeric(mergeData$month)
mergeData$Day.of.Week <-  as.numeric(mergeData$Day.of.Week)
mergeData$weekday <-  as.numeric(mergeData$weekday)
mergeData$PeakHour <- as.numeric(mergeData$PeakHour)
mergeData$day <- as.numeric(mergeData$day)

str(mergeData)

## Taking the subset of the data frame which only contains numeric features, removing categorical variables
merge.sel <- subset(mergeData,select = -c(Conditions,Wind_Direction))


##Checking for non linearity (visually) and transforming the variables
require(psych)
#pairs.panels(merge.sel,col="red")

##### Feature Transformation 
plot(density(merge.sel$Kwh))
 
## As seen from the distribution of KwH, we will use a log scale to convert the feature, which is nothing but our dependent variable
## Also, we will revert the transfromation with the model output
merge.sel$Kwh <- log10(merge.sel$Kwh)
plot(density(log10(merge.sel$Kwh)))

## Lets check other feauture with respect to kwH and see the relationship

##Lets check the histogram of Day and also the realtionship with kwH

hist(merge.sel$hour)
scatter.smooth(x=merge.sel$Kwh,y=merge.sel$hour)

## It does not look that great and lets see if we can use "forecast" package and use BoxCox transformtation on "hour" variable

require("forecast")

## Lets find the Lambda for the BoxCox transfromation on Hour. Which is caluclated as
##    wt = log(yt) if λ=0;
##    wt = (yt^λ−1)/λ otherwise.
## Where yt is the original observation and wt is the transformed observation and t denoted the observation

lambda <- BoxCox.lambda(merge.sel$hour)

## So Boxflot suggest a lambda of 0.1098 as the transformation unit
## Also, if we want to reverse the tranformation we can use the below formula
#     yt= exp(wt) if λ = 0;
#     yt= (λwt+1) ^ 1\λ  otherwise.

# Now since we have the Lambda we can transform the variable
merge.sel$hour <- BoxCox(merge.sel$hour,lambda = lambda)

## Checking the histogram and relationship plot, we see much better linearity 
hist(merge.sel$hour)
scatter.smooth(x=merge.sel$Kwh,y=merge.sel$hour)

## Similarly, we can improve the performance on "Day Of Week"
lambda <- BoxCox.lambda(merge.sel$Day.of.Week)
merge.sel$Day.of.Week <- BoxCox(merge.sel$Day.of.Week,lambda = lambda)

##### Use Leaps library to select features for Multiple Regression
#install.packages("leaps")
## 1) Feature Selection Exhaustive Search
library(leaps)

regsubsets.out <- regsubsets(Kwh ~ ., data = merge.sel, nvmax = 14,method = "exhaustive")
summary(regsubsets.out)
res <- summary(regsubsets.out)

par(mfrow=c(2,2))
plot(res$cp) 
plot(regsubsets.out, scale = "adjr2", main = "Adjusted R^2")
plot(regsubsets.out, scale = "bic", main = "BIC")
plot(res$adjr2)
coef(regsubsets.out,10)
res$adjr2[10]

## 2) Feature Selection Forward Search
library(leaps)

regsubsets.fwd <- regsubsets(Kwh ~ ., data = merge.sel, nvmax = 14,method = "forward")
summary(regsubsets.fwd)
resfwd <- summary(regsubsets.fwd)
par(mfrow=c(2,2))
plot(resfwd$cp)
plot(regsubsets.fwd, scale = "adjr2", main = "Adjusted R^2")
plot(regsubsets.fwd, scale = "bic", main = "BIC")
plot(resfwd$adjr2)
coef(regsubsets.fwd,10)
resfwd$adjr2[10]

## 3) Feature Selection Backward Search
library(leaps)

regsubsets.bwd <- regsubsets(Kwh ~ ., data = merge.sel, nvmax = 14,method = "backward")
summary(regsubsets.bwd)
resbwd <- summary(regsubsets.bwd)

par(mfrow=c(2,2))
plot(resbwd$cp)
plot(regsubsets.bwd, scale = "adjr2", main = "Adjusted R^2")
plot(regsubsets.bwd, scale = "bic", main = "BIC")
plot(resbwd$adjr2)
coef(regsubsets.bwd,10)
resbwd$adjr2[10]

## 4) Selecting a formula based on stepwise feature selection
regsubsets.step <- regsubsets(Kwh~hour + month + day + Day.of.Week + weekday + PeakHour + TemperatureF + Dew_PointF + Humidity +
                                VisibilityMPH, data = merge.sel, nvmax =14 )
summary(regsubsets.step)
resstep <- summary(regsubsets.step)
par(mfrow=c(2,2))
plot(resstep$cp)
plot(regsubsets.step, scale = "adjr2", main = "Adjusted R^2")
plot(regsubsets.step, scale = "bic", main = "BIC")
plot(resstep$adjr2)
coef(regsubsets.step,10)
resstep$adjr2[10]


### Develop a linear model using variables selected from Backward approach
#   The model will be built using the training sample of the data
#   The model will be vaildated using the validation sample of the data


############# SPLIT VALIDATION
# Split the data into training and validation samples
# We will use (train.size)% for training and (100-train.size)% for validation

set.seed(2017)
train.size <- 0.8
train.index <- sample.int(length(merge.sel$Kwh),round(length(merge.sel$Kwh)*train.size))
train.sample <- merge.sel[train.index,]
train.val <- merge.sel[-train.index,]


### Multiple regression model uses a simple formula:
#     Kwh = B0 + B1xTemperatureF + B2xPeakHour + B3xDew_PointF
#
# We will perform additional tests on training data

# We will use a stepwise selection of variables by backward elimination
# we will consider all possible canidate variables and eliminate one at a time


### Fit the model
fit <- lm(Kwh ~ hour + month + Day.of.Week + weekday + PeakHour + TemperatureF + Dew_PointF + Humidity + Sea_Level_PressureIn
          + VisibilityMPH , data = train.sample)

summary(fit) 
#R-squared:  0.5634,	Adjusted R-squared:  0.5628 

##Diagnostic plots provide checks for heteroscedasticity, normality, and influential observerations

plot(fit)

# Lets check the distribution of errors around the model
# Red Line is the model and green line shows the fit of the variables
car::crPlots(fit)

# Eliminate extreme values
cutoff <- 4/((nrow(train.sample)-length(fit$coefficients)-2)) # Cook's D plot, cutoff as 4/(n-k-1)
plot(fit,which=4, cook.levels = cutoff) #identify D values > cutoff
plot(fit,which=5, cook.levels = cutoff) #how important are these observations for the construction of the model

## DO NOT RUN BUT SHOW ON THE REPORT
train.sample <- train.sample[-which(rownames(train.sample) %in% c("3121")),]

# Refit the model
fit <- lm(Kwh ~ hour + month + Day.of.Week + weekday + PeakHour + TemperatureF + Dew_PointF + Humidity + Sea_Level_PressureIn
          + VisibilityMPH , data = train.sample)

summary(fit)

cutoff <- 4/((nrow(train.sample)-length(fit$coefficients)-2)) #Cook's D plot, cutoff as 4/(n-k-1)
plot(fit,which=4, cook.levels = cutoff) #identify D values > cutoff
plot(fit,which=5, cook.levels = cutoff) #how important are these observations for the construction of the model
train.sample <- train.sample[-which(rownames(train.sample) %in% c("1062","117","8170")),]


fit <- lm(Kwh ~ hour + month + Day.of.Week + weekday + PeakHour + TemperatureF + Dew_PointF + Humidity + Sea_Level_PressureIn
          + VisibilityMPH , data = train.sample)
## Print the Regression Coffecients
## We can see that we managed to increase adjusted r square value by 1% which is very insignifcant
summary(fit) #R-squared:  0.5646,	Adjusted R-squared:  0.5639 

####  Now we can evaluate the final linear model
#     Find all predicted values for both a training and a validation set

trainPred = subset(train.sample, select = -c(Kwh))
validPred = subset(train.val, select = -c(Kwh))

train.sample$Pred.Kwh <- predict(fit, newdata = trainPred)
train.val$Pred.Kwh <- predict(fit, newdata = validPred)

##### 1)  Check how good is the model on the training set ~ correlation^2,RME and MAE
train.corr <- round(cor(train.sample$Pred.Kwh,train.sample$Kwh),2)

## Lets check the data on how exactly has it predicted
head(train.sample[,c("Kwh","Pred.Kwh")])

## There is high correlation between actual and predicted. Lets calculate other paramters for the model
#Note:Bringing back the transformed kwH with reverse transformation
train.RMSE <- round(sqrt(mean((10 ^ train.sample$Pred.Kwh - 10 ^ train.sample$Kwh)^2)))
train.MAE <- round(mean(abs(10 ^ train.sample$Pred.Kwh - 10 ^ train.sample$Kwh)))
c(train.corr,train.RMSE,train.MAE)

## We can see that correlation between Actual and Predicted KwH is 75% better than predicted(56%)
## Root mean square with a value of 74 and Mean absolute error of 53 which is pretty good too


##### 2)  Check how good is the model on the Validation set ~ correlation^2,RME and MAE
val.corr <- round(cor(train.val$Pred.Kwh,train.val$Kwh),2)

## Lets check the data on how exactly has it predicted
head(train.val[,c("Kwh","Pred.Kwh")])

## There is high correlation between actual and predicted. Lets calculate other paramters for the model
#Note:Bringing back the transformed kwH with reverse transformation
### Print the Performance Metrix
val.RMSE <- round(sqrt(mean((10 ^ train.val$Pred.Kwh - 10 ^ train.val$Kwh)^2)))
val.MAE <- round(mean(abs(10 ^ train.val$Pred.Kwh - 10 ^ train.val$Kwh)))
c(val.corr,val.RMSE,val.MAE)

## We can see that correlation between Actual and Predicted KwH is 75%. Better than predicted(56%)
## Root mean square with a little higher than training(76) and Mean absolute error of 54 which is pretty good too



###################  Cross - validation
set.seed(2014)
library(caret)
library(klaR)
library(randomForest)

train.size <- 0.8
train.index <- sample.int(length(merge.sel$Kwh),round(length(merge.sel$Kwh)*train.size))
train.sample <- merge.sel[train.index,]
train.val <- merge.sel[-train.index,]

trainPred = subset(train.sample, select = -c(Kwh))
validPred = subset(train.val, select = -c(Kwh))

# define training control
train_control <- trainControl(method="cv", number=3)


# train the model
model <- caret::train(Kwh ~ hour + month + Day.of.Week + weekday + PeakHour + TemperatureF + Dew_PointF + Humidity + Sea_Level_PressureIn
               + VisibilityMPH , data = train.sample, trControl=train_control)
# summarize results

print(model)


train.sample$Pred.Kwh <- predict(model, newdata = trainPred)
train.val$Pred.Kwh <- predict(model, newdata = validPred)



##### 1)  Check how good is the model on the training set ~ correlation^2,RME and MAE
train.corr <- round(cor(train.sample$Pred.Kwh,train.sample$Kwh),2)

## Lets check the data on how exactly has it predicted
View(train.sample[,c("Kwh","Pred.Kwh")])

## There is high correlation between actual and predicted. Lets calculate other paramters for the model

train.RMSE <- round(sqrt(mean((10 ^ train.sample$Pred.Kwh - 10 ^ train.sample$Kwh)^2)))
train.MAE <- round(mean(abs(10 ^ train.sample$Pred.Kwh - 10 ^ train.sample$Kwh)))
c(train.corr,train.RMSE,train.MAE)


##### 2)  Check how good is the model on the Validation set ~ correlation^2,RME and MAE
val.corr <- round(cor(train.val$Pred.Kwh,train.val$Kwh),2)

## Lets check the data on how exactly has it predicted
View(train.val[,c("Kwh","Pred.Kwh")])

## There is high correlation between actual and predicted. Lets calculate other paramters for the model

val.RMSE <- round(sqrt(mean((10 ^ train.val$Pred.Kwh - 10 ^ train.val$Kwh)^2)))
val.MAE <- round(mean(abs(10 ^ train.val$Pred.Kwh - 10 ^ train.val$Kwh)))
c(val.corr,val.RMSE,val.MAE)

## We can see that correlation between Actual and Predicted KwH has imporved to 97%
## Root mean square with a 26 and Mean absolute error of 15 which is pretty good too



