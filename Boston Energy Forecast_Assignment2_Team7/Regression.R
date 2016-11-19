## Remove Categorical variables for now
# 1) Feature Selection - Leaps Library (Exhaustive, Forward, Backward)
# 2) Cross Validation - 
# 3) Apply ML - Get values
# 4) Apply Regularization to check fit of the model (should not be overfit or underfit)
#http://blog.minitab.com/blog/adventures-in-statistics/regression-analysis-tutorial-and-examples
# https://rpubs.com/ryankelly/reg (Linear Model Selection & Regularization)

#####Part 3:
##Testing datahttp://r-statistics.co/Linear-Regression.html for ML
## http://www.statmethods.net/stats/regression.html 
## exhaustive search to select the best feature for the Model
## https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3478798/  --> Weather Prediction

## 
mergeData <- read.csv("MergedData.csv",header = TRUE)
str(mergeData)

require(dplyr)
mergeData <- mergeData %>% select(-c(X,Date,Account,Channel,Units,year))

mergeData$hour <- as.numeric(mergeData$hour)
mergeData$month <- as.numeric(mergeData$month)
mergeData$Day.of.Week <-  as.numeric(mergeData$Day.of.Week)
mergeData$weekday <-  as.numeric(mergeData$weekday)
mergeData$PeakHour <- as.numeric(mergeData$PeakHour)
mergeData$day <- as.numeric(mergeData$day)

str(mergeData)

#mergeData$conidtionsnew <- dummy.code(mergeData$Conditions)
#colnames(new)

merge.sel <- subset(mergeData,select = -c(Conditions,Wind_Direction))




##Checking for non linearity (visually) and transforming the variables
require(psych)
pairs.panels(merge.sel,col="red")
plot(density(merge.sel$month))
plot(density(log10(merge.sel$Kwh))) ##Feature Transformation 

merge.sel$Kwh <- log10(merge.sel$Kwh)


##### Use Leaps library to select features for Multiple Regression
#install.packages("leaps")
## 1) Feature Selection Exhasutive Search
library(leaps)

regsubsets.out <- regsubsets(Kwh ~ ., data = merge.sel, nvmax = 14,method = "exhaustive")
summary(regsubsets.out)
res <- summary(regsubsets.out)

plot(res$cp)
plot(regsubsets.out, scale = "Cp", main = "Cp")
plot(regsubsets.out, scale = "adjr2", main = "Adjusted R^2")
plot(regsubsets.out, scale = "bic", main = "BIC")
plot(res$adjr2)
coef(regsubsets.out,9)
#
#(Intercept)                month          Day.of.Week              weekday 
#         0.769778155         -0.006889214          0.036255448          0.148816158 
#            PeakHour         TemperatureF           Dew_PointF             Humidity 
#         0.305204241         -0.007290945          0.009760529         -0.004888410 
#Sea_Level_PressureIn        VisibilityMPH 
#        0.048695757         -0.006923492 

## 2) Feature Selection Forward Search
library(leaps)

regsubsets.fwd <- regsubsets(Kwh ~ ., data = merge.sel, nvmax = 14,method = "forward")
summary(regsubsets.fwd)
resfwd <- summary(regsubsets.fwd)

plot(resfwd$cp)
plot(regsubsets.fwd, scale = "Cp", main = "Cp")
plot(regsubsets.fwd, scale = "adjr2", main = "Adjusted R^2")
plot(regsubsets.fwd, scale = "bic", main = "BIC")
plot(resfwd$adjr2)
coef(regsubsets.fwd,9)

## 3) Feature Selection Backward Search
library(leaps)

regsubsets.bwd <- regsubsets(Kwh ~ ., data = merge.sel, nvmax = 14,method = "backward")
summary(regsubsets.bwd)
resbwd <- summary(regsubsets.bwd)


plot(regsubsets.bwd, scale = "Cp", main = "Cp")
plot(regsubsets.bwd, scale = "adjr2", main = "Adjusted R^2")
plot(regsubsets.bwd, scale = "bic", main = "BIC")
plot(resbwd$adjr2)
coef(regsubsets.bwd,9)

## 4) Feature Selection Stepwise Search
library(leaps)

regsubsets.bwd <- regsubsets(Kwh ~ ., data = merge.sel, nvmax = 14,method = "seqrep")
summary(regsubsets.bwd)
resbwd <- summary(regsubsets.bwd)

plot(resbwd$cp)
plot(regsubsets.bwd, scale = "Cp", main = "Cp")
plot(regsubsets.bwd, scale = "adjr2", main = "Adjusted R^2")
plot(regsubsets.bwd, scale = "bic", main = "BIC")
plot(resbwd$adjr2)
coef(regsubsets.bwd,9)


####
regsubsets.bwd <- regsubsets(Kwh ~ ., data = merge.sel, nvmax = 14,method = c("exhaustive","backward", "forward", "seqrep"))
summary(regsubsets.bwd)
resbwd <- summary(regsubsets.bwd)

plot(resbwd$cp)
plot(regsubsets.bwd, scale = "Cp", main = "Cp")
plot(regsubsets.bwd, scale = "adjr2", main = "Adjusted R^2")
plot(regsubsets.bwd, scale = "bic", main = "BIC")
plot(resbwd$adjr2)
coef(regsubsets.bwd,9)
####
### Develop a linear model
#   The model will be built using the training sample of the data
#   The model will be vaildated using the validation sample of the data

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


### Fit the model (1)
fit <- lm(Kwh ~ weekday + PeakHour + TemperatureF + Dew_PointF + Humidity + Sea_Level_PressureIn
          + VisibilityMPH + Wind_SpeedMPH + WindDirDegrees + Day.of.Week + hour, data = train.sample)

summary(fit) #R2=55.46% Adj R = 55.39%

##Diagnostic plots provide checks for heteroscedasticity, normality, and influential observerations
layout(matrix(c(1,2,3,4),2,2))
plot(fit) # read from http://data.library.virginia.edu/diagnostic-plots/ about the plots and put in the report

# Lets check the distribution of errors around the model
require(car)
car::crPlots(fit)


### Fit the model (2)
## Keeping only Dichotomous and numeric variables
fit <- lm(Kwh ~ weekday + PeakHour + TemperatureF + Dew_PointF + Humidity + Sea_Level_PressureIn
          + VisibilityMPH + Wind_SpeedMPH + WindDirDegrees, data = train.sample)

summary(fit) #R2=55.46% Adj R = 55.39%

##Diagnostic plots provide checks for heteroscedasticity, normality, and influential observerations
layout(matrix(c(1,2,3,4),2,2))
plot(fit)

# Lets check the distribution of errors around the model
car::crPlots(fit)



set.seed(7)
library(caret)
library(plyr)
library(dplyr)
library(randomForest)
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(mergeData[,c(9,10,12,13,14,15,16,17,18,19,20,21)], mergeData[,11], sizes=c(1:8), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))





library(car)
layout(matrix(1:2, ncol = 2))
## Adjusted R2
res.legend <-
  subsets(regsubsets.out, statistic="adjr2", legend = FALSE, min.size = 4, main = "Adjusted R^2")
## Mallow Cp
res.legend <-
  subsets(regsubsets.out, statistic="cp", legend = FALSE, min.size = 5, main = "Mallow Cp")
abline(a = 1, b = 1, lty = 2)

cor(mergeData$Kwh,mergeData$PeakHour)
plot(mergeData$Kwh,mergeData$PeakHour)
scatter.smooth(x=mergeData$Kwh, y=mergeData$PeakHour, main="Kwh ~ PeakHour")
linearMod <- lm(Kwh ~ PeakHour, data = mergeData)
summary(linearMod)
print(linearMod)
