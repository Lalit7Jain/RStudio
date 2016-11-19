mydata <- read.csv("MergedData - MLR.csv",TRUE)
View(mydata)
str(mydata)

# I need to convert data type o continues type as lm() supports only continues variables
mydata$Date <- as.Date(mydata$Date,"%m/%d/%Y")
mydata$PeakHour <- as.integer(mydata$PeakHour)

#The lm(), or "linear model," function can be used to create a multiple regression model.
fit <- lm(mydata$Kwh ~
            mydata$hour+
            mydata$month+
            mydata$day+
            mydata$Day.of.Week+
            mydata$weekday+
            mydata$PeakHour+
            mydata$TemperatureF+
            mydata$Dew_PointF+
            mydata$Humidity+
            mydata$Sea_Level_PressureIn+
            mydata$VisibilityMPH+
            mydata$Wind_SpeedMPH+
            mydata$WindDirDegrees, data=mydata)

# The summary() function provide us with t-test, F-test, R-squared, residual, 
# and significance values.
summary(fit)

# Step 1: Feature Selection

