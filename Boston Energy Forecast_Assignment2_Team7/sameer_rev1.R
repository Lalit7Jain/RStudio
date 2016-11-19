setwd("D:/Fall 16/ADS/Assignment 2/Assignment 2(1)")
library(dplyr)
library(weatherData)
library(lubridate)

rd1 <- read.csv(file="RawData1.csv", head = TRUE)
View(rd1)
str(rd1)

# extract only kWh data
rd1.df <- as.data.frame(filter(rd1, rd1$Units == "kWh"))
View(rd1.df)

#convert date column datatype

rd1.df$Date_f <- as.Date(rd1.df$Date, "%m/%d/%Y")
str(rd1.df$Date_f)
library(lubridate)

rd1.df$day -> wday(rd1.df$Date_f)

cal_weekday <- function(DAY_arg){
  if (DAY_arg == 6 || DAY_arg == 7)
    return(0)
  else
    return(1)
}

cal_weekday(4)

rd1.df$day
sapply(rd1.df$day, cal_weekday(rd1.df$day[1]))




#### Getting Temperture Details 
#library(devtools)
library(weatherData)
getStationCode("Boston")

tempData <- getWeatherForDate("KBOS", start_date=min(td1.df$Date),
                              end_date = max(td1.df$Date),
                              opt_detailed=T,
                              opt_custom_columns=T, custom_columns=c(2))


WeatherData <- getWeatherForDate("KBOS", start_date='2014-01-01',
                              end_date = '2014-01-30',
                              opt_detailed=T,
                              opt_custom_columns=T, custom_columns=c(2:13))


WeatherData$date = date(WeatherData$Time)
WeatherData$hour = hour(WeatherData$Time)
str(WeatherData)
