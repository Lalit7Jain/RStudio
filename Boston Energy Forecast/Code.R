setwd("D:/Fall 16/ADS/Assignment 2/Assignment 2(1)")
##Reading the table
library(dplyr)
library(tidyr)
library(lubridate)
##after testing, variable names
##For loop for 2 dataset?
## Weekday issue in Pipline with function ?
rd1 = as.data.frame(read.csv("rawData1.csv",header = TRUE))
rd2 = as.data.frame(read.csv("rawData2.csv",header = TRUE))

##Filter only rows with 'kwh' units
d1 <- filter(rd1,Units == "kWh")
d2 <- filter(rd2,Units == "kWh")

#check structure of Data Frame
str(rd1)

# Combining both the datasets, remove factors
 

## ----------------- this part below not working ---- TEST ------------
##convert to Date type
readData1$Date <- as.numeric(rd1$Date,"%m/%d/%Y")
readData2$Date <- as.numeric(rd2$Date,"%m/%d/%Y")
d1$Channel <- as.character(d1$Channel)
d2$Channel <- as.character(d2$Channel)
readData1$Units <- as.character(rd1$Units)
readData2$Units <- as.character(rd2$Units)


  View(x <- rbind.data.frame(rd1[,3] + rd2[,3]))
View(rbind.data.frame(readData1[,2] + readData2[,2]))

readData1[,2]
## ----------------- this part above not working ---- TEST ------------

#Converting Data column after append
d1$Date <- as.Date(d1$Date,"%m/%d/%Y")
d2$Date <- as.Date(d2$Date,"%m/%d/%Y")

#Position of columns
columnnames <- grep("X",colnames(d1))
columnnamesval <- grep("X",colnames(d1),value = TRUE)
firstc = columnnames[1]
lastc = tail(columnnames,n=1)

readData.long <- gather(d1,key,values,columnnames)
#write.csv(readData.long,file = "Longformat.csv")

checkWeekday <- function(date){
  a = if((wday(date) == 6 || wday(date) == 7)) 0 else 1
  return(a)
}

newtest <- readData.long %>% group_by(Account,Date,Channel,Units) %>% 
  summarise('0' = sum(values[key %in% columnnamesval[1:12]]),
            '1' = sum(values[key %in% columnnamesval[13:24]]),
            '2' = sum(values[key %in% columnnamesval[25:36]]),
            '3' = sum(values[key %in% columnnamesval[37:48]]),
            '4'  = sum(values[key %in% columnnamesval[49:60]]),
            '5' = sum(values[key %in% columnnamesval[61:72]]),
            '6' = sum(values[key %in% columnnamesval[73:84]]),
            '7' = sum(values[key %in% columnnamesval[85:96]]),
            '8' = sum(values[key %in% columnnamesval[97:108]]),
            '9' = sum(values[key %in% columnnamesval[109:120]]),
            '10' = sum(values[key %in% columnnamesval[121:132]]),
            '11' = sum(values[key %in% columnnamesval[133:144]]),
            '12' = sum(values[key %in% columnnamesval[145:156]]),
            '13' = sum(values[key %in% columnnamesval[157:168]]),
            '14' = sum(values[key %in% columnnamesval[169:180]]),
            '15' = sum(values[key %in% columnnamesval[181:192]]),
            '16' = sum(values[key %in% columnnamesval[193:204]]),
            '17' = sum(values[key %in% columnnamesval[205:216]]),
            '18' = sum(values[key %in% columnnamesval[217:228]]),
            '19' = sum(values[key %in% columnnamesval[229:240]]),
            '20' = sum(values[key %in% columnnamesval[241:252]]),
            '21' = sum(values[key %in% columnnamesval[253:264]]),
            '22' = sum(values[key %in% columnnamesval[265:276]]),
            '23' = sum(values[key %in% columnnamesval[277:288]])) %>%
  mutate(month = lubridate::month(Date),day = day(Date),year = year(Date),'Day of Week' = wday(Date)) %>%
  mutate(weekday = sapply(Date, function(x) checkWeekday(x)))         

newtest2 <- gather(newtest,hour,Kwh,5:28)
newtest2$hour <- as.numeric(newtest2$hour)

##Calculate peak hour
cal_PeakHour <- function(hour){
  p = if(6 < hour & hour< 20) 1 else 0
  return(p)
}

newtest2$PeakHour <- sapply(newtest2$hour,function(x) cal_PeakHour(x))

#install.packages("weatherData")
library(weatherData)
getStationCode("Boston")
WeatherData <- getWeatherForDate("KBOS", start_date=min(newtest2$Date),
                                 end_date = max(newtest2$Date),
                                 opt_detailed=T,opt_custom_columns=T,
                                 custom_columns=c(2:13))


WeatherData$date = date(WeatherData$Time)
WeatherData$hour = hour(WeatherData$Time)
str(WeatherData)                        








