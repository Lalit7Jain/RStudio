#setwd("D:/Fall 16/ADS/Assignment 2/Assignment 2(1)")
##Reading the table
library(dplyr)
library(tidyr)
library(lubridate)
source("Function.R")
##after testing, variable names
## rm(list = 'd1')
##For loop for 2 dataset?
## Weekday issue in Pipline with function ?
rd1 <-  read.csv("rawData1.csv",header = TRUE)
rd2 <- read.csv("rawData2.csv",header = TRUE)

##Filter only rows with 'kwh' units
rd1 <-  rd1 %>% filter(Units == "kWh")
rd2 <-  rd2 %>% filter(Units == "kWh")

#check structure of Data Frame
str(head(rd2))

# Combining both the datasets, remove factors
data.combined <- rbind(rd1,rd2) 
data.combined[543:550,]

##convert to Date type
data.combined$Date <- as.Date(data.combined$Date,"%m/%d/%Y")
data.combined$Channel <- as.character(data.combined$Channel)
data.combined$Units <- as.character(data.combined$Units)

#Position of columns
columnpos <- grep("X",colnames(data.combined))
columnnamesval <- grep("X",colnames(data.combined),value = TRUE)
firstc = columnpos[1]
lastc = tail(columnpos,n=1)

##Gathering the data in long format
readData.long <- gather(data.combined,key,values,columnpos)

#write.csv(readData.long,file = "Longformat.csv")

##Function to find the Weekday
checkWeekday <- function(date){
  a = if((wday(date) == 6 || wday(date) == 7)) 0 else 1
  return(a)
}

aggData <- readData.long %>% group_by(Account,Date,Channel,Units) %>% 
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

##Gathering the columns into long format
aggData.long <- gather(aggData,hour,Kwh,5:28)
aggData.long$hour <- as.numeric(aggData.long$hour)

##Calculate peak hour
cal_PeakHour <- function(hour){
  p = if(hour > 6 & hour < 20) 1 else 0
  return(p)
}

aggData.long$PeakHour <- sapply(aggData.long$hour,function(x) cal_PeakHour(x))

### Scrapping the weather data from wunderground.com using weatherData library
#install.packages("weatherData")
library(weatherData)

mindate <- min(aggData.long$Date)
maxdate <- max(aggData.long$Date)

mindate <- as.Date("01/01/2014", "%m/%d/%Y")
maxdate <- as.Date("12/31/2014", "%m/%d/%Y")

getStationCode("Boston")
WeatherData <- getWeatherForDate("KBOS", start_date=mindate,
                                 end_date = maxdate,
                                 opt_detailed=T,opt_custom_columns=T,
                                 custom_columns=c(2:13))
temp <- WeatherData
WeatherData <- temp
WeatherData$date = date(WeatherData$Time)
WeatherData$hour = hour(WeatherData$Time)

# Writing weather data to CSV
write.csv(WeatherData,
          "WeatherData_new.csv")

WeatherDataCsv <- read.csv("WeatherData_new.csv", header = TRUE)

WeatherData <- WeatherDataCsv
#table(WeatherData$date)
#View(WeatherData[which(WeatherData$date == "2014-06-05"),])





# My observation by looking at the weather data at first glance
# we have got -999999 value in columns TempratureF, DewPointF, Sea_Level_PressureIn, Visibility MPH
## we need to covert our data to correct data type
# WindSpeed "Calm" which mean 0: Converting to character as it is in factor
WeatherData$date <-  as.Date(WeatherData$date,"%m/%d/%Y")
WeatherData$TemperatureF <- as.numeric(WeatherData$TemperatureF)
WeatherData$Dew_PointF <- as.numeric(WeatherData$Dew_PointF)
WeatherData$Sea_Level_PressureIn <- as.numeric(WeatherData$Sea_Level_PressureIn)
WeatherData$VisibilityMPH <- as.numeric(WeatherData$VisibilityMPH)
WeatherData$WindDirDegrees <- as.numeric(WeatherData$WindDirDegrees)
WeatherData$Humidity <- as.numeric(WeatherData$Humidity)

WeatherData$Wind_SpeedMPH[WeatherData$Wind_SpeedMPH == "Calm"] <- 0
WeatherData$Wind_SpeedMPH <- as.numeric(WeatherData$Wind_SpeedMPH)


## We need our data to fall in normal range to remove outliers
##                        MIN   MAX
##    TempF	                0	  100
##    DewPointF	          -20	  80
##    Humidity	           10	  100
##    Sea_Level_Pressure	 28	  32
##    Visibility	          0   10
##    Wind_Speed	          0 	50
#### Function to relace oulier value Attached 

## Temperature 
index <- which(WeatherData$TemperatureF < 0 | WeatherData$TemperatureF > 100)
for (i in index){
WeatherData$TemperatureF[i] = remove_out(WeatherData$TemperatureF,i,0,100)
}

## Dew Point
index <- which(WeatherData$Dew_PointF < -20 | WeatherData$Dew_PointF > 80)
for (i in index){
  WeatherData$Dew_PointF[i] = remove_out(WeatherData$Dew_PointF,i,-20,80)
}

## Humidity
index <- which(WeatherData$Humidity < 10 | WeatherData$Humidity > 100 | is.na(WeatherData$Humidity))
for (i in index){
  WeatherData$Humidity[i] = remove_out(WeatherData$Humidity,i,10,100)
}

## Wind_SpeedMPH
index <- which(WeatherData$Wind_SpeedMPH < 0 | WeatherData$Wind_SpeedMPH > 50 | is.na(WeatherData$Wind_SpeedMPH))
for (i in index){
  WeatherData$Wind_SpeedMPH[i] = remove_out(WeatherData$Wind_SpeedMPH,i,0,50)
}

## Sea_Level_Pressure
index <- which(WeatherData$Sea_Level_PressureIn < 28 | WeatherData$Sea_Level_PressureIn > 32 | is.na(WeatherData$Sea_Level_PressureIn))
for (i in index){
  WeatherData$Sea_Level_PressureIn[i] = remove_out(WeatherData$Sea_Level_PressureIn,i,28,32)
}

## VisibilityMPH
index <- which(WeatherData$VisibilityMPH < 0 | WeatherData$VisibilityMPH > 10 | is.na(WeatherData$VisibilityMPH))
for (i in index){
  WeatherData$VisibilityMPH[i] = remove_out(WeatherData$VisibilityMPH,i,0,10)
}

## WindDirDegree
index <- which(WeatherData$WindDirDegrees < 0 | WeatherData$WindDirDegrees > 360 | is.na(WeatherData$WindDirDegrees))
for (i in index){
  WeatherData$WindDirDegrees[i] = remove_out(WeatherData$WindDirDegrees,i,0,360)
}

##Function to get maximum time repeated value in a list
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


##Grouping and aggregations
Testdata <-WeatherData

WeatherData.Agg <- Testdata %>% 
  select(-c(Time,Gust_SpeedMPH,
            PrecipitationIn,Events)) %>%
  group_by(date,hour) %>% 
  summarise(TemperatureF = mean(TemperatureF),
                                    Dew_PointF = mean(Dew_PointF),
                                    Humidity = mean(Humidity),
                                    Sea_Level_PressureIn = mean(Sea_Level_PressureIn),
                                    VisibilityMPH = mean (VisibilityMPH),
                                    Wind_SpeedMPH = mean(Wind_SpeedMPH),
                                    WindDirDegrees = mean(WindDirDegrees),
            Conditions = names(table(Conditions))[which.max(table(Conditions))],
            Wind_Direction = names(table(Wind_Direction))[which.max(table(Wind_Direction))])


##Merge Data
mergeData <- merge(aggData.long,WeatherData.Agg,by.x = c("Date","hour"),by.y = c("date","hour"))

mergeData<- arrange(mergeData,Date,hour)

write.csv(mergeData,"MergedData.csv")

##Testing data

