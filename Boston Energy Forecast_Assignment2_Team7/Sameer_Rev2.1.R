#setwd("D:/Fall 16/ADS/Assignment 2/Assignment 2(1)")
##Reading the table
library(dplyr)
library(tidyr)
library(lubridate)
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

WeatherData$date = date(WeatherData$Time)
WeatherData$hour = hour(WeatherData$Time)

# Writing weather data to CSV
write.csv(WeatherData,
          "WeatherData_new.csv")

WeatherDataCsv <- read.csv("WeatherData_new.csv", header = TRUE)

WeatherData <- WeatherDataCsv
#table(WeatherData$date)
#View(WeatherData[which(WeatherData$date == "2014-06-05"),])

##Cleaning Weather Data
WeatherData$Wind_SpeedMPH[WeatherData$Wind_SpeedMPH == "Calm"] <- 0
#WeatherData$Wind_SpeedMPH[WeatherData$Wind_SpeedMPH < 0] <- 0
#WeatherData$TemperatureF[WeatherData$TemperatureF < 0] <- 0


##Function to get maximum time repeated value in a list
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


##Grouping and aggregations
Testdata <-WeatherData

testdata2 <- Testdata %>% select(-c(Time,Gust_SpeedMPH,PrecipitationIn,Events)) %>%
  group_by(date,hour) %>% summarise(TemperatureF = mean(TemperatureF),
                                    Dew_PointF = mean(Dew_PointF)) 


##Merge Data



##Testing data
test <- as.data.frame(c(1,2,0,4,0,0,5,6,8))
colnames(test) <- 'x'
test[which(test$x == 0),] <- test[(which(test$x == 0))-1,]

test


'
for(no of times ''0'' comes in the dataset){
if(pos-1 exist){
Replace with previos value
} else (pos+1 exist) {
Replace with next value
} 
}
}  '


# My observation by looking at the weather data at first glance
# we have got -999999 value in columns TempratureF, DewPointF, Sea_Level_PressureIn, Visibility MPH

# WindSpeed "Calm" which mean 0
WeatherData$Wind_SpeedMPH[WeatherData$Wind_SpeedMPH == "Calm"] <- 0
## we need to covert our data to correct data type
WeatherData$date <-  as.Date(WeatherData$date,"%m/%d/%Y")
WeatherData$TemperatureF <- as.numeric(WeatherData$TemperatureF)
WeatherData$Dew_PointF <- as.numeric(WeatherData$Dew_PointF)
WeatherData$Humidity <- as.numeric(WeatherData$Humidity)
WeatherData$Sea_Level_PressureIn <- as.numeric(WeatherData$Sea_Level_PressureIn)
WeatherData$VisibilityMPH <- as.numeric(WeatherData$VisibilityMPH)
WeatherData$Wind_SpeedMPH <- as.numeric(WeatherData$Wind_SpeedMPH)
WeatherData$WindDirDegrees <- as.numeric(WeatherData$WindDirDegrees)

## We need our data to fall in normal range to remove outliers
##                        MIN   MAX
##    TempF	                0	  100
##    DewPointF	          -20	  80
##    Humidity	           10	  100
##    Sea_Level_Pressure	 28	  32
##    Visibility	          0   10
##    Wind_Speed	          0 	44
##Function to relace oulier value
remove_out <- function(param,index,min_v,max_v){
  val = NULL
  val = param[index];
  if(val < min_v & val > max_v){
    if(index == 1){
      val = param[index+1]
    } 
    else if (index == 10386){
      val = param[index-1]
    } 
    else {
      val = param[index+1]
    }
    return(val) # Outlier removed Value return
  } 
  else{
  return(val)  #Normal Value return
  }
}

remove_out(WeatherData$TemperatureF,1,0,100)

### This is sample to know above function ####
print <- function(df,a){
  return(df[a])
}

print(WeatherData$TemperatureF,1)
##############################################