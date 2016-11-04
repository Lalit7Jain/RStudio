
## About the data,tell about the units, channel = Mildred School which we are interested in finding the KwH usage
## Observations where made at every 5 minute of time
## city of Boston has hired you to build a forecasting model to predict their energy usage

### Goal
### We will work on 3 parts as
# 1) Energy Usage of Mildred School
# 2) Weather data from wunderground.com for that particular year
# 3) Use Muliple linear regression based on best possible feature and build the model which can predict the energy usage in Boston with highest accuracy

#####Part 1) Energy Usage of Mildred School
## Reading the Data
rd1 <-  read.csv("rawData1.csv",header = TRUE)
rd2 <- read.csv("rawData2.csv",header = TRUE)
View(rd1)
head(rd1)

##from the data, here are some of the features and there description. We will also focus on brining or deriving addditional features as described in the table below (table from the Assignment)


##Since we are interested in only kwh units for Mildred School 
##Filter only rows with 'kwh' units
rd1 <-  rd1 %>% filter(Units == "kWh",Channel  == "MILDRED SCHOOL 1")
rd2 <-  rd2 %>% filter(Units == "kWh",Channel  == "MILDRED SCHOOL 1")
head(rd1)

#check structure of Data Frame
str(head(rd1))

# Combining both the datasets
data.combined <- rbind(rd1,rd2) 
data.combined[543:550,]

##convert to Date type and changing factor columns to character
data.combined$Date <- as.Date(data.combined$Date,"%m/%d/%Y")
data.combined$Channel <- as.character(data.combined$Channel)
data.combined$Units <- as.character(data.combined$Units)
str(head(data.combined))

## As we can see that the data is widely spread and for any analysis to be done we need to convert the data into long format
## We will first find the poisitions of the columns and also save the columnnames into array


#Position of columns
columnpos <- grep("X",colnames(data.combined))
head(columnpos)

##This wills store the names of the columns for the observations
columnnamesval <- grep("X",colnames(data.combined),value = TRUE)
head(columnpos)

## Gathering the data will use this libraries for more information refer https://cran.rstudio.com/web/packages/dplyr/vignettes/introduction.html

library(dplyr)
library(tidyr)

##Gathering the data in long format
readData.long <- gather(data.combined,key,values,columnpos)
head(readData.long)

## Now we have the data in the long format we can apply some aggregations on the data for each hour
## which will comprise of 12 observations for each hour
## we will create 24 new columns which will be sum of 12 observations of each hour using dplyr summarize and group by function

##Function to find the Weekday
checkWeekday <- function(date){
  a = if((wday(date) == 6 || wday(date) == 7)) 0 else 1
  return(a)
}

## Will use dplyr chaining function to group the data first and summarize the columns for each hour using the 12 observations/each hour
## ** Say about groupby, summarise, mutate using the documentation
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

head(aggData)

##Gathering the column again in long format so that data is in clean consistent format
## This way we have the kWH value for every hour and for every day of the year 2014
aggData.long <- gather(aggData,hour,Kwh,5:28)
head(aggData.long)

##We will change the hour column into numeric
aggData.long$hour <- as.numeric(aggData.long$hour)

##Now lets derive peak hour. Using the table definition we can define a function based on hours like below

##Calculate peak hour
cal_PeakHour <- function(hour){
  p = if(hour > 6 & hour < 20) 1 else 0
  return(p)
}

aggData.long$PeakHour <- sapply(aggData.long$hour,function(x) cal_PeakHour(x))

## So we now we have all the data in the format for further analysis

#####Part 2) Weather data from wunderground.com for that particular year
## We will use the WeatherData Package which use wunderground.com to...blah blah

### Scrapping the weather data from wunderground.com using weatherData library
#install.packages("weatherData")
library(weatherData)

##Finding minium and max date for our observed dataset
mindate <- min(aggData.long$Date)
maxdate <- max(aggData.long$Date)

#Converting into date
mindate <- as.Date("01/01/2014", "%m/%d/%Y")
maxdate <- as.Date("12/31/2014", "%m/%d/%Y")

## Lets get the station code for Boston
getStationCode("Boston")

## <<Talk about the function and its argument>>. It will take some time to load the data
WeatherData <- getWeatherForDate("KBOS", start_date=mindate,
                                 end_date = maxdate,
                                 opt_detailed=T,opt_custom_columns=T,
                                 custom_columns=c(2:13))

## Data looks like this
head(WeatherData)

##Lets use Lubridate package to find the date and hour
library(lubridate)

WeatherData$date = date(WeatherData$Time)
WeatherData$hour = hour(WeatherData$Time)

View(WeatherData)

## about the features, create a table and define the maximum and mimimum values for the features based on websites

## We can see that WeatherData api is pulling information for each hour interval. However to be very sure lets check this assumption
table(WeatherData$date)

##We can see that it is not the case, most of the days we had 24 observations where as some of them have more than 24 observations
## So whats the matter here, close look reveals that in some instances observations were taken more than once for each hour, for example here

View(WeatherData[which(WeatherData$date == "2014-06-05"),])


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

#### Function to relace oulier value Attached. We will use the approach of subsituting the previous or the next value of the observation
## For example, if the record 8999 has Temperature as -9999 we will use the record of 8998 so that this is still acceptable

## Tell something about the function

remove_out <- function(param,index,min_v,max_v){
  val = NULL
  val = param[index]
  if(val < min_v | val > max_v | is.na(val)){
    if(index-1 >= 1){
      val = param[index-1]
    } else if (index-1 <= 0){
      val = param[index+1]
    } 
    return(val)
  } else{
    print("Nothing changed")
    return(val)  #Normal Value return
  }
}

## Lets remove some outliers for Temperature. We will find out the records where Temperature is falling out of the range defined in the table


## Temperature
index <- which(WeatherData$TemperatureF < 0 | WeatherData$TemperatureF > 100 | is.na(WeatherData$Dew_PointF))
print(index)

## Lets just check that record, whats with that records
WeatherData[8206,]


## It seems we got it right and it is indeed an outlier. May be a machine input error. Lets remove this with out function and check the record again after the function 
for (i in index){
WeatherData$TemperatureF[i] = remove_out(WeatherData$TemperatureF,i,0,100)
}
WeatherData[8206,]

##Wow this looks good. Lets do the same for other features

## Dew Point
index <- which(WeatherData$Dew_PointF < -20 | WeatherData$Dew_PointF > 80 | is.na(WeatherData$Dew_PointF))
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

## Now since the data is clean, we can aggregate the dataset as done in part 1, so that we get records for each hour and we can take average values for numeric values and frequency count for character values
## Steps involved 1) Remove non essential features like Time, Gust_speedMH,P,E
## 2) Group the data by Date and hour 
## 3) summarise base on mean and frequency count



WeatherData.Agg <- WeatherData %>% 
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

head(WeatherData.Agg)

##Now the data looks good with the format we want
## Lets merge the data with part 1 of the energy usage data by Date and hour

##Merge Data
mergeData <- merge(aggData.long,WeatherData.Agg,by.x = c("Date","hour"),by.y = c("date","hour"))
head(mergeData)

##Arranging the data by Date and hour
mergeData<- arrange(mergeData,Date,hour)
head(mergeData)

write.csv(mergeData,"MergedData.csv")


#####Part 3:
##Testing datahttp://r-statistics.co/Linear-Regression.html
cor(mergeData$Kwh,mergeData$PeakHour)
plot(mergeData$Kwh,mergeData$PeakHour)
scatter.smooth(x=mergeData$Kwh, y=mergeData$PeakHour, main="Kwh ~ PeakHour")
linearMod <- lm(Kwh ~ PeakHour, data = mergeData)
summary(linearMod)
print(linearMod)
