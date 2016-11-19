


daycount <- table(rawdata.agg$date)

# generate vector of all dates
alldays <- seq(rawdata$date[1],length=342,by="+1 day")
# notice how R recognizes the date format and chooses the appropriate method for the seq() function. 
# You don't have to worry about 30 days in June, 31 in July etc. R does it all for you.
allcount <- table(alldays)

actindex <- match(names(allcount),names(daycount),nomatch = 0)

for (i in 1:33){
  
}
i = 1
building <- paste("Building",i)
subset <- rawdata[which(rawdata$vac == building),]


var = 24
year = 2013
url = paste("http://www.timeanddate.com/calendar/?year=",year,"&country=",var,sep = "")
raw.data <- readLines(url,warn = "F")
install.packages("rjson")
require(rjson)
rd  <- fromJSON(raw.data)
rd

require(RCurl)
require(XML)
var <- 24
year <- 2013
url <- paste("http://www.timeanddate.com/calendar/?year=",year,"&country=",var,sep = "")
SOURCE <-getURL(url,encoding = "UTF-8")
PARSED <- htmlParse(SOURCE)
xpathSApply(PARSED, "//span[@class='co1']",xmlValue)
xpathSApply(PARSED, "//table[@id='ISO-8859-1’']",xmlValue)

getUrl <- function(address,sensor = "false") {
  root <- "http://maps.google.com/maps/api/geocode/json?"
  u <- paste0(root,"address=", address, "&sensor=false")
  return(URLencode(u))
}
getUrl("Otakaari 1")

getUrlAirport <- function(lat,long) {
  root <- "http://api.wunderground.com/api/3a3f6a4875014282/geolookup/q/"
  u <- paste0(root,lat,",",long,".json")
  return(URLencode(u))
}


install.packages("RJSONIO")
require(RJSONIO)
target <- getUrlAirport(60.17178,24.92314)
target <- getUrlAirport(60.17991,24.83185)
dat <- fromJSON(target)
airports <- dat$location$nearby_weather_stations$airport$station
airports
q = NULL
for (i in 1:length(airports)){
    q <- rbind(q,airports[[i]])
  
}
q <- as.data.frame(q,stringsAsFactors = FALSE)



getdistance(60.18625,24.8284305,60.25017166,25.04480934)

q$city[q$latdif == min(as.numeric(q$latdif))]
q[which.min(q$londiff),]["city"]

city <- getAirpot(60.18625,24.8284305)


require(weatherData)
a <- getStationCode("EFFHF")
a
b <- NA

mindate <- as.Date("01/01/2013", "%m/%d/%Y")
maxdate <- as.Date("12/08/2013", "%m/%d/%Y")

WeatherTest <- getWeatherData("EFHK",mindate=mindate,maxdate = maxdate)
#Helsinki EFHK
#malmi EFHF


tmp <- read.csv("finlandEnergy.csv",stringsAsFactors = FALSE)
str(tmp)

FinlandEngergy_City <- merge(finlandEnergy,address.geocode,by.x = "vac",by.y = "building")
FinlandEngergy_City$City <- as.character(FinlandEngergy_City$City)

require(weatherData)
getStationCode("Lohja Porla")



#########

## Lets get the station code for Boston
requi#Converting into date
mindate <- as.Date("01/01/2013", "%m/%d/%Y")
maxdate <- as.Date("01/02/2013", "%m/%d/%Y")


code <- getStationCode("EFHK")


d= 5
for(i in 1:2) 
{ 
  nam <- stationcode[[i]]
  assign(nam, rnorm(3)+d)
}



getGeoCode("Taj Mahal")


root <- "http://maps.google.com/maps/api/geocode/json?"
u <- paste0(root,"address=", "1234, Taj Mahal, China", "&sensor=false&key=AIzaSyBmO7kN8BXjz_prZnTFauynxpr4eHsADosA")
target <- URLencode(u)
dat <- fromJSON(target)

d <- (strsplit("Building 15",split = " ")[[1]])[2]


colnames(`Building 1.5198.1`)[which(names(`Building 1.5198.1`) == "consumption Kwh/sqm")] <- "consumption.Kwh.sqm"
test <- returnRandomForest(csv = `Building 1.5198.1`,ntree = 150)

linearmodel <- as.data.frame(NULL)
m = 0
for(i in Buildinglist){
  m = m + 1
  build <- unique(i["vac"])
  value <- names(Buildinglist)[m]
  if(exists(paste(build,".",value,sep = ""))){
    name <- get(paste(build,".",value,sep = ""))
    colnames(name)[which(names(name) == "consumption Kwh/sqm")] <- "consumption.Kwh.sqm"
    modelcoeff <- returnLinearModel(csv = name)
    linearmodel <- rbind(linearmodel,modelcoeff)
  }
}
write.table(linearmodel,"linearModel_All.csv", sep = "," )
write.csv(linearmodel,"linearModel_All1.csv")


