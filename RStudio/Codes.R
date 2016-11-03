#checking git coniguration
help(c)
help("paste")
nth <- paste0(1:12, c("st", "nd", "rd", rep("th", 9)))
nth
names_example <- c(1:4)
names(names_example) <- c("a","b","c","d")
names_example

#import csv file
dataGapMinder <- read.csv(file="C:\\Users\\lalit\\Dropbox\\NEU_Curriculum\\SEM4-Fall16\\Advances Data Science_Architecture\\R-tutorial\\gapminder-FiveYearData.csv", header = TRUE)
View(dataGapMinder)
str(dataGapMinder)
typeof(dataGapMinder[1,]) # Every observation is a list

#Something about matrix
x <- matrix(1:50, ncol=5, nrow=10)
x
x <- matrix(1:50, ncol=5, nrow=10,byrow = TRUE)
x

#Datatype and datastructures
dataTypes <- c('double', 'complex', 'integer', 'character', 'logical')
dataStructures <- c('data.frame', 'vector', 'factor', 'list', 'matrix')
answer <- list(dataTypes, dataStructures)
answer

#interesting dataframe (Adding rows which can create factor)
lalit <- data.frame(firstname = "lalit",lastname="jain",lucky = 7)
lalit
str(lalit)

lalit <- rbind(c("chetan","jain",9),lalit)
levels(lalit$firstname) <- c(levels(lalit$firstname),'chetan')
lalit <- rbind(c("chetan","jain",9),lalit)
lalit
lalit <- na.omit(lalit)
lalit

# Some operations on the dataGapMinder dataframe
head(dataGapMinder[5])
dataGapMinder[dataGapMinder$year == 1957,] #Extracting observations for year 1957, remeber the argument needs a row first then columns, since we only need row we will add ',' for all the columns
dataGapMinder[,-(1:4)]
dataGapMinder[dataGapMinder$lifeExp > 80,]
dataGapMinder[dataGapMinder$year == 2002 |  dataGapMinder$year == 2007,]

gapminder_small <- dataGapMinder[c(1:9, 19:23),]

gapminder_small



            