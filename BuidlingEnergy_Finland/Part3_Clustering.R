getClusterKMeans <- function(csv){
  
  csv <- read.csv("finland_allBuilding_allWeather.csv",stringsAsFactors = FALSE)
  csv <- name
  
  require(dplyr)
  mergeData <- csv %>% select(-c(date,consumption.Kwh.sqm,area_floor._m.sqr,vac,BuildingID,meternumb,type,City))
  
  ## Converting non numeric features to numeric
  mergeData$hour <- as.numeric(mergeData$hour)
  mergeData$Consumption <- as.numeric(mergeData$Consumption)
  mergeData$month <- as.numeric(mergeData$month)
  mergeData$Day.of.Week <-  as.numeric(mergeData$Day.of.Week)
  mergeData$weekday <-  as.numeric(mergeData$weekday)
  mergeData$Holiday <- as.numeric(mergeData$Holiday)
  mergeData$Base_Hour_Class <- as.factor(mergeData$Base_Hour_Class)
  
  #str(mergeData)
  
  ## Taking the subset of the data frame which only contains numeric features, removing categorical variables
  merge.sel <- subset(mergeData,select = -c(Conditions,Wind_Direction))
  
  ## Scaling the data before fitting the model so that convergence is possible for the neurons and fitting is valid
  maxs <- apply(merge.sel[,-9], 2, max) 
  mins <- apply(merge.sel[,-9], 2, min)
  scaled <- as.data.frame(scale(merge.sel[,-9], center = mins, scale = maxs - mins))
  
  ## Converting base hour class from high/Low to 1 and 0
  Base_Class <- as.numeric(merge.sel$Base_Hour_Class)-1
  data = cbind(Base_Class,scaled)
  
  ## Applying K-means
  k.means.fit <- kmeans(data[,-1],2)
  
  attributes(k.means.fit)
  k.means.fit$centers
  k.means.fit$size
  
  ## Applying Elbow function to decide number of clusters
  wssplot <- function(data, nc=15, seed=1234){
    wss <- (nrow(data)-1)*sum(apply(data,2,var))
    for (i in 2:nc){
      set.seed(seed)
      wss[i] <- sum(kmeans(data, centers=i)$withinss)}
    plot(1:nc, wss, type="b", xlab="Number of Clusters",
         ylab="Within groups sum of squares")}
  
  wssplot(data[,-1],nc=7)
  
  
  library(cluster)
  clusplot(data, k.means.fit$cluster, main='2D representation of the Cluster solution',
           color=TRUE, shade=TRUE,
           labels=2, lines=0)
          
}

table(data[,1],k.means.fit$cluster)
          
## Hierarchical Clustering

#Hierarchical methods use a distance matrix as an input for the clustering algorithm.
# The choice of an appropriate metric will influence the shape of the clusters, as some elements may be close to one another according to one distance and farther away according to another

d <- dist(data[,1], method = "euclidean") # Euclidean distance matrix.
          
#We use the Euclidean distance as an input for the clustering algorithm (Ward’s minimum variance criterion minimizes the total within-cluster variance):
H.fit <- hclust(d, method="ward.D")

#The clustering output can be displayed in a dendrogram
plot(H.fit) # display dendogram
groups <- cutree(H.fit, k=3) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(H.fit, k=3, border="red") 


##### Cluster Analysis -- All Buildings
require(dplyr)
mergeData <- csv %>% select(-c(X,date,vac,BuildingID,meternumb,Consumption,City))

## Converting non numeric features to numeric
mergeData$hour <- as.numeric(mergeData$hour)

mergeData$month <- as.numeric(mergeData$month)
mergeData$Day.of.Week <-  as.numeric(mergeData$Day.of.Week)
mergeData$weekday <-  as.numeric(mergeData$weekday)
mergeData$Holiday <- as.numeric(mergeData$Holiday)
mergeData$Base_Hour_Class <- as.factor(mergeData$Base_Hour_Class)
mergeData$type <- as.factor(mergeData$type)
mergeData$Base_hour_Flag <- as.numeric(mergeData$Base_hour_Flag)
mergeData$area_floor._m.sqr <- as.numeric(mergeData$area_floor._m.sqr)

#str(mergeData)

## Taking the subset of the data frame which only contains numeric features, removing categorical variables
merge.sel <- subset(mergeData,select = -c(Conditions,Wind_Direction))

## Scaling the data before fitting the model so that convergence is possible for the neurons and fitting is valid
maxs <- apply(merge.sel[,-c(2,11)], 2, max) 
mins <- apply(merge.sel[,-c(2,11)], 2, min)
scaled <- as.data.frame(scale(merge.sel[,-c(2,11)], center = mins, scale = maxs - mins))

## Converting base hour class from high/Low to 1 and 0
Base_Class <- as.numeric(merge.sel$Base_Hour_Class)-1

data = cbind(Base_Class,scaled)
data = cbind(data,Type = merge.sel$type)

wssplot(data[,-c(1,18)],nc=7)
k.means.fit <- kmeans(data[,-c(1,18)],4)
