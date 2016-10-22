## Wordcloud (inaugrual speeches in quandteda package)
summary(inaugTexts)
require(quanteda)
dv <- data.frame(Party = c('democrats','democrats','republican','republican','democrats','democrats'))
recentCorpus <- corpus(inaugTexts[52:57], docvars=dv)
summary(recentCorpus)
partyDfm <- dfm(recentCorpus, groups='Party', ignoredFeatures=(stopwords('english')))
wordcloud::comparison.cloud(t(as.matrix(partyDfm)),n=100)

#We can analyze the **lexical diversity** of texts, using `lexdiv()` on a dfm:
# How many different words are used in the document complexity wise
setwd("C:/Users/lalit/Dropbox/NEU_Curriculum/SEM4-Fall16/Advances Data Science_Architecture/GitLocal/TextAnalytics_Demo/3_file_import/")
mytf <- textfile("inaugural/*.txt", docvarsfrom="filenames", dvsep="-", docvarnames=c("Year", "President"))
str(mytf)
inaugCorpus <- corpus(mytf)                   
myDfm <- dfm(subset(inaugCorpus, Year > 1980), verbose = FALSE)
lexdiversity <- lexdiv(myDfm, "R") ## 'R' for Guirad's root TTR (R = V/sqrt(N))
dotchart(sort(lexdiversity),color = "red")

#We can analyze the **readability** of texts, using `readability()` on a vector of texts or a corpus:
readab <- readability(subset(inaugCorpus, Year > 1980), measure = "Flesch.Kincaid")
#fk <- readability(SOTUCorpus, "Flesch.Kincaid")
dotchart(sort(readab))


#We can **identify documents and terms that are similar to one another**, using `similarity()`:
## Presidential Inaugural Address Corpus
presDfm <- dfm(inaugCorpus, ignoredFeatures = stopwords("english"))
# compute some document similarities
similarity(presDfm, c("2009-Obama.txt" , "2013-Obama.txt"), n=5, margin="documents", method = "cosine")
similarity(presDfm, c("2009-Obama.txt" , "2013-Obama.txt"), n=5, margin="documents", method = "Hellinger")
similarity(presDfm, c("2009-Obama.txt" , "2013-Obama.txt"), n=5, margin="documents", method = "eJaccard")

##And this can be used for **clustering documents**:
#x <- matrix(rnorm(100), nrow = 5)
#dist(x)

View(SOTUCorpus$documents)

data(SOTUCorpus, package="quantedaData")

presDfm <- dfm(subset(SOTUCorpus, lubridate::year(Date)>1981), verbose=FALSE, stem=TRUE,
               ignoredFeatures=stopwords("english", verbose=FALSE))
presDfm <- trim(presDfm, minCount=5, minDoc=3)
# hierarchical clustering - get distances on normalized dfm
presDistMat <- dist(as.matrix(weight(presDfm, "relFreq")))
# hiarchical clustering the distance object
presCluster <- hclust(presDistMat)
# label with document names
presCluster$labels <- docnames(presDfm)
# plot as a dendrogram
plot(presCluster)


## Tweet Analysis 
require(quanteda)
setwd("C:/Users/lalit/Dropbox/NEU_Curriculum/SEM4-Fall16/Advances Data Science_Architecture/GitLocal/TextAnalytics_Demo/6_advanced")
load("tweetSample.RData")
View(tweetSample)


require(lubridate)
require(dplyr)
tweetSample <- mutate(tweetSample, day = yday(created_at))
tweetSample <- mutate(tweetSample, dayDate = as.Date(day-1, origin = "2014-01-01"))
juncker <- filter(tweetSample, grepl('juncker', text, ignore.case=TRUE)) %>% mutate(cand='Juncker')
schulz <- filter(tweetSample, grepl('schulz', text, ignore.case=TRUE))  %>% mutate(cand='Schulz')
verhof <- filter(tweetSample, grepl('verhofstadt', text, ignore.case=TRUE)) %>% mutate(cand='Verhofstadt')
spitzAll <- bind_rows(juncker, schulz, verhof)


require(ggplot2)
require(scales)
# mentioning candidates names over time
plotDf <- count(spitzAll, cand, day=day)  %>% mutate(day=as.Date(day-1, origin = "2014-01-01"))

ggplot(data=plotDf, aes(x=day, y=n, colour=cand)) + 
  geom_line(size=1) +
  scale_y_continuous(labels = comma) + geom_vline(xintercept=as.numeric(as.Date("2014-05-15")), linetype=4) +
  geom_vline(xintercept=as.numeric(as.Date("2014-05-25")), linetype=4) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

##
require("lubridate")
year <- lubridate::year(docvars(SOTUCorpus, "Date"))
require(ggplot2)
partyColours <- c("blue", "blue", "black", "black", "red", "red")
p <- ggplot(data = docvars(SOTUCorpus), aes(x = year, y = fk)) + #, group = delivery)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  geom_smooth(alpha=0.2, linetype=1, color="grey70", method = "loess", span = .34) +
  xlab("") +
  ylab("Flesch-Kincaid") +
  geom_point(aes(colour = party)) +
  scale_colour_manual(values = partyColours) +
  geom_line(aes(), alpha=0.3, size = 1) +
  ggtitle("Text Complexity in State of the Union Addresses") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))
#quartz(height=7, width=12)
print(p)

