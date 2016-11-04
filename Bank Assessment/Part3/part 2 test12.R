
library(rvest)
library(stringr)
library(dplyr)

## Reading the URL
content2 <- read_html("https://www.ffiec.gov/nicpubweb/content/BHCPRRPT/BHCPR_Peer.htm")

## Getting all the a tags and respective value
allHref <- (html_attr(html_nodes(content2, "a"), "href"))
allHref.df = as.data.frame(allHref)

##Filtering only PeerGroup_1 values
pattern = "*/PeerGroup_1_*"
PeerGroup1 <- filter(allHref.df, grepl(pattern,allHref.df$allHref))
str(PeerGroup1)

##Downloading all the pdf file
for(i in 1:nrow(PeerGroup1)){
  if(i == 1){
    a <- PeerGroup1$allHref[i]
    some <- paste("https://www.ffiec.gov/nicpubweb/content/BHCPRRPT/",a,sep="")
    print(some)
    split <- strsplit(some,"/")
    unlisted <- unlist(split)
    filename <- unlisted[end(unlisted)[1]]
    dest <- paste("E:\\Pdffiles\\",filename,sep="")
    download.file(some, destfile = dest ,mode = "wb",cacheOK = FALSE)
    break
  }
}



library(pdftools)
download.file("https://www.ffiec.gov/nicpubweb/content/BHCPRRPT/"+  ,mode = "wb",cacheOK = FALSE)

## Run till here only
## *********************************************************************************************##
library(rvest)
library(stringr)
library(dplyr)
content1 <- read_html("http://www.imdb.com/title/tt1490017/")

content2 <- read_html("https://www.ffiec.gov/nicpubweb/content/BHCPRRPT/BHCPR_Peer.htm")



content1 %>%
  html_nodes("#titleCast .itemprop span") %>% #  
  html_text()

content2 %>%
  html_nodes(".contentfull h4") %>% #  Class tag
  html_text()

content2 %>%
  html_nodes(".contentfull table div") %>% #  
  html_text()

content2 %>%
  html_nodes(".lightest a") %>% #  
  html_text()

content2 %>%
  html_nodes(".contentfull a table div") %>% #  
  html_text()

content2 %>%
  html_nodes("table .lightest a") %>% #  
  html_text()

content2 %>%
  html_nodes("table .lightest href a") %>% #  
  html_text()

allHref <- (html_attr(html_nodes(content2, "a"), "href"))

allHref.df = as.data.frame(allHref)
write.csv(allHref.df,"D:/Fall 16/ADS/Assignment 1/part 2/allHref.csv")
View(allHref.df)

pattern = "*/PeerGroup_1_*"
PeerGroup1 <- filter(allHref.df, grepl(pattern,allHref.df$allHref))
str(PeerGroup1)

library(pdftools)
download.file("https://www.ffiec.gov/nicpubweb/content/BHCPRRPT/"+, mode = "wb",cacheOK = FALSE)

