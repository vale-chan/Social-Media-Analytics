require(httr)
require(jsonlite)
library(dplyr)
library(ggplot2)
library(hms)
library(lubridate)
library(quanteda)
library(reshape2)
library(scales)
library(syuzhet)
library(urltools)

#function for getting comments

comments <- function(name, key){
  url <- paste0('https://www.googleapis.com/youtube/v3/commentThreads?part=snippet&maxResults=100&videoId=',name,'&maxResults=50&key=',key)
  res<-GET(url)
  res<-rawToChar(res$content)
  res<- fromJSON(res)
  nexttoken<-res$nextPageToken
  result0<-res$items$snippet$topLevelComment$snippet$textOriginal
  
  while (!is.null(nexttoken)) {
    url2<-paste0(url,'&pageToken=',nexttoken)
    res<-GET(url2)
    result<-rawToChar(res$content)
    result<- fromJSON(result)
    result1<-result$items$snippet$topLevelComment$snippet$textOriginal
    nexttoken<-result$nextPageToken
    result0<-rbind(result0,result1)
  }
  return(data.frame(Comments = c(t(result0))))
}

com1 <- comments("SSrjAXK5pGw", "AIzaSyDjUAfD8u4RVt5AxiEHd7NjZT99KhanyJ4")
com2 <- comments("TCy_UOjEir0", "AIzaSyDjUAfD8u4RVt5AxiEHd7NjZT99KhanyJ4")
com3 <- comments("OhX2KQs3v5w", "AIzaSyDjUAfD8u4RVt5AxiEHd7NjZT99KhanyJ4")
#Video 4 has comments blocked, so no comment-analysis for Video 4


## SENTIMENT TABLE ##

##video1##
# Converting tweets to ASCII to trackle strange characters
com1_text <- com1$Comments
com1_text <- iconv(com1_text, from = "UTF-8", to = "ASCII", sub = "")

# removing retweets
com1_text <- gsub("(RT|via)((?:\\b\\w*@\\w+)+)", "", com1_text)

# removing mentions

com1_text <- gsub("@\\w+", "", com1_text)
sentiment1 <- get_nrc_sentiment((com1_text))
sentimentscores1 <- data.frame(colMeans(sentiment1[,0:8]))
names(sentimentscores1) <- "Score"
sentimentscores1 <- cbind("sentiment" = rownames(sentimentscores1), sentimentscores1)
rownames(sentimentscores1) <- NULL

ggplot(data = sentimentscores1, aes(x = sentiment, y = Score)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  scale_fill_discrete(name = "Emotion") +
  xlab("") + ylab("Average emotion scores") +
  theme_minimal() +
  theme(legend.position="bottom")


##video2##
# Converting tweets to ASCII to trackle strange characters
com2_text <- com2$Comments
com2_text <- iconv(com2_text, from = "UTF-8", to = "ASCII", sub = "")

# removing retweets
com2_text <- gsub("(RT|via)((?:\\b\\w*@\\w+)+)", "", com2_text)

# removing mentions
com2_text <- gsub("@\\w+", "", com2_text)
sentiment2 <- get_nrc_sentiment((com2_text))
sentimentscores2 <- data.frame(colMeans(sentiment2[,0:8]))
names(sentimentscores2) <- "Score"
sentimentscores2 <- cbind("sentiment" = rownames(sentimentscores2), sentimentscores2)
rownames(sentimentscores2) <- NULL

ggplot(data = sentimentscores2, aes(x = sentiment, y = Score)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  scale_fill_discrete(name = "Emotion") +
  xlab("") + ylab("Average emotion scores") +
  theme_minimal() +
  theme(legend.position="bottom")



##video3##
# Converting tweets to ASCII to trackle strange characters
com3_text <- com3$Comments
com3_text <- iconv(com3_text, from = "UTF-8", to = "ASCII", sub = "")

# removing retweets
com3_text <- gsub("(RT|via)((?:\\b\\w*@\\w+)+)", "", com3_text)

# removing mentions
com3_text <- gsub("@\\w+", "", com3_text)
sentiment3 <- get_nrc_sentiment((com3_text))
sentimentscores3 <- data.frame(colMeans(sentiment3[,0:8]))
names(sentimentscores3) <- "Score"
sentimentscores3 <- cbind("sentiment" = rownames(sentimentscores3), sentimentscores3)
rownames(sentimentscores3) <- NULL

ggplot(data = (sentimentscores3), aes(x = sentiment, y = Score)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  scale_fill_discrete(name = "Emotion") +
  xlab("") + ylab("Average emotion scores") +
  theme_minimal() +
  theme(legend.position="bottom")



