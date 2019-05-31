#load packages

library(dplyr)
library(ggplot2)
library(hms)
library(lubridate)
library(quanteda)
library(reshape2)
library(scales)
library(syuzhet)
library(purrrlyr)



##SENTIMENT ANALYSIS - PREPARE DATA##

#load & remove data
tweets <- read.csv("climateChangeSample.csv", stringsAsFactors = F)
#create new "date" column & eliminate december
tweets$date = substr(tweets$created_at,1,10)

tweets <- filter(tweets, as.Date(date) > as.Date("2018-12-31"))

#create datasets for each month
tweetsJan <- filter(tweets, as.Date(date) > as.Date("2018-12-31") & as.Date(date) < as.Date("2019-02-01"))
tweetsFeb <- filter(tweets, as.Date(date) > as.Date("2019-01-31") & as.Date(date) < as.Date("2019-03-01"))
tweetsMar <- filter(tweets, as.Date(date) > as.Date("2019-02-28") & as.Date(date) < as.Date("2019-04-01"))
tweetsApr <- filter(tweets, as.Date(date) > as.Date("2019-03-31") & as.Date(date) < as.Date("2019-05-01"))

#cleaning data
text <- tweets$text
tweets_dfm <- dfm(text, remove_punct = T, remove_url = T, remove_numbers = T, remove_symbols = T, remove = stopwords("en"))
View(tweets_dfm)
#get ready for sentiment analysis
afinn <- readRDS("afinn.rds")
tweets_afinn <- dfm_lookup(tweets_dfm, dictionary = afinn)

#prepare sentiment scoring
quanteda::convert(tweets_afinn, to = "data.frame") %>%
  mutate(afinn_score = (neg5 * -5) + (neg4 * -4) + (neg3 * -3) + (neg2 * -2) + (neg1 * -1) + (zero * 0) + (pos1 * 1) + (pos2 * 2) + (pos3 * 3) + (pos4 * 4) + (pos5 * 5)) %>%
  select(afinn_score) -> afinn_score

#add score as new column to original data
tweets$sentimentScore <- afinn_score$afinn_score



## SENTIMENT TABLE ##
# Converting tweets to ASCII to trackle strange characters
tweets_text <- tweets$text
tweets_text <- iconv(tweets_text, from="UTF-8", to="ASCII", sub="")

# removing retweets
tweets_text<-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets_text)

# removing mentions
tweets_text <- gsub("@\\w+","",tweets_text)
sentiment <- get_nrc_sentiment((tweets_text))
sentimentscores <- data.frame(colSums(sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL

ggplot(data = sentimentscores,aes(x = sentiment, y = Score)) +
  geom_bar(aes(fill = sentiment),stat = "identity") +
  theme(legend.position ="none") +
  xlab("Sentiments") + ylab("Scores") +
  ggtitle("Total sentiment based on scores") +
  theme_minimal()



## SPECIFIC SENTIMENTS OVER TIME ##
tweets$sentiments <- sentiment

#emotion over time
monthlySentiment <- tweets %>%
  group_by(month(created_at, label = TRUE)) %>%
  summarise(anger = mean(sentiments$anger),
            anticipation = mean(sentiments$anticipation),
            disgust = mean(sentiments$disgust),
            fear = mean(sentiments$fear),
            joy = mean(sentiments$joy),
            sadness = mean(sentiments$sadness),
            surprise = mean(sentiments$surprise),
            trust = mean(sentiments$trust)) %>%
  melt

names(monthlySentiment) <- c("month", "sentiment", "meanvalue")

ggplot(data = monthlySentiment,
       aes(month, y = meanvalue, group = sentiment, color = sentiment))+
  geom_line() +
  geom_point() +
  labs(x = "Month", colour = "Month") +
  xlab("Month") + ylab("Number of tweets") +
  theme_minimal()

#positive and negative sentiments over time
posNegSentiment <- tweets %>%
  group_by(month(created_at, label = TRUE)) %>%
  summarise(negative = mean(sentiments$negative),
            positive = mean(sentiments$positive)) %>%
  melt

names(posNegSentiment) <- c("month", "sentiment", "meanvalue")

ggplot(data = posNegSentiment,
       aes(month, y = meanvalue, group = sentiment, color = sentiment))+
  geom_line() +
  geom_point() +
  labs(x = "Month", colour = "Month") +
  xlab("Month") + ylab("Number of tweets") +
  theme_minimal()

#daily sentimental change
#positive
posNegDaily <- tweets %>%
  group_by(date) %>%
  summarise(positive = mean(sentiments$positive),
            negative = mean(sentiments$negative)) %>%
  melt

names(posNegDaily) <- c("days", "sentiment", "meanvalue")

ggplot(data = posNegDaily,
       aes(days, y = meanvalue, group = sentiment, color = sentiment))+
  geom_line() +
  geom_point() +
  labs(x = "Sentiment", colour = "Sentiment") +
  xlab("Day") + ylab("Number of tweets") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



## SENTIMENT ANALYSIS - SENTIMENT OVER TIME ##
#create new "date" column
tweets$date = substr(tweets$created_at,1,10)

#sentiment change over time
tweets %>%
  group_by(date) %>%
  summarise(avgSentiment = mean(sentimentScore)) -> sentimentOverTime

ggplot(data = sentimentOverTime, aes(x = date, y = avgSentiment, group = 1)) +
  geom_line(aes(color = 'pink'), size = 1) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



### HASHTAG ANALYSIS ###

tweets$tags_split <- lapply(tweets$hashtags, function(tags) strsplit(tags, ' '))

##MOST USED HASHTAGS##
# Getting the hashtags from the list 
tags_split <- unlist(tweets$tags_split)

# Formatting by removing the white spacea
tags <- sapply(tags_split, function(y) nchar(trimws(y)) > 0 & !is.na(y))
tag_df <- as.data.frame(table(tolower(tags_split[tags])))
as_tibble(strsplit("this is a test" , " ")[[1]])
tag_df <- tag_df[order(-tag_df$Freq),]
tag_df <- tag_df[1:10,]

ggplot(tag_df, aes(x = reorder(Var1,-Freq), y = Freq)) +
  geom_bar(stat="identity", fill="darkslategray")+
  theme_minimal() + 
  xlab("#Hashtags") + ylab("Count")


#versuch lukas

daily_tags <- aggregate(
    tweets$hashtags,
    list(tweets$date),
    function(tags) paste(tags, collapse = " ")
  )
 
names(daily_tags) <- c("date","tags")

daily_tags$tags_split <- sapply(daily_tags$tags, function(tags) strsplit(tags, " +"))

 
#hashtags january

tags_split <- unlist(tweetsJan$tags_split)
tags_splitJan <- unlist(tweetsJan$tags_split)
tagsJan <- sapply(tags_splitJan, function(y) nchar(trimws(y)) > 0 & !is.na(y))
tagJan_df <- as.data.frame(table(tolower(tags_splitJan[tagsJan])))
tagJan_df <- tagJan_df[order(-tagJan_df$Freq),]
tagJan_df <- tagJan_df[1:10,]

ggplot(tagJan_df, aes(x = reorder(Var1,-Freq), y = Freq)) +
  geom_bar(stat="identity", fill="darkslategray")+
  theme_minimal() + 
  xlab("#Hashtags january") + ylab("Count")


#hashtags february

tags_split <- unlist(tweetsFeb$tags_split)
tags_splitFeb <- unlist(tweetsFeb$tags_split)
tagsFeb <- sapply(tags_splitFeb, function(y) nchar(trimws(y)) > 0 & !is.na(y))
tagFeb_df <- as.data.frame(table(tolower(tags_splitFeb[tagsFeb])))
tagFeb_df <- tagFeb_df[order(-tagFeb_df$Freq),]
tagFeb_df <- tagFeb_df[1:10,]

ggplot(tagFeb_df, aes(x = reorder(Var1,-Freq), y = Freq)) +
  geom_bar(stat="identity", fill="darkslategray")+
  theme_minimal() + 
  xlab("#Hashtags february") + ylab("Count")


#hashtags march

tags_split <- unlist(tweetsMar$tags_split)
tags_splitMar <- unlist(tweetsMar$tags_split)
tagsMar <- sapply(tags_splitMar, function(y) nchar(trimws(y)) > 0 & !is.na(y))
tagMar_df <- as.data.frame(table(tolower(tags_splitMar[tagsMar])))
tagMar_df <- tagMar_df[order(-tagMar_df$Freq),]
tagMar_df <- tagMar_df[1:10,]

ggplot(tagMar_df, aes(x = reorder(Var1,-Freq), y = Freq)) +
  geom_bar(stat="identity", fill="darkslategray")+
  theme_minimal() + 
  xlab("#Hashtags march") + ylab("Count")


#hashtags april

tags_split <- unlist(tweetsApr$tags_split)
tags_splitApr <- unlist(tweetsApr$tags_split)
tagsApr <- sapply(tags_splitApr, function(y) nchar(trimws(y)) > 0 & !is.na(y))
tagApr_df <- as.data.frame(table(tolower(tags_splitApr[tagsApr])))
tagApr_df <- tagApr_df[order(-tagApr_df$Freq),]
tagApr_df <- tagApr_df[1:10,]

ggplot(tagApr_df, aes(x = reorder(Var1,-Freq), y = Freq)) +
  geom_bar(stat="identity", fill="darkslategray")+
  theme_minimal() + 
  xlab("#Hashtags april") + ylab("Count")



##USER ACTIVITES##
#most acctive accounts#

tweets %>%
  group_by(screen_name) %>%
  summarise(frq = n()) %>%
  arrange(desc(frq)) -> mostActiveAccount

mostActiveAccount_df <- as.data.frame(mostActiveAccount)
mostActiveAccount_df <- mostActiveAccount_df[order(-mostActiveAccount_df$frq),]
mostActiveAccount_df <- mostActiveAccount_df[1:10,]

ggplot(mostActiveAccount_df, aes(x = reorder(screen_name,-frq), y = frq)) +
  geom_bar(stat="identity", fill="darkslategray")+
  theme_minimal() + 
  xlab("Accounts") + ylab("Count")

tweets %>%
  group_by(date) %>%
  summarise(avgSentiment = mean(sentimentScore)) -> sentimentOverTime

ggplot(data = sentimentOverTime, aes(x = date, y = avgSentiment, group = 1)) +
  geom_line(aes(color = 'pink'), size = 1) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## DESCRIPTIVE STUFF ##
#number of tweets
count(tweets)

#distribution of tweets over time
counts <- table(tweets$date)
plot(counts, col = "red")

#distribution of tweets over months
ggplot(data = tweets, aes(x = month(created_at, label = TRUE))) +
  geom_bar(aes(fill = ..count..)) +
  xlab("Month") + ylab("Number of tweets") + 
  theme_minimal() +
  scale_fill_gradient(low = "cadetblue3", high = "chartreuse4")

#distribution of tweets over weekdays
ggplot(data = tweets, aes(x = wday(created_at, label = TRUE))) +
  geom_bar(aes(fill = ..count..)) +
  xlab("Day of the week") + ylab("Number of tweets") + 
  theme_minimal() +
  scale_fill_gradient(low = "turquoise3", high = "darkgreen")

#amount of retweets
tweets %>%
  count(is_retweet) -> retweeted %>%

ggplot(data = tweets, aes(x = as.Date(created_at), fill = is_retweet)) +
  geom_histogram(bins=48) +
  xlab("Time") + ylab("Number of tweets") + theme_minimal() +
  scale_fill_manual(values = c("chartreuse4", "chartreuse3"), name = "Retweet")

#finding geo location
tweets %>%
  count(location) -> locations #no use, selfdescriptive

tweets %>%
  count(geo_coords) -> geo_location #no use, only 4 activated

#language setting (lang: Matches tweets that have been classified by Twitter as being of a particular language)
tweets %>%
  count(lang) -> language  #all in english



