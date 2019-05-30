#load packages
library(quanteda)
library(dplyr)
library(ggplot2)
library(lubridate)
library(hms)
library(scales)
library(syuzhet)


##SENTIMENT ANALYSIS - PREPARE DATA##

#load & inspect data
tweets <- read.csv("climateChangeSample.csv", stringsAsFactors = F)
text <- tweets$text

#cleaning data
tweets_dfm <- dfm(text, remove_punct = T, remove_url = T, remove_numbers = T, remove_symbols = T, remove = stopwords("en"))

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

#specific sentiments over months
tweets$sentiments <- sentiment




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



## HASHTAG ANALYSIS ##
# Getting the hashtags from the list 
tags_split <- unlist(strsplit(as.character(unlist(tweets$hashtags)),' '))

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



