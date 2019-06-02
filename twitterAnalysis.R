#load packages

library(dplyr)
library(ggplot2)
library(hms)
library(lubridate)
library(quanteda)
library(reshape2)
library(scales)
library(syuzhet)
library(urltools)
library(tidyverse)
library(rtweet)


#library(purrrlyr)


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
  geom_bar(aes(fill = sentiment), stat = "identity") +
  scale_fill_discrete(name = "Emotion") +
  xlab("Sentiments") + ylab("Sentiment Scores") +
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

ggplot(data = monthlySentiment, aes(month, y = meanvalue, group = sentiment, color = sentiment)) +
  geom_line() + geom_point() +
  labs(colour = "Emotion") +
  xlab("") + ylab("Average emotion score") +
  ggtitle("Emotional change of tweets") +
  theme_minimal()

#positive and negative sentiments over time
posNegSentiment <- tweets %>%
  group_by(month(created_at, label = TRUE)) %>%
  summarise(negative = mean(sentiments$negative),
            positive = mean(sentiments$positive)) %>%
  melt

names(posNegSentiment) <- c("month", "sentiment", "meanvalue")

ggplot(data = posNegSentiment, aes(month, y = meanvalue, group = sentiment, color = sentiment))+
  geom_line() + geom_point() +
  labs(x = "Month", colour = "Sentiment") +
  xlab("") + ylab("Average sentiment score") +
  ggtitle("Sentiment change over time") +
  theme_minimal()

#daily sentimental change
posNegDaily <- tweets %>%
  group_by(date) %>%
  summarise(positive = mean(sentiments$positive),
            negative = mean(sentiments$negative)) %>%
  melt

names(posNegDaily) <- c("days", "sentiment", "meanvalue")

ggplot(data = posNegDaily, aes(days, y = meanvalue, group = sentiment, color = sentiment))+
  geom_line() + geom_point() +
  labs(x = "Sentiment", colour = "Sentiment") +
  xlab("") + ylab("Average sentiment score") +
  theme_minimal() +
  ggtitle("Sentiment change on daily basis") +
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
  xlab("") + ylab("Average sentiment score") +
  theme_minimal() +
  ggtitle("Change of sentiment score on a daily basis") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none")



### HASHTAG ANALYSIS ###

tweets$tags_split <- lapply(tweets$hashtags, function(tags) strsplit(tags, ' '))

##MOST USED HASHTAGS##
# Getting the hashtags from the list 
tags_split <- unlist(tweets$tags_split)

# Formatting by removing the white spacea
tags <- sapply(tags_split, function(y) nchar(trimws(y)) > 0 & !is.na(y))
tag_df <- as.data.frame(table(tolower(tags_split[tags])))
tag_df <- tag_df[order(-tag_df$Freq),]
tag_df <- tag_df[1:10,]

ggplot(tag_df, aes(x = reorder(Var1,-Freq), y = Freq)) +
  geom_bar(stat="identity", fill="darkslategray") +
  xlab("#Hashtags") + ylab("") +
  ggtitle("Top 10 used hashtags") +
  theme_minimal()


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
  ggtitle("Top 10 used hashtags in January") +
  xlab("#Hashtags") + ylab("Count")


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
  ggtitle("Top 10 used hashtags in February") +
  xlab("#Hashtags") + ylab("Count")


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
  ggtitle("Top 10 used hashtags in March") +
  xlab("#Hashtags") + ylab("Count")


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
  ggtitle("Top 10 used hashtags in April") +
  xlab("#Hashtags") + ylab("Count")



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
  ggtitle("Top 10 active accounts") +
  xlab("Accounts") + ylab("Number of tweets")


#most acctive accounts over time
tweets %>%
  filter(screen_name == "PMgeezer" |
           screen_name == "ImmoralReport" |
           screen_name == "marlowstephens" |
           screen_name == "datalgorithms" |
           screen_name == "thomasj17431826" |
           screen_name == "Don32373037" |
           screen_name == "johannaihli" |
           screen_name == "dmzastro" |
           screen_name == "trojancowboy" |
           screen_name == "wernerpatels") -> mostAcctive

ggplot(mostAcctive, aes(x = as.Date(created_at), fill = screen_name)) +
  geom_histogram(position = "identity", stat="count", bins = 50, show.legend = F) +
  facet_wrap(~screen_name, ncol = 1) + 
  xlab("Date") + ylab("Amount of tweets") +
  theme(axis.ticks.y = element_blank(), axis.ticks.x = element_blank()) +
  ggtitle("Tweeting activity of the 10 most active accounts")


#amount of retweets for top 10 users

ggplot(mostAcctive, aes(x = as.Date(created_at), fill = is_retweet)) +
  geom_histogram(position = "identity", stat="count", bins = 50, show.legend = T) +
  facet_wrap(~screen_name, ncol = 1) +
  ylab("Amount of tweets") +
  scale_fill_discrete(name = "Retweet", labels = c("no", "yes")) +
  theme(axis.title.x = element_blank(), axis.ticks.y = element_blank(), axis.ticks.x = element_blank()) +
  ggtitle("Tweeting activity of the 10 most active accounts with re-/tweets ratio")


#most retweeted tweet

tweets %>%
  filter(retweet_status_id != '') %>%
  group_by(retweet_status_id) %>%
  summarise(frq = n()) %>%
  arrange(desc(frq)) -> topRetweets

topRetweets_df <- as.data.frame(topRetweets)
topRetweets_df <- topRetweets_df[order(-topRetweets_df$frq),]
topRetweets_df <- topRetweets_df[1:10,]

ggplot(topRetweets_df, aes(x = reorder(retweet_status_id,-frq), y = frq)) +
  geom_bar(stat="identity", fill="darkslategray") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + 
  xlab("Retweets") + ylab("Count") +
  ggtitle("Amount of the 10 most retweeted tweets")


tweets %>%
  group_by(date) %>%
  filter(retweet_status_id == "x1112884461882925056" |
           retweet_status_id == "x1105606202069843969" |
           retweet_status_id == "x1111196341739417600" |
           retweet_status_id == "x1092227514594979840" |
           retweet_status_id == "x1108751853972606977" |
           retweet_status_id == "x1106701643557625856" |
           retweet_status_id == "x1097670309363281920" |
           retweet_status_id == "x1110671797865906178" |
           retweet_status_id == "x1109093459313524737" |
           retweet_status_id == "x1108751853972606977") -> topTenRetweets

ggplot(topTenRetweets, aes(x = as.Date(created_at), fill = retweet_status_id)) +
  geom_histogram(position = "identity", stat="count", bins = 50, show.legend = T) +
  xlab("") + ylab("Amount of retweets") +
  scale_fill_discrete(name = "Account of origin", label = unique(topTenRetweets$retweet_screen_name)) +
  theme_minimal() +
  ggtitle("Top 10 retweets")

topTenRetweetsHash_dfm <- dfm(topTenRetweets$hashtags)
topfeatures(topTenRetweetsHash_dfm)


# most retweeted account

tweets %>%
  filter(retweet_status_id != '') %>%
  group_by(retweet_screen_name) %>%
  summarise(frq = n()) %>%
  arrange(desc(frq)) -> topRetweetedAccounts

topRetweetedAccounts_df <- as.data.frame(topRetweetedAccounts)
topRetweetedAccounts_df <- topRetweetedAccounts_df[order(-topRetweetedAccounts_df$frq),]
topRetweetedAccounts_df <- topRetweetedAccounts_df[1:10,]

ggplot(topRetweetedAccounts_df, aes(x = reorder(retweet_screen_name,-frq), y = frq)) +
  geom_bar(stat="identity", fill="darkslategray") +
  theme_minimal() + 
  xlab("Retweeted Accounts") + ylab("Count") +
  ggtitle("Top 10 retweeted accounts")


tweets %>%
  group_by(date) %>%
  filter(retweet_screen_name == "superyayadize" |
           retweet_screen_name == "MarkYoungTruth" |
           retweet_screen_name == "ClintEastwoodLA" |
           retweet_screen_name == "AGirlToOne" |
           retweet_screen_name == "LionelMedia" |
           retweet_screen_name == "gailsline" |
           retweet_screen_name == "DeplorablAnnJoy" |
           retweet_screen_name == "Trump_Girl_USA" |
           retweet_screen_name == "Doodisgirl" |
           retweet_screen_name == "Sundncefn") -> topTenRetweetedAccounts

ggplot(topTenRetweetedAccounts, aes(x = as.Date(created_at), fill = retweet_screen_name)) +
  geom_histogram(position = "identity", stat="count", bins = 50, show.legend = T) +
  xlab("") + ylab("Amount of retweets") +
  scale_fill_discrete(name = "Account") +
  theme_minimal() +
  ggtitle("Top 10 retweeted accounts")



##URL-STUFF##
#add "domain" column

tweets$domain <- domain(tweets$urls_expanded_url)
tweetsJan$domain <- domain(tweetsJan$urls_expanded_url)
tweetsFeb$domain <- domain(tweetsFeb$urls_expanded_url)
tweetsMar$domain <- domain(tweetsMar$urls_expanded_url)
tweetsApr$domain <- domain(tweetsApr$urls_expanded_url)


#top youtube links#

tweets %>%
  filter(domain != '') %>%
  filter(domain == "www.youtube.com" | domain == "youtu.be") %>%
  group_by(urls_expanded_url) %>%
  summarise(sum=n()) %>%
  arrange(desc(sum)) %>%
  head()

tweets %>%
  filter(domain != '') %>%
  filter(domain == "www.youtube.com" | domain == "youtu.be") -> tweetsYT

ggplot(tweetsYT, aes(x = as.Date(created_at), fill = urls_expanded_url)) +
  geom_histogram(position = "identity", stat="count", bins = 50, show.legend = F) +
  xlab("") + ylab("Amount of urls") +
  theme_minimal() +
  ggtitle("External youtube links")


#top 10 Youtube urls

tweets %>%
  filter(domain != '') %>%
  filter(domain == "www.youtube.com" | domain == "youtu.be") %>%
  group_by(urls_t.co) %>%
  summarise(sum=n()) %>%
  arrange(desc(sum)) -> top10YTUrls

top10YTUrls_df <- as.data.frame(top10YTUrls)
top10YTUrls_df <- top10YTUrls_df[order(-top10YTUrls_df$sum),]
top10YTUrls_df <- top10YTUrls_df[1:10,]

ggplot(top10YTUrls_df, aes(x = reorder(urls_t.co,-sum), y = sum)) +
  geom_bar(stat="identity", fill="darkslategray") +
  theme_minimal() + 
  xlab("External youtube urls") + ylab("Count") +
  ggtitle("Top 10 external youtube urls")


#top facebook links#
tweets %>%
  filter(domain != '') %>%
  filter(domain == "www.facebook.com") %>%
  group_by(urls_expanded_url) %>%
  summarise(sum=n()) %>%
  arrange(desc(sum)) %>%
  head(20)


tweets %>%
  filter(domain != '') %>%
  filter(domain == "www.facebook.com") -> tweetsFB

ggplot(tweetsFB, aes(x = as.Date(created_at), fill = urls_expanded_url)) +
  geom_histogram(position = "identity", stat="count", bins = 50, show.legend = F) +
  xlab("") + ylab("Amount of external urls") +
  theme_minimal() +
  ggtitle("External facebook links")

#find top youtube links january#

tweetsJan %>%
  filter(domain != '') %>%
  filter(domain == "www.youtube.com" | domain == "youtu.be") %>%
  group_by(urls_expanded_url) %>%
  summarise(sum=n()) %>%
  arrange(desc(sum)) %>%
  head(10)


#find top youtube links february#

tweetsFeb %>%
  filter(domain != '') %>%
  filter(domain == "www.youtube.com" | domain == "youtu.be") %>%
  group_by(urls_expanded_url) %>%
  summarise(sum=n()) %>%
  arrange(desc(sum)) %>%
  head(10)


#find top youtube links march#

tweetsMar %>%
  filter(domain != '') %>%
  filter(domain == "www.youtube.com" | domain == "youtu.be") %>%
  group_by(urls_expanded_url) %>%
  summarise(sum=n()) %>%
  arrange(desc(sum)) %>%
  head(10)


#find top youtube links april#

tweetsApr %>%
  filter(domain != '') %>%
  filter(domain == "www.youtube.com" | domain == "youtu.be") %>%
  group_by(urls_expanded_url) %>%
  summarise(sum=n()) %>%
  arrange(desc(sum)) %>%
  head(10)


#top facebook links january#

tweetsJan %>%
  filter(domain != '') %>%
  filter(domain == "www.facebook.com") %>%
  group_by(urls_expanded_url) %>%
  summarise(sum=n()) %>%
  arrange(desc(sum)) %>%
  head(10)


#top facebook links february#

tweetsFeb %>%
  filter(domain != '') %>%
  filter(domain == "www.facebook.com") %>%
  group_by(urls_expanded_url) %>%
  summarise(sum=n()) %>%
  arrange(desc(sum)) %>%
  head(10)


#top facebook links march#

tweetsMar %>%
  filter(domain != '') %>%
  filter(domain == "www.facebook.com") %>%
  group_by(urls_expanded_url) %>%
  summarise(sum=n()) %>%
  arrange(desc(sum)) %>%
  head(10)


#top facebook links april#

tweetsApr %>%
  filter(domain != '') %>%
  filter(domain == "www.facebook.com") %>%
  group_by(urls_expanded_url) %>%
  summarise(sum=n()) %>%
  arrange(desc(sum)) %>%
  head(10)



## DESCRIPTIVE STUFF ##
#number of tweets
count(tweets)

#4unique accounts contributing to tweets in this dataset
unique(tweets$screen_name) %>% length()


#distribution of tweets over time
ggplot(tweets, aes(x = as.Date(created_at))) +
  geom_histogram(position = "identity", stat="count", bins = 50) +
  geom_bar(fill = "violetred2") +
  xlab("") + ylab("Amount of tweets") +
  theme_minimal() +
  ggtitle("Amount of tweets over time period")


#distribution of tweets over months

ggplot(data = tweets, aes(x = month(created_at, label = TRUE))) +
  geom_bar(aes(fill = ..count..)) +
  xlab("") + ylab("Number of tweets") +
  theme_minimal() +
  ggtitle("Amount of tweets per month") +
  scale_fill_gradient(low = "cadetblue3", high = "chartreuse4", name = "Count")

#distribution of tweets over weekdays

ggplot(data = tweets, aes(x = wday(created_at, label = TRUE))) +
  geom_bar(aes(fill = ..count.., label = "test")) +
  xlab("") + ylab("Number of tweets") + 
  ggtitle("Average amount of tweets per weekday") +
  theme_minimal() +
  scale_fill_gradient(low = "cadetblue3", high = "chartreuse4", name = "Count")

#amount of retweets
tweets %>%
  count(is_retweet) -> retweeted

ggplot(data = tweets, aes(x = as.Date(created_at), fill = is_retweet)) +
  geom_histogram(bins=48) +
  xlab("") + ylab("Number of tweets") +
  theme_minimal() +
  ggtitle("Total amount of tweets per day, differentiating between tweets and retweets") +
  scale_fill_manual(values = c("chartreuse4", "chartreuse3"), name = "Retweet")

#finding geo location
tweets %>%
  count(location) -> locations #no use, selfdescriptive

tweets %>%
  count(geo_coords) -> geo_location #no use, only 4 activated

#language setting (lang: Matches tweets that have been classified by Twitter as being of a particular language)
tweets %>%
  count(lang)  #all in english

