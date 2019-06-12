#load packages

library(dplyr)
library(ggplot2)
library(hms)
library(lubridate)
library(quanteda)
library(reshape2)
library(scales)
library(stringr)
library(syuzhet)
library(urltools)


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



## SENTIMENT TABLE ##
# Converting tweets to ASCII to trackle strange characters

tweets_text <- tweets$text
tweets_text <- iconv(tweets_text, from="UTF-8", to="ASCII", sub="")

# removing retweets

tweets_text <- gsub("(RT|via)((?:\\b\\w*@\\w+)+)", "", tweets_text)

# removing mentions

tweets_text <- gsub("@\\w+", "", tweets_text)
sentiment <- get_nrc_sentiment((tweets_text))
sentimentscores <- data.frame(colSums(sentiment[,0:8]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment" = rownames(sentimentscores), sentimentscores)
rownames(sentimentscores) <- NULL

#total emotional score

ggplot(data = sentimentscores, aes(x = sentiment, y = Score)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  scale_fill_discrete(name = "Emotion") +
  xlab("") + ylab("Total emotion scores") +
  theme_minimal() +
  theme(legend.position = "bottom")



## SPECIFIC SENTIMENTS OVER TIME ##

tweets$sentiments <- sentiment

#emotion over time

monthlyEmotion <- tweets %>%
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

names(monthlyEmotion) <- c("month", "sentiment", "meanvalue")

ggplot(data = monthlyEmotion, aes(month, y = meanvalue, group = sentiment, color = sentiment)) +
  geom_line() + geom_point() +
  labs(colour = "Emotion") +
  xlab("") + ylab("Average emotion score") +
  theme_minimal() +
  theme(legend.position="bottom")
#ggtitle("Monthly emotional change of tweets")



####
dailyEmotion <- tweets %>%
  group_by(date) %>%
  summarise(anger = mean(sentiments$anger),
            anticipation = mean(sentiments$anticipation),
            disgust = mean(sentiments$disgust),
            fear = mean(sentiments$fear),
            joy = mean(sentiments$joy),
            sadness = mean(sentiments$sadness),
            surprise = mean(sentiments$surprise),
            trust = mean(sentiments$trust)) %>%
  melt

names(dailyEmotion) <- c("day", "sentiment", "meanvalue")

ggplot(data = dailyEmotion, aes(day, y = meanvalue, group = sentiment, color = sentiment)) +
  geom_line() + geom_point() +
  labs(colour = "Emotion") +
  xlab("") + ylab("Average emotion score") +
  theme_minimal() +
  theme(legend.position="bottom", axis.text.x = element_text(angle = 90, hjust = 1))
#ggtitle("Daily emotional change of tweets")


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
  theme_minimal() +
  theme(legend.position="bottom")
#ggtitle("Monthly positive and negative sentiment change of tweets")

#daily sentimental change

posNegDaily <- tweets %>%
  group_by(date) %>%
  summarise(negative = mean(sentiments$negative),
            positive = mean(sentiments$positive)) %>%
  melt

names(posNegDaily) <- c("days", "sentiment", "meanvalue")

ggplot(data = posNegDaily, aes(days, y = meanvalue, group = sentiment, color = sentiment))+
  geom_line() + geom_point() +
  labs(x = "Sentiment", colour = "Sentiment") +
  xlab("") + ylab("Average sentiment score") +
  theme_minimal() +
  theme(legend.position="bottom") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#ggtitle("Daily change of positive and negative sentiment of tweets")


##positiv-negative sentiment aggregated##
#monthly#

sentimentMonthly <- tweets %>%
  group_by(month(created_at, label = TRUE)) %>%
  summarise(mean = mean(sentiments$positive) + mean(sentiments$negative)) %>%
  melt

names(sentimentMonthly) <- c("month", "sentiment", "meanvalue")

ggplot(data = sentimentMonthly, aes(month, y = meanvalue, group = sentiment, color = sentiment)) +
  geom_line() + geom_point() + geom_smooth(aes(fill = factor(sentiment), color = sentiment)) +
  labs(x = "Sentiment", colour = "Sentiment") +
  xlab("") + ylab("Average sentiment score") +
  theme_minimal() +
  theme(legend.position = "none")
#ggtitle("Monthly sentiment change of tweets")

#daily#

sentimentDaily <- tweets %>%
  group_by(date) %>%
  summarise(mean = mean(sentiments$positive) + mean(sentiments$negative)) %>%
  melt

names(sentimentDaily) <- c("days", "sentiment", "meanvalue")

ggplot(data = sentimentDaily, aes(days, y = meanvalue, group = sentiment, color = sentiment)) +
  geom_line() + geom_point() +
  labs(x = "Sentiment", colour = "Sentiment") +
  xlab("") + ylab("Average sentiment score") +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#ggtitle("Daily sentiment change of tweets")


### HASHTAG ANALYSIS ###

tweets$tags_split <- lapply(tweets$hashtags, function(tags) strsplit(tags, ' '))


##MOST USED HASHTAGS##
# Getting the hashtags from the list 

tags_split <- unlist(tweets$tags_split)


# Formatting by removing the white spacea

tags <- sapply(tags_split, function(y) nchar(trimws(y)) > 0 & !is.na(y))
tag_df <- as.data.frame(table(tolower(tags_split[tags])))
tag_df <- tag_df[order(-tag_df$Freq),]
tag10_df <- tag_df[1:10,]


#hashtags january

tweetsJan$tags_split <- lapply(tweetsJan$hashtags, function(tags) strsplit(tags, ' '))
tags_split <- unlist(tweetsJan$tags_split)
tags_split <- unlist(tweetsJan$tags_split)
tagsJan <- sapply(tags_split, function(y) nchar(trimws(y)) > 0 & !is.na(y))
tagJan_df <- as.data.frame(table(tolower(tags_split[tagsJan])))
tagJan_df <- tagJan_df[order(-tagJan_df$Freq),]
tag10Jan_df <- tagJan_df[1:10,]


#hashtags february

tweetsFeb$tags_split <- lapply(tweetsFeb$hashtags, function(tags) strsplit(tags, ' '))
tags_split <- unlist(tweetsFeb$tags_split)
tags_split <- unlist(tweetsFeb$tags_split)
tagsFeb <- sapply(tags_split, function(y) nchar(trimws(y)) > 0 & !is.na(y))
tagFeb_df <- as.data.frame(table(tolower(tags_split[tagsFeb])))
tagFeb_df <- tagFeb_df[order(-tagFeb_df$Freq),]
tag10Feb_df <- tagFeb_df[1:10,]


#hashtags march

tweetsMar$tags_split <- lapply(tweetsMar$hashtags, function(tags) strsplit(tags, ' '))
tags_split <- unlist(tweetsMar$tags_split)
tags_split <- unlist(tweetsMar$tags_split)
tagsMar <- sapply(tags_split, function(y) nchar(trimws(y)) > 0 & !is.na(y))
tagMar_df <- as.data.frame(table(tolower(tags_split[tagsMar])))
tagMar_df <- tagMar_df[order(-tagMar_df$Freq),]
tag10Mar_df <- tagMar_df[1:10,]


#hashtags april

tweetsApr$tags_split <- lapply(tweetsApr$hashtags, function(tags) strsplit(tags, ' '))
tags_split <- unlist(tweetsApr$tags_split)
tags_split <- unlist(tweetsApr$tags_split)
tagsApr <- sapply(tags_split, function(y) nchar(trimws(y)) > 0 & !is.na(y))
tagApr_df <- as.data.frame(table(tolower(tags_split[tagsApr])))
tagApr_df <- tagApr_df[order(-tagApr_df$Freq),]
tag10Apr_df <- tagApr_df[1:10,]



##USER ACTIVITES##
#most acctive accounts#

tweets %>%
  group_by(screen_name) %>%
  summarise(frq = n()) %>%
  arrange(desc(frq)) -> mostActiveAccount

mostActiveAccount_df <- as.data.frame(mostActiveAccount)
mostActiveAccount_df <- mostActiveAccount_df[order(-mostActiveAccount_df$frq),]
mostActiveAccount_df <- mostActiveAccount_df[1:10,]


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
           screen_name == "wernerpatels") -> mostActive

ggplot(mostActive, aes(x = date, fill = screen_name)) +
  geom_histogram(position = "identity", stat = "count", bins = 50, show.legend = F) +
  facet_wrap(~screen_name, ncol = 1) + 
  xlab("") + ylab("Amount of tweets") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.ticks = element_blank()) +
  scale_y_continuous(breaks = c(0, 15))



#amount of retweets for top 10 users

ggplot(mostActive, aes(x = date, fill = is_retweet)) +
  geom_histogram(position = "identity", stat="count", bins = 50, show.legend = T) +
  facet_wrap(~screen_name, ncol = 1) +
  xlab("") + ylab("Amount of tweets and retweets") +
  scale_fill_discrete(name = "Retweet", labels = c("no", "yes")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.ticks = element_blank(), legend.position = "bottom") +
  scale_y_continuous(breaks = c(0, 15))



#most retweeted tweet

tweets %>%
  filter(retweet_status_id != '') %>%
  group_by(retweet_status_id) %>%
  summarise(frq = n()) %>%
  arrange(desc(frq)) -> topRetweets

topRetweets_df <- as.data.frame(topRetweets)
topRetweets_df <- topRetweets_df[order(-topRetweets_df$frq),]
topRetweets_df <- topRetweets_df[1:10,]

topTenTweetIds <- c("x1112884461882925056",
                   "x1105606202069843969",
                   "x1111196341739417600",
                   "x1092227514594979840",
                   "x1108751853972606977",
                   "x1106701643557625856",
                   "x1097670309363281920",
                   "x1110671797865906178",
                   "x1109093459313524737",
                   "x1099316223110594566")

topTenRetweetsMask <- sapply(
  tweets$retweet_status_id,
  function(id) is.element(id, topTenTweetIds)
)

tweets[topTenRetweetsMask,] %>%
  group_by(date) -> topTenRetweets

topTenTweetsMask <- sapply(
  tweets$status_id,
  function(id) is.element(id, topTenTweetIds)
)

ggplot(topTenRetweets, aes(x = as.Date(created_at), fill = retweet_status_id)) +
  geom_histogram(position = "identity", stat="count", bins = 50, show.legend = T) +
  xlab("") + ylab("Amount of retweets") +
  scale_fill_discrete(name = "Account of origin", label = tweets[topTenTweetsMask,"screen_name"]) +
  theme_minimal() +
  theme(legend.position = "bottom")


# most retweeted account

tweets %>%
  filter(retweet_status_id != '') %>%
  group_by(retweet_screen_name) %>%
  summarise(frq = n()) %>%
  arrange(desc(frq)) -> topRetweetedAccounts

topRetweetedAccounts_df <- as.data.frame(topRetweetedAccounts)
topRetweetedAccounts_df <- topRetweetedAccounts_df[order(-topRetweetedAccounts_df$frq),]
topRetweetedAccounts_df <- topRetweetedAccounts_df[1:10,]


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
  theme(legend.position = "bottom")


##URL-STUFF##
#add "domain" column

tweets$domain <- domain(tweets$urls_expanded_url)
tweetsJan$domain <- domain(tweetsJan$urls_expanded_url)
tweetsFeb$domain <- domain(tweetsFeb$urls_expanded_url)
tweetsMar$domain <- domain(tweetsMar$urls_expanded_url)
tweetsApr$domain <- domain(tweetsApr$urls_expanded_url)

tweets %>%
  filter(domain != '') %>%
  group_by(urls_expanded_url) %>%
  summarise(sum=n()) %>%
  arrange(desc(sum)) %>%
  head(10)



#top youtube links#

tweets %>%
  filter(domain != '') %>%
  filter(domain == "www.youtube.com" | domain == "youtu.be") %>%
  group_by(urls_expanded_url) %>%
  summarise(sum=n()) %>%
  arrange(desc(sum)) %>%
  head(10)

tweets %>%
  filter(domain != '') %>%
  filter(domain == "www.youtube.com" | domain == "youtu.be") -> tweetsYT

ggplot(tweetsYT, aes(x = as.Date(created_at), fill = urls_expanded_url)) +
  geom_histogram(position = "identity", stat="count", bins = 50, show.legend = F) +
  xlab("") + ylab("Amount of urls") +
  theme_minimal()


#top 10 Youtube urls

tweets %>%
  filter(domain != '') %>%
  filter(domain == "www.youtube.com" | domain == "youtu.be") %>%
  group_by(urls_t.co) %>%
  summarise(sum=n()) %>%
  arrange(desc(sum)) -> topYTUrls

top10YTUrls_df <- as.data.frame(topYTUrls)
top10YTUrls_df <- top10YTUrls_df[order(-top10YTUrls_df$sum),]
top10YTUrls_df <- top10YTUrls_df[1:10,]


#top facebook links#
tweets %>%
  filter(domain != '') %>%
  filter(domain == "www.facebook.com") %>%
  group_by(urls_expanded_url) %>%
  summarise(sum=n()) %>%
  arrange(desc(sum)) %>%
  head(10)


tweets %>%
  filter(domain != '') %>%
  filter(domain == "www.facebook.com") -> tweetsFB


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


#distribution of tweets over months

ggplot(data = tweets, aes(x = month(created_at, label = TRUE))) +
  geom_bar(aes(fill = ..count..)) +
  xlab("") + ylab("Number of tweets") +
  theme_minimal() +
  scale_fill_gradient(low = "cadetblue3", high = "chartreuse4", name = "Count")
#ggtitle("Amount of tweets per month")


#distribution of tweets over weekdays

ggplot(data = tweets, aes(x = wday(created_at, label = TRUE))) +
  geom_bar(aes(fill = ..count.., label = "test")) +
  xlab("") + ylab("Average number of tweets") +
  theme_minimal() +
  scale_fill_gradient(low = "cadetblue3", high = "chartreuse4", name = "Count")
#ggtitle("Average amount of tweets per weekday")


#amount of retweets
tweets %>%
  count(is_retweet) -> retweeted

ggplot(data = tweets, aes(x = as.Date(created_at), fill = is_retweet)) +
  geom_histogram(bins=48) +
  xlab("") + ylab("Number of tweets") +
  theme_minimal() +
  scale_fill_discrete(name = "Retweet", labels = c("no", "yes"))
#ggtitle("Total amount of tweets per day, differentiating between tweets and retweets")



##HASHTAG NETWORK DATA GENERATING##
tweetsNet <- read.csv('climateChangeSample.csv',stringsAsFactors = F)
tweetsNet %>%
  filter(!is.na(hashtags)) %>%
  nrow() 

tweetsNet %>%
  filter(!is.na(hashtags)) %>%
  select(hashtags) -> hashNet

hashNet <- hashNet$hashtags %>%
  tolower() %>%
  str_split(" ") %>%
  lapply(function(x) {expand.grid(x, x, w = 1 / length(x), stringsAsFactors = FALSE)}) %>%
  bind_rows

hashNet %>%
  filter(Var1 != Var2) -> hashNet  

hashNet %>%
  filter(str_detect(Var1,'<u+') == F & str_detect(Var2,'<u+') == F) -> hashNet
colnames(hash)<-c('Source','Target', 'Weight')

write.csv(hashNet,"hashNetwork.csv",row.names = FALSE)



####################
#retweet edge table#
####################

tweetsEdge <- read.csv('climateChangeSample.csv',stringsAsFactors = F)

edges <- tweetsEdge %>%
  filter(is_retweet == T) %>%
  select(screen_name, retweet_screen_name)

colnames(edges) <- c('Source','Target')
write.csv(edges,'edgeTable.csv',row.names = FALSE)



