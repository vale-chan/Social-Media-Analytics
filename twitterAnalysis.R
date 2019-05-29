#install & load packages
install.packages("quanteda")
install.packages("dplyr")

require(quanteda)
require(dplyr)


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



##test if removing # and @ changes something ##

#cleaning data
tweets_dfm_test <- dfm(text, remove_punct = T, remove_url = T, remove_numbers = T, remove_symbols = T, remove = stopwords("en"), remove_twitter = T)

#get ready for sentiment analysis
afinn <- readRDS("afinn.rds")
tweets_afinn_test <- dfm_lookup(tweets_dfm_test, dictionary = afinn)

#prepare sentiment scoring
quanteda::convert(tweets_afinn_test, to = "data.frame") %>%
  mutate(afinn_score = (neg5 * -5) + (neg4 * -4) + (neg3 * -3) + (neg2 * -2) + (neg1 * -1) + (zero * 0) + (pos1 * 1) + (pos2 * 2) + (pos3 * 3) + (pos4 * 4) + (pos5 * 5)) %>%
  select(afinn_score) -> afinn_score

#add score as new column to original data
tweets$sentimentScore_test <- afinn_score$afinn_score

#test the differences
mean(tweets$sentimentScore)
mean(tweets$sentimentScore_test)

# the difference is very small... for now, the first option (sentimentScore) is uses



## SENTIMENT ANALYSIS - SENTIMENT OVER TIME ##

#create new "date" column
tweets$date = substr(tweets$created_at,1,10)

#sentiment change over time
require(ggplot2)
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

#amount of retweets
tweets %>%
  count(is_retweet) -> retweeted
head(retweeted)

#finding geo location
tweets %>%
  count(location) -> locations
head(locations, 10) #no use, selfdescriptive

tweets %>%
  count(geo_coords) -> geo_location
head(geo_location, 10) #no use, only 4 activated

#language setting (lang: Matches tweets that have been classified by Twitter as being of a particular language)
tweets %>%
  count(lang) -> language
head(language) #all in english



