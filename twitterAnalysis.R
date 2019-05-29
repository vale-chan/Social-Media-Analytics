#install & load packages
install.packages("quanteda")
install.packages("dplyr")

require(quanteda)
require(dplyr)


##SENTIMENT ANALYSIS##
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
View(tweets)

