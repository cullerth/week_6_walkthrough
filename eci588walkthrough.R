library(dplyr)
library(readr)
library(tidyr)
library(rtweet)
library(writexl)
library(readxl)
library(tidytext)
library(textdata)
library(ggplot2)
library(scales)

### 1. Prepare ###
## 1c. Set up ##

## store api keys
## in credentials.r -- .gitignore

## authenticate via web browser
token <- create_token(
  app = app_name,
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)

## check to see if the token is loaded
# get_token()

### 2. Wrangle ###
## 2a. Import Tweets ##

#search tweets
ngss_all_tweets <- search_tweets(q = "#NGSSchat", n=5000)

################################################################################
### ✅ Comprehension Check: ### 
# How many tweets did our query using the Twitter API actually return? How many variables?
# 229 obs. of 90 variables

# Why do you think our query pulled in far less than 5,000 tweets requested?
# It's ony looking for tweets from the past 6-9 days

# Does our query also include retweets? How do you know?
# Yes, the data includes a T/F column named "is_retweet"
################################################################################

#remove retweets
ngss_non_retweets <- search_tweets("#NGSSchat", 
                                   n=5000, 
                                   include_rts = FALSE)

#using the OR operator
ngss_or_tweets <- search_tweets(q = "#NGSSchat OR ngss", 
                                n=5000,
                                include_rts = FALSE)

################################################################################
### ✅ Comprehension Check: ###
# Try including both search terms but excluding the OR operator to answer the following question: Does excluding the OR operator return more tweets, the same number of tweets, or fewer tweets? Why?

ngss_and_tweets <- search_tweets(q = "#NGSSchat ngss", 
                                 n=5000,
                                 include_rts = FALSE)
# Excluding the OR operater returns fewer tweets because spaces are treated as the AND operator, so what is being returned is tweets that contain both "#NGSSchat" AND "ngss"

# What other useful arguments does the search_tweet() function contain? Try adding one and see what happens.
#   "type" seems to be an interesting argument...could help to uncover tweets coded as "popular"; tried adding it to ngss_or_tweets and it returned zero results though, so maybe not for this data set. 

################################################################################

# use multipe queries
ngss_tweets <- search_tweets2(c("#NGSSchat OR ngss",
                                '"next generation science standard"',
                                '"next generation science standards"',
                                '"next gen science standard"',
                                '"next gen science standards"'
), 
n=5000,
include_rts = FALSE)

#our first dictionary
ngss_dictionary <- c("#NGSSchat OR ngss",
                     '"next generation science standard"',
                     '"next generation science standards"',
                     '"next gen science standard"',
                     '"next gen science standards"')

ngss_tweets <- search_tweets2(ngss_dictionary,
                              n=5000,
                              include_rts = FALSE)

ccss_dictionary <- c("#commoncore", '"common core"')

ccss_tweets <- ccss_dictionary %>% 
  search_tweets2(n=5000, include_rts = FALSE)

################################################################################
### ✅ Comprehension Check: ###
# Use the search_tweets function to create you own custom query for a twitter hashtag or topic(s) of interest.
# In keeping with my banned books theme, I am interested in seeing what people are tweeting about #BannedBooks during #BlackHistoryMonth...

bb_bhm_tweets <- search_tweets(q = "#bannedbooks #BlackHistoryMonth", 
                  n=5000,
                  include_rts = FALSE)
## this only found 2 tweets

bb_bhm_tweets2 <- search_tweets(q = "'banned books' 'black history month'", 
                               n=5000,
                               include_rts = FALSE)
## this only produced 1 tweet

banned_books <- search_tweets(q = "#bannedbooks", 
                               n=5000,
                               include_rts = FALSE)
## searching for just banned books only returned 22 tweets, I bet there are a lot more during banned books week which is in the Fall sometime if I remember correctly. 

bb_and_ftr <- search_tweets(q = "#bannedbooks OR #freedomtoread", 
                              n=5000,
                              include_rts = FALSE)
##this gave me 122 tweets!

################################################################################

#write to excel
write_xlsx(ngss_tweets, "data/ngss_tweets.xlsx")
write_xlsx(ccss_tweets, "data/csss_tweets.xlsx")

#other useful queries

#search for recent tweets by specific users
fi <- c("sbkellogg", "mjsamberg", "haspires", "tarheel93", "drcallie_tweets", "AlexDreier")

fi_tweets <- fi %>%
  get_timelines(include_rts=FALSE)

sample_n(fi_tweets, 10) %>%
  select(screen_name, text)

# vignette("intro", package="rtweet")

################################################################################
### ✅ Comprehension Check: ###
# To conclude Section 2a, try one of the following search functions from the rtweet vignette:
#   get_timelines() Get the most recent 3,200 tweets from users.
#   stream_tweets() Randomly sample (approximately 1%) from the live stream of all tweets.
#   get_friends() Retrieve a list of all the accounts a user follows.
#   get_followers() Retrieve a list of the accounts following a user.
#   get_favorites() Get the most recently favorited statuses by a user.
#   get_trends() Discover what’s currently trending in a city.
#   search_users() Search for 1,000 users with the specific hashtag in their profile bios.

whats_up_raleigh <- get_trends("raleigh")
#top 5 topics by tweet volume (number of tweets?): "Johnson", "Neera Tanden", "Jake", "USPS", "Superman" 

whats_up_durham <- get_trends("durham")
#interesting: "Error: Could not find trend data for that location."  

whats_up_chapel_hill <- get_trends("chapel hill")
#same error for CH, wonder what the full list o cities you can actually search for is? what's the criteria for inclusion?

whats_up_ann_arbor <- get_trends("ann arbor")
#no data for ann arbor either! c'mon. 

whats_up_lansing <- get_trends("lansing")
#nor Lansing! I give up, it's a capitol city. 

################################################################################

## 2b. Tidy Text ##

ngss_tweets <- read_xlsx("data/ngss_tweets.xlsx")
ccss_tweets <- read_xlsx("data/csss_tweets.xlsx")

# ngss_text <- filter(ngss_tweets, lang == "en")
# ngss_text <- select(ngss_text,screen_name, created_at, text)
# ngss_text <- mutate(ngss_text, standards = "ngss")
# ngss_text <- relocate(ngss_text, standards)
# ngss_text <- select(ngss_text, standards, screen_name, created_at, text)

ngss_text <-
  ngss_tweets %>%
  filter(lang == "en") %>%
  select(screen_name, created_at, text) %>%
  mutate(standards = "ngss") %>%
  relocate(standards)

################################################################################
### ✅ Comprehension Check: ###
# Create an new ccss_text data frame for our ccss_tweets Common Core tweets by modifying code above.

ccss_text <-
  ccss_tweets %>%
  filter(lang == "en") %>%
  select(screen_name, created_at, text) %>%
  mutate(standards = "ccss") %>%
  relocate(standards)

################################################################################

#combine data frames
tweets <- bind_rows(ngss_text, ccss_text)
# head(tweets)
# tail(tweets)

# tokenize text
tweet_tokens <- 
  tweets %>%
  unnest_tokens(output = word, 
                input = text, 
                token = "tweets")
# remove stop words
tidy_tweets <-
  tweet_tokens %>%
  anti_join(stop_words, by = "word")

# custom stop words
count(tidy_tweets, word, sort = T)

filter(tweets, grepl('amp', text))

tidy_tweets <-
  tweet_tokens %>%
  anti_join(stop_words, by = "word") %>%
  filter(!word == "amp")

################################################################################
### ✅ Comprehension Check: ###
# We’ve created some unnecessarily lengthy code to demonstrate some of the steps in the tidying process. Rewrite the tokenization and removal of stop words processes into a more compact series of commands and save your data frame as tidy_tweets.

tidy_tweets <-  tweets %>%
  unnest_tokens(word, text, token = "tweets") %>% 
  anti_join(stop_words, by = "word") %>%
  filter(!word == "amp")

################################################################################

## 2c. Add sentiment values ##

afinn <- get_sentiments("afinn")

# afinn

bing <- get_sentiments("bing")

# bing

nrc <- get_sentiments("nrc")

# nrc

################################################################################
### ✅ Comprehension Check: ###
# How were these sentiment lexicons put together and validated? Hint: take a look at Chapter 2 from Text Mining with R.
# They were compiled through either crowdsourcing or the researchers who authored them, and were validated also by crowdsourcing or by cross-referencing them against online reviews and twitter data. 

# Why should we be cautious when using and interpreting them?
# These sentiment lexicons shouldn't be used with data that is all too different from the texts they were validated against. The authors of Text Mining with R specifically call out narrative fiction from 200 years ago, so these lexicons definitely wouldn't be good contenders for my banned books project, for example!

################################################################################

#join setiments
sentiment_afinn <- inner_join(tidy_tweets, afinn, by = "word")
# sentiment_afinn

sentiment_bing <- inner_join(tidy_tweets, bing, by = "word")
# sentiment_bing

################################################################################
### ✅ Comprehension Check: ###
# Create a sentiment_nrc data frame using the code above.

sentiment_nrc <- inner_join(tidy_tweets, nrc, by = "word")

# sentiment_nrc

# What do you notice about the change in the number of observations (i.e. words) between the tidy_tweets and data frames with sentiment values attached? Why did this happen?
# There are far fewer observations in the sentiment data frames. This probably happened because not every single word used in the tweets has a corollary in the lexicons?

################################################################################

### 3. Explore ###
## 3a. Time Series

ts_plot(tweets, by = "days")

################################################################################
### ✅ Comprehension Check:###
# Use ts_plot with the group_by function to compare the number of tweets over time by Next Gen and Common Core standards
# Which set of standards is Twitter users talking about the most?
# Hint: use the ?ts_plot help function to check the examples to see how this can be done.

ts_plot(tweets, by = "days")

ts_plot(dplyr::group_by(tweets, standards), "days")

################################################################################

## 3b. Sentiment Summary ##

#sentiment counts 
summary_bing <- count(sentiment_bing, sentiment, sort = TRUE)

summary_bing <- sentiment_bing %>% 
  group_by(standards) %>% 
  count(sentiment) 

#compute sentiment value
summary_bing <- sentiment_bing %>% 
  group_by(standards) %>% 
  count(sentiment, sort = TRUE) %>% 
  spread(sentiment, n) 

summary_bing <- sentiment_bing %>% 
  group_by(standards) %>% 
  count(sentiment, sort = TRUE) %>% 
  spread(sentiment, n) %>%
  mutate(sentiment = positive - negative) %>%
  mutate(lexicon = "bing") %>%
  relocate(lexicon)

# summary_bing

summary_afinn <- sentiment_afinn %>% 
  group_by(standards) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(lexicon = "AFINN") %>%
  relocate(lexicon)

# summary_afinn

################################################################################
### ✅ Comprehension Check :###
# For your final task for this walkthough, calculate a single sentiment score for NGSS and CCSS using the remaining nrc and loughan lexicons and answer the following questions. Are these findings above still consistent?

#nrc
summary_nrc <- sentiment_nrc %>% 
  group_by(standards) %>%
  count(sentiment, sort = TRUE) %>% 
  filter(sentiment != "trust") %>% 
  filter(sentiment != "anticipation") %>% 
  filter(sentiment != "joy") %>% 
  filter(sentiment != "fear") %>% 
  filter(sentiment != "anger") %>% 
  filter(sentiment != "sadness") %>% 
  filter(sentiment != "disgust") %>% 
  filter(sentiment != "surprise") %>%
  spread(sentiment, n) %>%
  mutate(sentiment = positive - negative) %>%
  mutate(lexicon = "nrc") %>%
  relocate(lexicon)

# summary_nrc

#loughran
loughran <- get_sentiments("loughran")

sentiment_loughran <- inner_join(tidy_tweets, loughran, by = "word")

summary_loughran <- sentiment_loughran %>% 
  group_by(standards) %>%
  count(sentiment, sort = TRUE) %>% 
  filter(sentiment != "trust") %>% 
  filter(sentiment != "anticipation") %>% 
  filter(sentiment != "joy") %>% 
  filter(sentiment != "fear") %>% 
  filter(sentiment != "anger") %>% 
  filter(sentiment != "sadness") %>% 
  filter(sentiment != "disgust") %>% 
  filter(sentiment != "surprise") %>%
  filter(sentiment != "constraining") %>%
  filter(sentiment != "litigious") %>%
  filter(sentiment != "uncertainty") %>%
  spread(sentiment, n) %>%
  mutate(sentiment = positive - negative) %>%
  mutate(lexicon = "loughran") %>%
  relocate(lexicon)

# summary_loughran

# Hint: The nrc lexicon contains “positive” and “negative” values just like bing and loughan, but also includes values like “trust” and “sadness” as shown below. You will need to use the filter() function to select rows that only contain “positive” and “negative.”

# These analyses aren't consistent with bing + afinn & the Rosenberg findings. NRC analysis indicates that the conversation around the common core standard is much more positive than the other lexicons would lead you to believe; Loughran analysis is much more negative overall for both.

################################################################################







