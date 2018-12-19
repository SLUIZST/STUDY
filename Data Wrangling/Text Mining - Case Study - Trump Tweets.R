# ============ Dates, Times, and Text Mining  ======================

# -- Text Mining

# --- Case study: Trump Tweets

library(tidyverse)
library(ggplot2)
library(lubridate)
library(tidyr)
library(scales)
set.seed(1)

# -- In general, we can extract data directly from twitter using the \emph{rtweet} package.
# However, in this case, a group has already compiled data for us and made it available 
# at http://www.trumptwitterarchive.com.

url <- 'http://www.trumptwitterarchive.com/data/realdonaldtrump/%s.json'
trump_tweets <- map(2009:2017, ~sprintf(url, .x)) %>%
  map_df(jsonlite::fromJSON, simplifyDataFrame = TRUE) %>%
  filter(!is_retweet & !str_detect(text, '^"')) %>%
  mutate(created_at = parse_date_time(created_at, orders = "a b! d! H!:M!:S! z!* Y!", tz="EST")) 


#For convenience we include the result of the code above in the dslabs package:

library(dslabs)
data("trump_tweets")

head(trump_tweets)

names(trump_tweets)

# The help file ?trump_tweets provides details on what each variable represents. The tweets are represented by the textvariable:
trump_tweets %>% select(text) %>% head

# the source variable tells us the device that was used to compose and upload each tweet:
trump_tweets %>% count(source) %>% arrange(desc(n))

# We can use extract to remove the Twitter for part of the source and filter out retweets.

trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  count(source) 

# We are interested in what happened during the campaign, so for the analysis here we 
# will focus on what was tweeted between the day Trump announced his campaign and 
# election day. So we define the following table:

campaign_tweets <- trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  filter(source %in% c("Android", "iPhone") &
           created_at >= ymd("2015-06-17") & 
           created_at < ymd("2016-11-08")) %>%
  filter(!is_retweet) %>%
  arrange(created_at)

# We can now use data visualization to explore the possibility that two different 
# groups were tweeting from these devices. For each tweet, we will extract the hour, 
# in the east coast (EST), it was tweeted then compute the proportion of tweets tweeted 
# at each hour for each device.

ds_theme_set()
campaign_tweets %>%
  mutate(hour = hour(with_tz(created_at, "EST"))) %>%
  count(source, hour) %>%
  group_by(source) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup %>%
  ggplot(aes(hour, percent, color = source)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Hour of day (EST)",
       y = "% of tweets",
       color = "")

# We notice a big peak for the Android in early hours of the morning, between 6 and 8 AM.
# There seems to be a clear different in these patterns. We will therefore assume that 
# two different entities are using these two devices. Now we will study how their tweets 
# differ. To do this we introduce the tidytext package.

# --- Text as data

# The tidytext package helps us convert free from text into a tidy table. Having the data
# in this format greatly facilitates data visualization and applying statistical 
# techniques.

library(tidytext)

# The main function needed to achieve this is unnest_tokens. A token refers to the units 
# that we are considering to be a data point. The most common token will be words, but 
# they can also be single characters, ngrams, sentences, lines or a pattern defined by 
# a regex. The functions will take a vector of strings and extract the tokens so that 
# each one gets a row in the new table. Here is a simple example:

example <- data_frame(line = c(1, 2, 3, 4),
                      text = c("Roses are red,", "Violets are blue,", "Sugar is sweet,", "And so are you."))
example
example %>% unnest_tokens(word, text)

# Now let's look at a quick example with a tweet number 3008

i <- 3008
campaign_tweets$text[i]
campaign_tweets[i,] %>% 
  unnest_tokens(word, text) %>%
  select(word)

# Note that the function tries to convert tokens into words and strips characters 
# important to twitter such as # and @. A token in twitter is not the same as in regular 
# english. For this reason instead of using the default, words, we define a regex that 
# captures twitter character. The pattern appears complex but all we are defining is a
# patter that starts with @, # or neither and is followed by any combination of letter or 
# digits:

pattern <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"

# We can now use the unnest_tokens function with the regex option and appropriately 
# extract the hashtags and mentions:
  
  campaign_tweets[i,] %>% 
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  select(word)
  
# Another minor adjustment we want to make is remove the links to pictures:
    
 campaign_tweets[i,] %>% 
 mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
 unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
 select(word)
 
 # Now we are now read to extract the words for all our tweets.
 
 tweet_words <- campaign_tweets %>% 
   mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
   unnest_tokens(word, text, token = "regex", pattern = pattern) 
 
 # And we can now answer questions such as "what are the most commonly used words?"
 
 tweet_words %>% 
   count(word) %>%
   arrange(desc(n))
  
# It is not surprising that these are the top words. The top words are not informative. 
# The tidytext package has database of these commonly used words, referred to as stop 
# words, in text mining:
   
   stop_words
           
   
# If we filter out rows representing stop words with filter(!word %in% stop_words$word)
   
tweet_words <- campaign_tweets %>% 
 mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
 unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
 filter(!word %in% stop_words$word ) 

# we end up with a much more informative set of top 10 tweeted words

tweet_words %>% 
  count(word) %>%
  top_n(10, n) %>%
  mutate(word = reorder(word, n)) %>%
  arrange(desc(n))

# Some exploration of the resulting words (not show here) reveals a couple of unwanted 
# characteristics in our tokens. First, some of our tokens are just numbers (years for 
# example). We want to remove these and we can find them using the regex ^\d+$. Second, 
# some of our tokens come from a quote and they start with '. We want to remove the ' 
# when it's at the start of a word so we will use str_replace. We add these two lines 
# to the code above to generate are final table:

tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  filter(!word %in% stop_words$word &
  !str_detect(word, "^\\d+$")) %>%
  mutate(word = str_replace(word, "^'", ""))


# Now that we have all our words in a table, along with information about what device 
# was used to compose the tweet they came from, we can start exploring which words are 
# more common when comparing Android to iPhone.

# For each word we want to know if it is more likely to come from an Android tweet or an 
# iPhone tweet. We previously introduced the odds ratio a summary statistic useful for 
# quantifying these differences. We each device and a given word, let's call it y, we 
# compute the odds or the ratio between the proportion of words that are y and not y and 
# compute the ratio of those odds. Here we will have many proportions that are 0 so 
# we use the 0.5 correction.

android_iphone_or <- tweet_words %>%
  count(word, source) %>%
  spread(source, n, fill = 0) %>%
  mutate(or = (Android + 0.5) / (sum(Android) - Android + 0.5) / 
           ( (iPhone + 0.5) / (sum(iPhone) - iPhone + 0.5)))

head(android_iphone_or)

android_iphone_or %>% arrange(desc(or))
android_iphone_or %>% arrange(or)

# Given that several of these words are overall low frequency words we can impose a 
# filter based on the total frequency like this:
  
android_iphone_or %>% filter(Android+iPhone > 100) %>%
  arrange(desc(or))

android_iphone_or %>% filter(Android+iPhone > 100) %>%
  arrange(or)


# We already see somewhat of a pattern in the types of words that are being tweeted more 
# in one device versus the other. However, we are not interested in specific words but 
# rather in the tone. Vaziri's assertion is that the Android tweets are more hyperbolic. 
# So how can we check this with data? Hyperbolic is a hard sentiment to extract from words
# as it relies on interpreting phrases. However, words can be associated to more basic 
# sentiment such as as anger, fear, joy and surprise. In the next section we demonstrate 
# basic sentiment analysis.

# ------------- Sentiment Analysis

# In sentiment analysis we assign a word to one or more "sentiment". Although this 
# approach will miss context dependent sentiments, such as sarcasm, when performed on 
# large numbers of words, summaries can provide insights.

# The first step in sentiment analysis is to assign a sentiment to each word. The 
# tidytext package includes several maps or lexicons in the object sentiments:

table(sentiments$lexicon)

# The bing lexicon divides words into positive and negative. We can see this using the tidytext function get_sentiments:
  
get_sentiments("bing")

# The AFINN lexicon assigns a score between -5 and 5, with -5 the most negative and 5 
# the most positive.

get_sentiments("afinn")

# The loughran and nrc lexicons provide several different sentiments:
  
get_sentiments("loughran") %>% count(sentiment)
get_sentiments("nrc") %>% count(sentiment)

# To start learning about how these lexicons were developed read this help file ?sentiments

?sentiments

# For the analysis here we are interested in exploring the different sentiments of each 
# tweet so we will use the nrc lexicon:
  
nrc <- sentiments %>%
  filter(lexicon == "nrc") %>%
  select(word, sentiment)

# We can combine the words and sentiments using inner_join, which will only keep words 
# associated with a sentiment. Here are 10 random words extracted from the tweets:
  
tweet_words %>% inner_join(nrc, by = "word") %>% 
  select(source, word, sentiment) %>% sample_n(10)
  
# Now we are ready to perform a quantitative analysis comparing Android and iPhone by 
# comparing the sentiments of the tweets posted from each device. Here we could perform 
# a tweet by tweet analysis, assigning a sentiment to each tweet. However, this somewhat 
# complex since each tweet will have several sentiments attached to it, one for each word 
# appearing in the lexicon. For illustrative purposes we will perform a much simpler 
# analysis: we will count and compare the frequencies of each sentiment appears for each 
# device.

sentiment_counts <- tweet_words %>%
  left_join(nrc, by = "word") %>%
  count(source, sentiment) %>%
  spread(source, n) %>%
  mutate(sentiment = replace_na(sentiment, replace = "none"))

sentiment_counts

# Because more words were used on the Android than on the phone:
  
  tweet_words %>% group_by(source) %>% summarize(n = n())
  
# For each sentiment we can compute the odds of being in the device: proportion of 
# words with sentiment versus proportion of words without and then compute the odds 
# ratio comparing the two devices
  
sentiment_counts %>%
    mutate(Android = Android / (sum(Android) - Android) , 
           iPhone = iPhone / (sum(iPhone) - iPhone), 
           or = Android/iPhone) %>%
    arrange(desc(or))
  
# So we do see some difference and the order is interesting: the largest three sentiments
# are disgust, anger, and negative! But are they statistically significant? How does this
# compare if we are just assigning sentiments at random?
  
# To answer that question we can compute, for each sentiment, an odds ratio and 
# confidence interval. We will add the two values we need to form a two-by-two table and 
# the odd rat

library(broom)

log_or <- sentiment_counts %>%
  mutate( log_or = log( (Android / (sum(Android) - Android)) / (iPhone / (sum(iPhone) - iPhone))),
          se = sqrt( 1/Android + 1/(sum(Android) - Android) + 1/iPhone + 1/(sum(iPhone) - iPhone)),
          conf.low = log_or - qnorm(0.975)*se,
          conf.high = log_or + qnorm(0.975)*se) %>%
  arrange(desc(log_or))

log_or

# A graphical visualization shows some sentiments that are clearly overrepresented:
  
  log_or %>%
  mutate(sentiment = reorder(sentiment, log_or)) %>%
  ggplot(aes(x = sentiment, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point(aes(sentiment, log_or)) +
  ylab("Log odds ratio for association between Android and sentiment") +
  coord_flip() 
  
# We see that the disgust, anger, negative sadness and fear sentiments are associated 
# with the Android in a way that is hard to explain by chance alone. Words not associated
# to a sentiment were strongly associated with the iPhone source, whic is in agreement 
# with the original claim about hyperbolic tweets.
  
# If we are interested in exploring which specific words are driving these differences, 
# we can back to our android_iphone_or object:
  
  android_iphone_or %>% inner_join(nrc) %>%
    filter(sentiment == "disgust" & Android + iPhone > 10) %>%
    arrange(desc(or))
  
# We can make a graph
  
  android_iphone_or %>% inner_join(nrc, by = "word") %>%
    mutate(sentiment = factor(sentiment, levels = log_or$sentiment)) %>%
    mutate(log_or = log(or)) %>%
    filter(Android + iPhone > 10 & abs(log_or)>1) %>%
    mutate(word = reorder(word, log_or)) %>%
    ggplot(aes(word, log_or, fill = log_or < 0)) +
    facet_wrap(~sentiment, scales = "free_x", nrow = 2) + 
    geom_bar(stat="identity", show.legend = FALSE) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
  
