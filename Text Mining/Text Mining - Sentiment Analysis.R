# ----------------------------------------------------------------------------------
# Chapter 2 - Sentiment Analysis with Tidy Data
# ----------------------------------------------------------------------------------

library(tidytext)

sentiments

get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")


# ====== Sentiment analysis with inner join

library(janeaustenr)
library(dplyr)
library(stringr)
library(tidytext)


tidy_books <- austen_books() %>% 
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

nrcjoy <- get_sentiments("nrc") %>% filter(sentiment == "joy")

tidy_books %>% filter(book == "Emma") %>%
  inner_join(nrcjoy) %>%
  count(word, sort = TRUE)


# --- Calculating a net sentiment (positive - negative)

janeaustensentiment <- tidy_books %>% 
  inner_join(get_sentiments("bing")) %>%
  count(book, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

#-- ploting these sentiment across the plot trajectory of each novel

library(ggplot2)

ggplot(janeaustensentiment, aes(index, sentiment, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")


# -- Comparing the three sentiment dicitionaries

pride_prejudice <- tidy_books %>% filter(book == "Pride & Prejudice")
pride_prejudice

afinn <- pride_prejudice %>% inner_join(get_sentiments("afinn")) %>%
  group_by(index = linenumber %/% 80) %>%
  summarise(sentiment = sum(score)) %>%
  # summarise(sentiment = sum(value)) %>%
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(
  pride_prejudice %>% 
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing et al."),
  pride_prejudice %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", 
                                         "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

#-- we now have an estimate of the net sentiment (positive - negative) in 
#   each chunk of the novel text for each lexicon. Let's bind them togheter

bind_rows(afinn, bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

# -- Why is, for example, the result for the NRC lexicon biased so high in 
#    sentiment compared to the Bing et al. result?

get_sentiments("nrc") %>% filter(sentiment %in% c("positive", "negative")) %>%
  count(sentiment)

get_sentiments("bing") %>% count(sentiment)

# -- Most common positive and negative words

bing_word_counts <- tidy_books %>% 
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts

bing_word_counts %>% 
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) + 
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

# -- this lets us spot an anomaly in the sentimental analysis; the word "miss"
#    is coded as negative but it's used as a title for young, unmarried women
#    in Jane Austen's works. 


custom_stop_words <- bind_rows(data.frame(word = c("miss"),
                                          lexicon = c("custom")),
                               stop_words)
custom_stop_words

#-- Wordclouds

library("wordcloud")

# tidy_books %>% anti_join(stop_words) %>% count(word) %>% 
#               with(wordcloud(word, n, max.words = 100))

tidy_books %>% anti_join(custom_stop_words) %>% count(word) %>% 
  with(wordcloud(word, n, max.words = 100))



#-- tagging positive and negative words in the word cloud

library(reshape2)

tidy_books %>% inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)