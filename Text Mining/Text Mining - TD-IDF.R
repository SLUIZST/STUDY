# ----------------------------------------------------------------------------------
# Chapter 3 - Analyzing word and document frequency: tf_idf
# ----------------------------------------------------------------------------------

# -- Term frequency
# What are the most commonly used words in Jane Austen's novels?

library(dplyr)
library(janeaustenr)
library(tidytext)

book_words <- austen_books() %>% unnest_tokens(word, text) %>% 
  count(book, word, sort = TRUE) %>% ungroup()

total_words <- book_words %>% group_by(book) %>% summarize(total = sum(n))

total_words

book_words <- left_join(book_words, total_words)

book_words

# Let's look at the distribution of n/total for each novel: the number of times a word appears in a novel divided
# by the total number of terms (words) in that novel. That is exactly what term frequency is

library(ggplot2)

ggplot(book_words, aes(n/total, fill = book)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~book, ncol = 2, scales = "free_y")

# Zipf's Law
# "The frequency that a word appears is inversely proportional to its rank" George Zipf

freq_by_rank <- book_words %>% group_by(book) %>% mutate(rank = row_number(), 'term frequency' = n/total)
freq_by_rank

# Plotting, an inversely proportional relationship between frequency and rank will have a constant, negative slope

freq_by_rank %>% ggplot(aes(rank, 'term frequency', color = book)) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10()  + scale_y_log10()  # => ERROR

# Test -> something wrong with 'term frequancy' is causing error in scale_Y-log() 
freq_by_rank_tst <- freq_by_rank %>% mutate(term_freq_1000 = (n/total) * 1000)
freq_by_rank_tst %>% ggplot(aes(rank, term_freq_1000, color = book)) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) 

# Other way, from beginning
freq_by_rank2 <- book_words %>% group_by(book) %>% mutate(rank = row_number(), term_freq = n/total)
freq_by_rank2
freq_by_rank2 %>% ggplot(aes(rank, term_freq, color = book)) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10()  + scale_y_log10()

# -- Power Law
# https://en.wikipedia.org/wiki/Power_law

# we could view this plot as a broken power law, with, say, three sections. Let's see what the expoent of
# the power law for the middle section of the rank range

rank_subset <- freq_by_rank2 %>% filter(rank < 500, rank > 100)

lm(log10(term_freq) ~ log10(rank), data = rank_subset)

# Let's plot this fitted power law with the data 
freq_by_rank2 %>% ggplot(aes(rank, term_freq, color = book)) +
  geom_abline(intercept = -0.5932, slope = -1.1, color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() + scale_y_log10()


# ----- The bind_tf_idf function

# Calculating tf-idf attempts to find the words that are important (i.e., common) ina a text, but not too common

book_words_tf_idf <- book_words %>% bind_tf_idf(word, book, n)
book_words_tf_idf

# tf-idf is very low (near zero) for words that occur in many of the documents in a collection

book_words_tf_idf %>% select(-total) %>% arrange(desc(tf_idf))

# Let's look at a visualization for these high tf-idf words

book_words_tf_idf %>% arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(book) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = book)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 2, scales = "free") +
  coord_flip()

# Still all proper nouns!

# --- Another corpus, Physics Texts

library(gutenbergr)

physics <- gutenberg_download(c(37729, 14725, 13476, 5001), meta_fields = "author")


physics_words <- physics %>% unnest_tokens(word, text) %>%
  count(author, word, sort = TRUE) %>%
  ungroup()

physics_words

# calculating and visualizing the high tf-idf words

plot_physics <- physics_words %>% bind_tf_idf(word, author, n) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  mutate(author = factor(author, levels = (c("Galilei, Galileo",
                                             "Huygens, Christiaan",
                                             "Tesla, Nikola",
                                             "Einstein, Albert"))))

plot_physics %>% group_by(author) %>%
  top_n(15, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = author)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~author, ncol = 2, scales = "free") +
  coord_flip()

# There are some less meaningful words in the texts, for instance: "AB","RC", and
# so forth are names of rays, circles, angles, and so on for Huygens.

library(stringr)

physics %>% filter(str_detect(text, "AK")) %>% 
  select(text)

# Let's remove some of these less meaningful words to make a better, more
# meaningful plot

mystopwords <- data_frame(word = c("eq","co","rc","ac","ak","bn","fig",
                                   "file","cg","cb","cm"))

physics_words <- anti_join(physics_words, mystopwords, by = "word")


plot_physics <- physics_words %>% bind_tf_idf(word, author, n) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  mutate(author = factor(author, levels = (c("Galilei, Galileo",
                                             "Huygens, Christiaan",
                                             "Tesla, Nikola",
                                             "Einstein, Albert"))))

plot_physics %>% group_by(author) %>%
  top_n(15, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = author)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~author, ncol = 2, scales = "free") +
  coord_flip()

