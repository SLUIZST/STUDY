# ----------------------------------------------------------------------------------
# Chapter 1 - The Tidy Text Format
# ----------------------------------------------------------------------------------

# --- Principles


text <- c("Because I could not stop for death - ",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality",
          "Emily Dickinson")

text

# -- In order to turn it into a tidy text dataset, we first need o put it into a data frame

library(dplyr)
text_df <- data_frame(line = 1:5, text = text)

text_df

# -- It isn't yet compatible with tidy text analysis, becaus we can't filter words or count wich occur 
#    most frequently. We need to convert this so that it has one token per document per row.

# -- Just for now, we only have one document (the poem)

# -- Breakin the text into individual tokens (Tokenization) and tranforming it into a tidy data structure

library(tidytext)

text_df %>% unnest_tokens(word, text)


# ------- Tidying the Works of Jane Austen

library(janeaustenr)
library(dplyr)
library(stringr)

original_books <- austen_books() %>% group_by(book) %>%
                  mutate(linenumber = row_number(),
                  chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", ignore_case = TRUE)))) %>%
                  ungroup()

original_books

# -- restructuring it in one-token-per-row, making the tidy dataset

library(tidytext)

tidy_books <- original_books %>% unnest_tokens(word, text)

# -- removing the stop words

data("stop_words")

tidy_books_clean <- tidy_books %>% anti_join(stop_words)

# -- Obs: it's possible to use filter() to only use one set of stop words, if that is more appropriate for a 
#         certain analysis

# -- Finding the most common words in all the books as a whole

tidy_books_clean %>% count(word, sort = TRUE)

# -- Using ggplot2 for this

library(ggplot2)

tidy_books_clean %>% count(word, sort = TRUE) %>%
                     filter(n > 600) %>%
                     mutate(word = reorder(word, n)) %>%
                     ggplot(aes(word, n)) + geom_col() + xlab(NULL) + coord_flip()


# -- https://ropensci.org/tutorials/gutenbergr_tutorial/

# --- Some science fiction and fantasy novels by H.G. Wells. Let's get The Time Machine (35), 
#     The War of the Worlds (36), The Invisible Man (5230) and The Island of Doctor Moreau (159)

library(gutenbergr)

# https://www.rdocumentation.org/packages/gutenbergr/versions/0.1.4/topics/gutenberg_download
# gutenberg_get_mirror(verbose = TRUE)

hgwells <- gutenberg_download(c(35, 36, 5230, 159), verbose = TRUE)    

tidy_hgwells <- hgwells %>% unnest_tokens(word, text) %>% anti_join(stop_words)

tidy_hgwells %>% count(word, sort = TRUE)

# -- Now let's get somw works from the Bronte sisters: Jane Eyre (1260), Wuthering Heights (768),
#    The Tenant of Wildfell Hall(969), Villette (9182) and Agnes Grey (767)

bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))    

tidy_bronte <- bronte %>% unnest_tokens(word, text) %>% anti_join(stop_words)

tidy_bronte %>% count(word, sort = TRUE)

# -- Now let's calculate the frequency for each word in the works of Jane Austen, the Bronte sisters 
#    and H.G.Wells by binding the data frames together


library(tidyr)

freq <- bind_rows(mutate(tidy_bronte, author = "Bronte Sisters"),
                  mutate(tidy_hgwells, author = "H.G. Wells"),
                  mutate(tidy_books_clean, author = "Jane Austen")) %>%
        mutate(word = str_extract(word, "[a-z']+")) %>%
        count(author, word) %>%
        group_by(author) %>%
        mutate(proportion = n / sum(n)) %>%
        select(-n) %>%
        spread(author, proportion) %>%
        gather(author, proportion, 'Bronte Sisters':'H.G. Wells')


# -- plotting
library(scales)

# expect a warning about rows with missing values being removed
ggplot(freq, aes(x = proportion, y = `Jane Austen`,
                 color = abs(`Jane Austen` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001),
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position = "none") +
  labs(y = "Jane Austen", x = NULL)


# How similar and different these sets of word frequencies are -> correlation test

#cor.test(data = frequency[frequency$author == "Bronte Sisters", ], ~ proportion + `Jane Austen`)
cor.test(data = freq[freq$author == "Bronte Sisters", ], ~ proportion + `Jane Austen`) 


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
         #summarise(sentiment = sum(score)) %>%
         summarise(sentiment = sum(value)) %>%
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


# ----------------------------------------------------------------------------------
# Chapter 3
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
#        "The frequency that a word appears is inversely proportional to its rank" George Zipf

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



                    


