# ----------------------------------------------------------------------------------
# Chapter 4 - Relationships between words: N-grams and correlations
# ----------------------------------------------------------------------------------

library(dplyr)
library(janeaustenr)
library(tidytext)

# -- Tokenizing by N-gram
# -- We can also use unnest_tokens() to tokenize into consecutive sequences of words, called n-grams
# -- setting n to 2, we are examining pairs of two consecutive words, offen called "bigrams"

austen_bigrams <- austen_books() %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)

austen_bigrams

# -- Counting and filtering N-grams

# examining the most common bigrams

austen_bigrams %>% count(bigram, sort = TRUE )

# most of common bigrams are pairs of common words. This is a useful time to use tidyr's separate() 

library(tidyr)

bigrams_separated <- austen_bigrams %>% separate(bigram, c("word1", "word2"), sep = " ")
bigrams_separated

# now we can remove cases where either word1 or word2 is a stop word

bigrams_filtered <- bigrams_separated %>% 
                    filter(!word1 %in% stop_words$word) %>%
                    filter(!word2 %in% stop_words$word)
bigrams_filtered

# new biagrams count

bigram_counts <- bigrams_filtered %>% count(word1, word2, sort = TRUE)
bigram_counts
  
  
# we may want to work with the recombined words, tidy unite() is the inverse of separate()
# separate / filter / count / unite let us find the most common bigrams not containing stop words

bigrams_unite <- bigrams_filtered %>% unite(bigram, word1, word2, sep = " ")
bigrams_unite

# in other analyses we may be interested in the most common trigrams, which are consecutive sequence of three words

austen_books() %>% 
          unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
          separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
          filter(!word1 %in% stop_words$word,
                 !word2 %in% stop_words$word,
                 !word3 %in% stop_words$word) %>%
          count(word1, word2, word3, sort = TRUE)

# Analyzing Bigrams

# -- as a simple example, we might be interested in the most common "streets" 
#    mentioned in each book

bigrams_filtered %>% filter(word2 == "street") %>%
                     count(book, word1, sort = TRUE)

# -- A bigram can also be treated as a term in a document in the same way that
#    we treated individual words. For example, we can look at the tf-idf of 
#    bigrams across Austen novels

bigram_tf_idf <- bigrams_unite %>% count(book, bigram) %>%
                 bind_tf_idf(bigram, book, n) %>%
                 arrange(desc(tf_idf))
bigram_tf_idf


# Using bigrams to provide context in sentiment analysis

# -- In the sentiment analysis a word's context can matter nearly as much as its
#    presence
#    Now that we have the data organized into bigrams, it's easy to tell how often
#    words are preceded by a word like "not"

bigrams_separated %>% filter(word1 == "not") %>%
                      count(word1, word2, sort = TRUE)


# -- Let's use Afinn Lexicon

AFINN <- get_sentiments("afinn")
AFINN

# -- Examining the most frequent words that are preceded by "not" and are 
#    associated with a sentiment

not_words <- bigrams_separated %>% filter(word1 == "not") %>%
             inner_join(AFINN, by = c(word2 = "word")) %>%
             count(word2, value, sort = TRUE) %>%
             ungroup()

not_words

# -- It's worth asking wich words contributed the most in the "wrong" direction
#    To compute that, we can multiple their score (value) by the number of times
#    they appear

library(ggplot2)

not_words %>% mutate(contribution = n * value) %>%
              arrange(desc(abs(contribution))) %>%
              head(20) %>%
              mutate(word2 = reorder(word2, contribution)) %>%
              ggplot(aes(word2, n * value, fill = n * value > 0)) +
              geom_col(show.legend = FALSE) +
              xlab("Words preceded by \"not\"") +
              ylab("Sentiment score * number of occurrences") +
              coord_flip()

# The bigrams "not like" and "not help" were overwhelmingly the largest causes of misidentification
# But there are others common words that negate the subsequent term

negation_words <- c("not", "no", "never", "without")

negated_words <- bigrams_separated %>% filter(word1 %in% negation_words) %>%
                 inner_join(AFINN, by = c(word2 = "word")) %>%
                 count(word1, word2, value, sort = TRUE) %>%
                 ungroup()

negated_words


# ploting 

negated_words %>% mutate(contribution = n * value) %>%
                    arrange(desc(abs(contribution))) %>%
                    mutate(word2 = reorder(word2, contribution)) %>%
                    #group_by(word1) %>%
                    head(40) %>%
                    #top_n(20) %>%
                    #ungroup %>%
                    ggplot(aes(word2, n * value, fill = n * value > 0)) +
                    geom_col(show.legend = FALSE) +
                    xlab("Words preceded by negation term") +
                    ylab("Sentiment score * number of occurrences") +
                    facet_wrap(~word1, ncol = 2, scales = "free") +  
                    coord_flip()                  
                  

# -- Visualizing a Network of Bigrams with ggraph

library(igraph)

bigram_counts

bigram_graph <- bigram_counts %>% filter(n > 20) %>% graph_from_data_frame()

bigram_graph

library(ggraph)

set.seed(2017)

ggraph(bigram_graph, layout = "fr") + 
      geom_edge_link() +
      geom_node_point() +
      geom_node_text(aes(label = name), vjust = 1, hjust = 1)


#-- making a better-looking graph
#  - edge_alpha aesthetic to the link layer to make links transparent based on how common or rare the bigrams is
#  - grig::arrow() to add directionality with an arrow, with end_cap option that tells the arrow to end before 
#    touching the node
#  - tinker with the options to the node layer to make the nodes more attractive   
#  - adding a theme that is useful for plotting networks, theme_void()

set.seed(2016)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a, end_cap = circle(.07, 'inches')) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()


# -- Visualizing Bigrams in Others texts

#  Creating a function to easily clean and visualize bigrams on a text dataset

library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(igraph)
library(ggraph)

count_bigrams <- function(dataset){
    dataset %>%
      unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
      separate(bigram, c("word1", "word2"), sep = " ") %>%
      filter(!word1 %in% stop_words$word,
             !word2 %in% stop_words$word) %>%
      count(word1, word2, sort = TRUE)
}


visualize_bigrams <- function(bigrams) {
    set.seed(2016)
    a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
    
    bigrams %>%
        graph_from_data_frame() %>%
        ggraph(layout = "fr") +
        geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
        geom_node_point(color = "lightblue", size = 5) +
        geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
        theme_void()
}

# analysing King James Bible

library(gutenbergr)

kjv <- gutenberg_download(10)

library(stringr)

kjv_bigrams <- kjv %>% count_bigrams()

# filter out rare combinations, as well as digits
kjv_bigrams %>% filter(n > 40,
                      !str_detect(word1, "\\d"),
                      !str_detect(word2, "\\d")) %>%
                visualize_bigrams


# -- Counting and Correlation Pairs of words with de widyr Package

# tokenizing by n-grams is a useful way to explore pairs of adjacent words. However, we may also be interested
# in words that tend to co-occur within particular documents or particular chapters, even if they don't occur
# next to each other

# Counting and Correlating Among Sections

austen_section_words <- austen_books() %>% 
            filter(book == "Pride & Prejudice") %>%
            mutate(section = row_number() %/% 10) %>%
            filter(section > 0) %>%
            unnest_tokens(word, text) %>%
            filter(!word %in% stop_words$word)

austen_section_words

library(widyr)

# counting words co-occuring within sections
word_pairs <- austen_section_words %>%
              pairwise_count(word, section, sort = TRUE)

word_pairs


word_pairs %>% filter(item1 == "darcy")

# ---- Examining pairwise correlation

# How often words appear together relative to how often they appear separetely
# phi coefficient

# Finding the phi coeficient between words based on how they appear in the same section => pairwise_cor()

# we need to filter for at least relatively common words first
word_cors <- austen_section_words %>% 
          group_by(word) %>%
          filter(n() >= 20) %>%
          pairwise_cor(word, section, sort = TRUE)

word_cors


# finding the words most correlated with a word like "pounds"

word_cors %>% filter(item1 == "pounds")

# picking some interesting words and finding the other words associated with them

word_cors %>% filter(item1 %in% c("elizabeth", "pounds", "married", "pride")) %>%
              group_by(item1) %>%
              top_n(6) %>%
              ungroup() %>%
              mutate(item2 = reorder(item2, correlation)) %>%
              ggplot(aes(item2, correlation)) +
              geom_bar(stat = "identity") +
              facet_wrap(~item1, scales = "free") +
              coord_flip()

# visualizing the correlations and cluster of words

set.seed(2016)

word_cors %>% filter(correlation > .15) %>%
          graph_from_data_frame() %>%
          ggraph(layout = "fr") +
          geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
          geom_node_point(color = "lightblue", size = 5) +
          geom_node_text(aes(label = name), repel = TRUE) +
          theme_void()


  





  
  
  













  