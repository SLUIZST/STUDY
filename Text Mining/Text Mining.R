#------------------------------------------------------------------------------------------------------------------------
# Libraries
#------------------------------------------------------------------------------------------------------------------------

library(tidyr)
# -- Reading from a spreadsheet
library(readxl)   
# -- In order to turn it into a tidy text dataset, we first need o put it into a data frame
library(dplyr)
library(stringr)
# -- Tokenization and tranforming it into a tidy data structure
library(tidytext)
# -- Stop words
library(stopwords)
# -- Visualization
library(ggplot2)
library(wordcloud)
library(reshape2)
library(scales)
# Dicionarios lexicos em PT.
library(lexiconPT)


#------------------------------------------------------------------------------------------------------------------------
# Functions
#------------------------------------------------------------------------------------------------------------------------
visualize_terms <- function(dataset, min_terms) {
    dataset %>% count(word, sort = TRUE) %>%
                filter(n > min_terms) %>%
                mutate(word = reorder(word, n)) %>%
                ggplot(aes(word, n)) + geom_col() + xlab(NULL) + coord_flip()
}


#-----------------------------------------------------------------
visualize_cloud <- function(dataset, maxwords) {
    dataset %>% count(word) %>% 
                with(wordcloud(word, n, max.words = maxwords))
}



#-----------------------------------------------------------------
visualize_cloud_polarity  <- function(dataset, maxwords) {
    dataset %>% count(word, polarity, sort = TRUE) %>%
                acast(word ~ polarity, value.var = "n", fill = 0) %>%
                comparison.cloud(colors = c("gray20", "gray80", "blue"),
                max.words = maxwords)
  
}
  


#-----------------------------------------------------------------
count_bigrams <- function(dataset){
  dataset %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE)
}

#-----------------------------------------------------------------
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

#------------------------------------------------------------------------------------------------------------------------
# Reading file
#------------------------------------------------------------------------------------------------------------------------

#  Working directory
getwd()

# Changing the working directory 
setwd("F:/Sergio/Estudo/Ciencia de Dados/Study/Text Mining")


# Reading from Excel
Text_Untidy <- read_excel("Comentarios sobre TD 2.xlsx", sheet="Comentarios")
head(Text_Untidy)

# Converting to a dataframe
dText <- data_frame(line = 1:83, text = Text_Untidy$Texto, noticia = Text_Untidy$Noticia)
dText

#------------------------------------------------------------------------------------------------------------------------
# Preparing file
#------------------------------------------------------------------------------------------------------------------------

# -- Breaking the text into individual tokens (Tokenization) and tranforming it into a tidy data structure
tidy_text <- dText %>% unnest_tokens(word, text)
tidy_text

# -- Stop words
stopwords_PT <- stopwords(language="portuguese")
stopwords_PT
class(stopwords_PT)

# Converting the stopwords into a data_frame in order to do the anti-join with tidy_text
dstopwords_PT <- data_frame(word = stopwords_PT, lexicon="SMART")
dstopwords_PT

# -- Removing stop words
text_clean <- tidy_text %>% anti_join(dstopwords_PT)
text_clean

#------------------------------------------------------------------------------------------------------------------------
# Most common words
#------------------------------------------------------------------------------------------------------------------------

# -- Finding the most common words in the Text
text_clean %>% count(word, sort = TRUE)


# -- Do we need to remove other stop words? For instance: "?"
dcustom_stopwords_PT <- bind_rows(data.frame(word = c(iconv("é", "latin1", "UTF-8")),
                                          lexicon = c("SMART")),
                                 dstopwords_PT)
dcustom_stopwords_PT

# -- Removing stop words again
text_clean <- text_clean %>% anti_join(dcustom_stopwords_PT)

# -- Verifying again the most common words
text_clean %>% count(word, sort = TRUE)

# - Visualizing
text_clean %>% visualize_terms(20)

text_clean %>% visualize_cloud(200)


# ------ Frequency
text_freq <- text_clean %>% 
            count(word, sort = TRUE) %>% 
            mutate(proportion = n / sum(n)) %>%
            select(-n) 

text_freq


# --- Comparing frequency between texts
texts_freq <- text_clean %>% 
             mutate(word = str_extract(word, "[a-z']+")) %>%
             count(noticia, word) %>% 
             group_by(noticia) %>%
             mutate(proportion = n / sum(n)) %>%
             ungroup() %>%
             select(-n) %>%
             spread(noticia, proportion) %>%
             gather(noticia, proportion, 'Noticia 2':'Noticia 3')

texts_freq

texts_freq %>% filter(noticia == "Noticia 1")


# -- Comparing texts in a graph
# expect a warning about rows with missing values being removed
 ggplot(texts_freq, aes(x = proportion, y = `Noticia 1`,
        color = abs(`Noticia 1` - proportion))) +
        geom_abline(color = "gray40", lty = 2) +
        geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
        geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
        scale_x_log10(labels = percent_format()) +
        scale_y_log10(labels = percent_format()) +
        scale_color_gradient(limits = c(0, 0.001),
                             low = "darkslategray4", high = "gray75") +
        facet_wrap(~noticia, ncol = 2) +
        theme(legend.position = "none") +
        labs(y = "Noticia 1", x = NULL)

 # How similar and different these sets of word frequencies are -> correlation test
 
 cor.test(data = texts_freq[texts_freq$noticia == "Noticia 2", ], ~ proportion + `Noticia 1`) 
 
 cor.test(data = texts_freq[texts_freq$noticia == "Noticia 3", ], ~ proportion + `Noticia 1`) 

 
# ----------------------------------------------------------------------------------
# Analyzing word and document frequency: tf_idf
# ----------------------------------------------------------------------------------

text_freq_2 <- text_clean %>% 
               count(noticia, word, sort = TRUE) %>% 
               ungroup()
   
total_words <- text_freq_2 %>% group_by(noticia) %>% summarize(total = sum(n))
total_words

text_freq_2 <- left_join(text_freq_2, total_words)
text_freq_2

# Let's look at the distribution of n/total: the number of times a word appears in 
# the text divided by the total number of terms (words) in that text.
# That is exactly what term frequency is

ggplot(text_freq_2, aes(n/total, fill = noticia)) +
      geom_histogram(show.legend = FALSE) +
      xlim(NA, 0.09) +
      facet_wrap(~noticia, ncol = 2, scales = "free_y")

# Zipf's Law
# "The frequency that a word appears is inversely proportional to its rank" George Zipf

text_freq_by_rank <- text_freq_2 %>% mutate(rank = row_number(), term_freq = n/total)
text_freq_by_rank


# Plotting, an inversely proportional relationship between frequency and rank will have 
# a constant, negative slope
text_freq_by_rank %>% ggplot(aes(rank, term_freq)) +
                      geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
                      scale_x_log10()  + scale_y_log10()


# -- Power Law
# https://en.wikipedia.org/wiki/Power_law

# we could view this plot as a broken power law, with, say, three sections. Let's see what the expoent of
# the power law for the middle section of the rank range

rank_subset <- text_freq_by_rank %>% filter(rank < 500, rank > 100)

lm(log10(term_freq) ~ log10(rank), data = rank_subset)

# Let's plot this fitted power law with the data 
text_freq_by_rank %>% ggplot(aes(rank, term_freq)) +
                      geom_abline(intercept = -1.713, slope = -1.1, color = "gray50", linetype = 2) +
                      geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
                      scale_x_log10() + scale_y_log10()


# ----- The bind_tf_idf function

# Calculating tf-idf attempts to find the words that are important (i.e., common) ina a text, but not too common

text_clean_tf_idf <- text_freq_2 %>% bind_tf_idf(word, noticia, n)
text_clean_tf_idf

# tf-idf is very low (near zero) for words that occur in many of the documents in a collection

text_clean_tf_idf %>% select(-total) %>% arrange(desc(tf_idf))

# Let's look at a visualization for these high tf-idf words

text_clean_tf_idf %>% arrange(desc(tf_idf)) %>%
              mutate(word = factor(word, levels = rev(unique(word)))) %>%
              group_by(noticia) %>%
              top_n(15) %>%
              ungroup %>%
              ggplot(aes(word, tf_idf, fill = noticia)) +
              geom_col(show.legend = FALSE) +
              labs(x = NULL, y = "tf-idf") +
              facet_wrap(~noticia, ncol = 2, scales = "free") +
              coord_flip()



#------------------------------------------------------------------------------------------------------------------------
# Sentiment Analysis
#------------------------------------------------------------------------------------------------------------------------

# A sample of the Lexicon PT
sample_n(oplexicon_v3.0, size = 20) %>% arrange(polarity)

# Faz o a jun??o por interse??o.
text_polarity <- inner_join(text_clean,
                           oplexicon_v3.0[, c("term", "polarity")],
                           by = c("word" = "term"))


# tagging positive and negative words in the word cloud
visualize_cloud_polarity(text_polarity, 100)

# ===> Testing

# --- Calculating a net sentiment (positive - negative)

text_polarity_vis1 <- text_polarity %>% 
                  count(noticia, index = line %/% 80, polarity) %>%
                  spread(polarity, n, fill = 0) 

text_polarity_vis2 <- text_polarity %>% 
                  count(noticia, polarity) %>%
                  spread(polarity, n, fill = 0) 
  

ggplot(text_polarity, aes(line, polarity, fill = noticia)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~noticia, ncol = 2, scales = "free_x")


# --- 

text_polarity_vis3 <- text_polarity %>% 
  count(noticia, polarity)

ggplot(text_polarity_vis3, aes(polarity, n, fill = noticia)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~noticia, ncol = 2, scales = "free_x")


#------------------------------------------------------------------------------------------------------------------------
# References
#------------------------------------------------------------------------------------------------------------------------

# http://r-br.2285057.n4.nabble.com/R-br-Importacao-correta-de-palavras-acentuadas-com-a-gdata-read-xls-td3443632.html