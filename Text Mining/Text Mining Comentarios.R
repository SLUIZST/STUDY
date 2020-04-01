#-------------- Importing the data -----------

#  Working directory
getwd()

# Changing the working directory 
setwd("R:/Equipe/Sergio/Pessoal/Capacitação/Data Science/R")

# Reading the comments
library(readxl)

dComentarios <- read_excel("Comentários sobre TD.xlsx", sheet="Comentarios")
head(dComentarios)


# -- In order to turn it into a tidy text dataset, we first need o put it into a data frame
library(dplyr)
library(stringr)

df <- data_frame(line = 1:83, text = dComentarios$Texto)
df

# -- Breakin the text into individual tokens (Tokenization) and tranforming it into a tidy data structure
library(tidytext)
tidy_comments <- df %>% unnest_tokens(word, text)


# -- removing the stop words
#data("stop_words")

# https://cran.r-project.org/web/packages/stopwords/stopwords.pdf
# https://pt.stackoverflow.com/questions/51548/nuvem-de-palavras-no-r

library(stopwords)

stpw <- stopwords(language="portuguese")
class(stpw)

# TEST => converting the stopwords into a data_frame in order to do the anti-join with tidy_comments
df_stpw <- data_frame(word = stpw, lexicon="SMART")


comments_clean <- tidy_comments %>% anti_join(df_stpw)

# -- Finding the most common words in comments

comments_clean %>% count(word, sort = TRUE)

# -- Using ggplot2 for this

library(ggplot2)

comments_clean %>% count(word, sort = TRUE) %>%
                   filter(n > 20) %>%
                   mutate(word = reorder(word, n)) %>%
                   ggplot(aes(word, n)) + geom_col() + xlab(NULL) + coord_flip()


#-- Wordclouds

library("wordcloud")

comments_clean %>% count(word) %>% with(wordcloud(word, n, max.words = 200))


# https://cran.r-project.org/web/packages/syuzhet/vignettes/syuzhet-vignette.html

# http://www.leg.ufpr.br/~walmes/ensino/mintex/tutorials/03-sentimento.html

# Dicionários léxicos em PT.
library(lexiconPT)

# Uma amostra do dicionário de termos rotulados.
sample_n(oplexicon_v3.0, size = 20) %>%
  arrange(polarity)

# Faz o a junção por interseção.
comments_pol <- inner_join(comments_clean,
                     oplexicon_v3.0[, c("term", "polarity")],
                     by = c("word" = "term"))

# Agora o termos tem sua polaridade presente na tabela.
sample_n(comments_pol, size = 20)


#-- tagging positive and negative words in the word cloud

#get_sentiments("nrc")

library("wordcloud")
library(reshape2)

comments_pol%>%
  count(word, polarity, sort = TRUE) %>%
  acast(word ~ polarity, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80", "blue"),
                   max.words = 100)


