# ================================================================================================
#    Text Mining With R
# ================================================================================================

# ==== PRINCIPLES

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

cor.test(data = frequency[frequency$author == "Bronte Sisters", ], ~ proportion + `Jane Austen`) 


get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")


















