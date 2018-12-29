# Changing the working directory 
setwd("V:/TIC_ESTTIC_MIE/NP-2/PROCESSOS/Experiência do Usuário/UX + Transformação Digital/Pesquisa sobre Desafios da Iniciacao/Resultados")

# Carregando a planilha de resultados 

library(readxl)

xsheets1 <- excel_sheets("Resultados Finais Pesquisa - Tratados - 16112018 - 1.0.xlsx")

dResultadosPesquisa <- read_excel("Resultados Finais Pesquisa - Tratados - 16112018 - 1.0.xlsx", sheet="Planilha1")
str(dResultadosPesquisa)

# Bibliotecas
library(tidyverse)
library(ggplot2)
library(lubridate)
library(tidyr)
library(scales)
set.seed(1)

#-----------------------------------------------------------------------
# Analisando as respostas em GR9_Faltou_Desafio 
#-----------------------------------------------------------------------
library(tidytext)

# -------
# Example
# --------
poem <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")

text_df <- data_frame(line = 1:4, text = poem)

text_df %>%  unnest_tokens(word, text)

# Reference: https://www.tidytextmining.com/tidytext.html
#-----------------------------------------------------------


dResultText <- dResultadosPesquisa %>% filter(!is.na(dResultadosPesquisa$GR9_Faltou_Desafio))
vFaltouDesafio <- dResultText$GR9_Faltou_Desafio
Id <- dResultText$ID

FaltouDesafio_df <- data_frame(line=Id, text=vFaltouDesafio)
head(FaltouDesafio_df)


words_FaltouDesafio <- FaltouDesafio_df %>% unnest_tokens(word, text)

# Contagem simples
words_FaltouDesafio %>% 
  count(word) %>%
  arrange(desc(n))

# stopwords
# ref: https://www.rdocumentation.org/packages/corpus/versions/0.9.1/topics/stopwords

library("stopwords")
stw <- stopwords("portuguese")

# stop_words => Just for english words

words_FaltouDesafio_2 <- words_FaltouDesafio %>% filter(!word %in% stw)


WordsFaltouDesaf <- data_frame(Word=words_FaltouDesafio_2$word)
write.csv(WordsFaltouDesaf, "WordsFaltouDesafio.csv", row.names = FALSE)

# Contagem simples pós filtragem com stopwords
count_words <- words_FaltouDesafio_2 %>% 
  count(word) %>%
  arrange(desc(n))


# --- Analisando as frequências

freq <- mutate(count_words, proportion = n/sum(n))

freq %>% arrange(desc(proportion))

library("ggplot2")

p <- ggplot(freq, aes(x = proportion, y = n, label = word)) +
  #geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(nudge_x=0.075, check_overlap = TRUE, vjust = 1.5) +
  #geom_text_repel() +
  scale_x_log10(labels = percent_format()) + 
  scale_y_log10() +
  xlab("Frequência") +
  ylab("Contagem") + 
  scale_color_gradient(limits = c(0, 0.028), low = "darkslategray4", high = "gray75") # +
  #theme(legend.position="none") +
  #theme_economist()

#-----------------------------------------------------------------------
# Analisando as respostas em GR9_Acao_FaltaTempo 
#-----------------------------------------------------------------------

library(tidytext)

dAcaoFaltaTempo <- dResultadosPesquisa %>% filter(!is.na(dResultadosPesquisa$GR9_Acao_FaltaTempo))
vFaltaTempo <- dAcaoFaltaTempo$GR9_Acao_FaltaTempo
Id2 <- dAcaoFaltaTempo$ID

FaltaTempo_df <- data_frame(line=Id2, text=vFaltaTempo)

words_FaltaTempo <- FaltaTempo_df %>% unnest_tokens(word, text)

# Contagem simples
words_FaltaTempo %>% 
  count(word) %>%
  arrange(desc(n))

# stopwords
# ref: https://www.rdocumentation.org/packages/corpus/versions/0.9.1/topics/stopwords

library("stopwords")
stw <- stopwords("portuguese")

words_FaltaTempo_2 <- words_FaltaTempo %>% filter(!word %in% stw)

# Exporta para arquivo csv os termos filtrados sem as stopwords, para geração de nuvem de palavras
WordsFaltaT <- data_frame(Word=words_FaltaTempo_2$word)
write.csv(WordsFaltaT, "WordsAcoesFaltaTempo.csv", row.names = FALSE)

# Contagem simples pós filtragem com stopwords
count_words <- words_FaltaTempo_2 %>% 
  count(word) %>%
  arrange(desc(n))

#-----------------------------------------------------------------------
# Analisando as respostas em GR9_Acao_Relevancia 
#-----------------------------------------------------------------------

library(tidytext)

dAcaoRelevancia <- dResultadosPesquisa %>% filter(!is.na(dResultadosPesquisa$GR9_Acao_Relevancia))
vRelevancia <- dAcaoRelevancia$GR9_Acao_Relevancia
Id3 <- dAcaoRelevancia$ID

Relevancia_df <- data_frame(line=Id3, text=vRelevancia)

words_Relevancia <- Relevancia_df %>% unnest_tokens(word, text)

# Contagem simples
words_Relevancia %>% 
  count(word) %>%
  arrange(desc(n))

library("stopwords")
stw <- stopwords("portuguese")

words_Relevancia_2 <- words_Relevancia %>% filter(!word %in% stw)

# Exporta para arquivo csv os termos filtrados sem as stopwords, para geração de nuvem de palavras
WordsRelevT <- data_frame(Word=words_Relevancia_2$word)
write.csv(WordsRelevT, "WordsAcoesRelevancia.csv", row.names = FALSE)

# Contagem simples pós filtragem com stopwords
count_words <- words_Relevancia_2 %>% 
  count(word) %>%
  arrange(desc(n))

