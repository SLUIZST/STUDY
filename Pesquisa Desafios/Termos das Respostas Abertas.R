# -- Muda o diretorio default para local da planiha de resultados
#    => Coloque o diretório onde se encontra a planilha
setwd("F:/DIRETORIO/SUBDIRETORIO1/SUBDIRETORIO2")

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
library(tidytext)
set.seed(1)

#-----------------------------------------------------------------------
# Analisando as respostas em GR9_Faltou_Desafio 
#-----------------------------------------------------------------------

dResultText <- dResultadosPesquisa %>% filter(!is.na(dResultadosPesquisa$GR9_Faltou_Desafio))
vFaltouDesafio <- dResultText$GR9_Faltou_Desafio
Id <- dResultText$ID

FaltouDesafio_df <- data_frame(line=Id, text=vFaltouDesafio)
head(FaltouDesafio_df)

# identifica as palavras
words_FaltouDesafio <- FaltouDesafio_df %>% unnest_tokens(word, text)

# Contagem simples
words_FaltouDesafio %>% 
  count(word) %>%
  arrange(desc(n))

# stopwords
# ref: https://www.rdocumentation.org/packages/corpus/versions/0.9.1/topics/stopwords

library("stopwords")
stw <- stopwords("portuguese")

# Retira as stop_words 
words_FaltouDesafio_2 <- words_FaltouDesafio %>% filter(!word %in% stw)

# Gera planilha para geração da tag cloud
WordsFaltouDesaf <- data_frame(Word=words_FaltouDesafio_2$word)
write.csv(WordsFaltouDesaf, "WordsFaltouDesafio.csv", row.names = FALSE)

# Contagem simples pos filtragem com stopwords
count_words <- words_FaltouDesafio_2 %>% 
  count(word) %>%
  arrange(desc(n))


# --- Analisando as frequencias (TESTE)
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
  xlab("Frequ?ncia") +
  ylab("Contagem") + 
  scale_color_gradient(limits = c(0, 0.028), low = "darkslategray4", high = "gray75") 

#-----------------------------------------------------------------------
# Analisando as respostas em GR9_Acao_FaltaTempo 
#-----------------------------------------------------------------------

dAcaoFaltaTempo <- dResultadosPesquisa %>% filter(!is.na(dResultadosPesquisa$GR9_Acao_FaltaTempo))
vFaltaTempo <- dAcaoFaltaTempo$GR9_Acao_FaltaTempo
Id2 <- dAcaoFaltaTempo$ID

FaltaTempo_df <- data_frame(line=Id2, text=vFaltaTempo)

# identifica as palavras
words_FaltaTempo <- FaltaTempo_df %>% unnest_tokens(word, text)

# Contagem simples
words_FaltaTempo %>% 
  count(word) %>%
  arrange(desc(n))

# retira as stopwords
library("stopwords")
stw <- stopwords("portuguese")

words_FaltaTempo_2 <- words_FaltaTempo %>% filter(!word %in% stw)

# Exporta para arquivo csv os termos filtrados sem as stopwords, para gera??o de nuvem de palavras
WordsFaltaT <- data_frame(Word=words_FaltaTempo_2$word)
write.csv(WordsFaltaT, "WordsAcoesFaltaTempo.csv", row.names = FALSE)

# Contagem simples pos filtragem com stopwords
count_words <- words_FaltaTempo_2 %>% 
  count(word) %>%
  arrange(desc(n))

#-----------------------------------------------------------------------
# Analisando as respostas em GR9_Acao_Relevancia 
#-----------------------------------------------------------------------

dAcaoRelevancia <- dResultadosPesquisa %>% filter(!is.na(dResultadosPesquisa$GR9_Acao_Relevancia))
vRelevancia <- dAcaoRelevancia$GR9_Acao_Relevancia
Id3 <- dAcaoRelevancia$ID

Relevancia_df <- data_frame(line=Id3, text=vRelevancia)

# identifica as palavras
words_Relevancia <- Relevancia_df %>% unnest_tokens(word, text)

# Contagem simples
words_Relevancia %>% 
  count(word) %>%
  arrange(desc(n))

# retira as stopwords
library("stopwords")
stw <- stopwords("portuguese")

words_Relevancia_2 <- words_Relevancia %>% filter(!word %in% stw)

# Exporta para arquivo csv os termos filtrados sem as stopwords, para gera??o de nuvem de palavras
WordsRelevT <- data_frame(Word=words_Relevancia_2$word)
write.csv(WordsRelevT, "WordsAcoesRelevancia.csv", row.names = FALSE)

# Contagem simples p?s filtragem com stopwords
count_words <- words_Relevancia_2 %>% 
  count(word) %>%
  arrange(desc(n))

