# --- Carrega planilha

# -- Muda diretorio
setwd("F:/Sergio/Estudo/Ciencia de Dados/Study/Pesquisa Desafios")

library(readxl)

xsheets1 <- excel_sheets("Resultados Finais Pesquisa.xlsm")

dResultadosPesquisa <- read_excel("Resultados Finais Pesquisa.xlsm", sheet="Favorabilidade")
str(dResultadosPesquisa)

# ---------- Histograma ----------------

library(tidyverse)
library(ggplot2)

# -- Usando Reshape para "fundir" os boxplots
library(reshape2)

#---------------------------------------------------------------------------------------
#--- Favorabilidade das respostas ao Grupo GR3 - O quanto voce discorda ou concorda com as afirma??es abaixo

#  Consigo ser eficiente no uso do meu tempo
#  Sinto-me incentivado pelo meu gerente a empregar parte do meu tempo para reflex?o e melhoria das minhas atividades
#  Sou valorizado por buscar a melhoria das atividades que executo
#  Sinto-me incentivado pelo meu gerente a empregar parte do meu tempo para estudo e aprendizagem
#  Minha equipe est? subdimensionada para realizar as atividades que dela s?o demandadas
#  Pr?ticas e controles administrativos desnecess?rios afetam 100% do uso do meu tempo 
#  Observo as pessoas questionando as pr?ticas pouco produtivas (Ex: "Precisamos fazer isso dessa forma?") 
#  Minhas atribui??es n?o geram valor para a companhia 
#  Se minha atribui??es fossem automatizadas, geraria maior ganho para a companhia 
#  Sinto-me capaz de assumir atribui??es que gerariam maior valor para a companhia

# Teste de Favorabilidade
vMeltGR3 <-melt(dResultadosPesquisa, measure.vars=c("GR3_Eficiente_Fav", "GR3_Incentivado_Melhoria_Fav", "GR3_Valorizado_Melhoria_Fav", "GR3_Incentivado_Estudo_Fav", "GR3_Equipe_Sub_Fav", "GR3_Ctrl_Adm_Fav", "GR3_Pessoas_Quest_Fav", "GR3_Atrib_SemValor_Fav", "GR3_Autom_Atrib_Fav", "GR3_Capaz_Fav"))

vMeltGR3 %>%  ggplot(aes(value, fill = variable)) + geom_histogram(binwidth = 5)

# VERSAO LEGENDA COMPLETA
vMeltGR3 %>% mutate(var_reord=reorder(variable, value, FUN = median)) %>% 
  ggplot(aes(Respondente, value, fill = variable)) + 
  geom_boxplot() + xlab("") + ylab("(0) N?o Sei  - (1) Discordo Tot.  ==>  (7) Concordo Tot.") + 
  geom_point(show.legend = TRUE) +
  facet_grid(.~var_reord, scale="free_x") + 
  ggtitle("O quanto você discorda ou concorda com as afirmações abaixo") + theme_bw() +
  scale_fill_discrete(name="Quest?es",
                      breaks=c("GR3_Eficiente", "GR3_Incentivado_Melhoria", "GR3_Valorizado_Melhoria", "GR3_Incentivado_Estudo", "GR3_Equipe_Sub", "GR3_Ctrl_Adm", "GR3_Pessoas_Quest", "GR3_Atrib_SemValor", "GR3_Autom_Atrib", "GR3_Capaz"),
                      labels=c("Consigo ser eficiente no uso do meu tempo", "Sinto-me incentivado pelo meu gerente a empregar parte do meu tempo para reflex?o e melhoria das minhas atividades", "Sou valorizado por buscar a melhoria das atividades que executo", "Sinto-me incentivado pelo meu gerente a empregar parte do meu tempo para estudo e aprendizagem", "Minha equipe est? subdimensionada para realizar as atividades que dela s?o demandadas", "Pr?ticas e controles administrativos desnecess?rios afetam 100% do uso do meu tempo", "Observo as pessoas questionando as pr?ticas pouco produtivas (Ex: 'Precisamos fazer isso dessa forma?')", "Minhas atribui??es n?o geram valor para a companhia", "Se minha atribui??es fossem automatizadas, geraria maior ganho para a companhia", "Sinto-me capaz de assumir atribui??es que gerariam maior valor para a companhia")) 
