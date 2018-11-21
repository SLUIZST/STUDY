# ----------------------------------------------------------
# 2018/11/20 - Resultados Finais Pesquisa Desafios
#              Sergio Luiz    
# ----------------------------------------------------------

#-------------- Importing the data -----------

#  Working directory
getwd()

# Changing the working directory 
setwd("F:/Sergio/Estudo/Ciencia de Dados/Study/Data Wrangling/Arquivos")

library(readxl)

xsheets1 <- excel_sheets("Resultados Finais Pesquisa.xlsx")
xsheets1

dResultadosPesquisa <- read_excel("Resultados Finais Pesquisa.xlsx", sheet="Planilha1")
dResultadosPesquisa

# --- Testes com Boxplot
library(tidyverse)
library(ggplot2)

p_GR4_Auton <- dResultadosPesquisa %>% ggplot(aes(Respondente, GR4_Autonomia_Tempo)) + 
               geom_boxplot() + 
               theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
               xlab("") + ylab("Autonomia Tempo")

p_GR4_Auton

p_GR4_Gestor_Incent <- dResultadosPesquisa %>% ggplot(aes(Respondente, GR4_Gestor_Incentiva)) + 
                      geom_boxplot() + 
                      theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
                      xlab("") + ylab("Gestor Incentiva")

library(gridExtra)
grid.arrange(p_GR4_Auton, p_GR4_Gestor_Incent, ncol = 2)

# -> TIP: https://www.r-bloggers.com/extra-extra-get-your-gridextra/


