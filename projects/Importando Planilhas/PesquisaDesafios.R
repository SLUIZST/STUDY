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


