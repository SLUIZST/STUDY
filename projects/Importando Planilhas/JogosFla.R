# ----------------------------------------------------------
# 2018/11/20 - Jogos Fla
#              Sergio Luiz    
# ----------------------------------------------------------

#-------------- Importing the data -----------

#  Working directory
getwd()

# Changing the working directory 
setwd("F:/Sergio/Estudo/Ciencia de Dados/Study/Data Wrangling/Arquivos")

library(readxl)

xsheets <- excel_sheets("Jogos Fla.xlsx")
xsheets

dJogosFla <- read_excel("Jogos Fla.xlsx", sheet="2018")
dJogosFla

dJogosFla$Competição

