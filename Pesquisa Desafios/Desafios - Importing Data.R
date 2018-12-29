#-------------- Importing the data -----------

#  Working directory
getwd()

# Changing the working directory 
setwd("V:/TIC_ESTTIC_MIE/NP-2/PROCESSOS/Experiência do Usuário/UX + Transformação Digital/Pesquisa sobre Desafios da Iniciacao/Resultados")

library(readxl)

xsheets1 <- excel_sheets("Resultados Finais Pesquisa - Tratados - 16112018 - 1.0.xlsx")

dResultadosPesquisa <- read_excel("Resultados Finais Pesquisa - Tratados - 16112018 - 1.0.xlsx", sheet="Planilha1")
str(ResultadosPesquisa)