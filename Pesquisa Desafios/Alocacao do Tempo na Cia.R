# --- Carrega planilha

# -- Muda diretório
setwd("V:/TIC_ESTTIC_MIE/NP-2/PROCESSOS/Experiência do Usuário/UX + Transformação Digital/Pesquisa sobre Desafios da Iniciacao/Resultados")

library(readxl)

xsheets1 <- excel_sheets("Resultados Finais Pesquisa - Tratados - 16112018 - 1.0.xlsx")

dResultadosPesquisa <- read_excel("Resultados Finais Pesquisa - Tratados - 16112018 - 1.0.xlsx", sheet="Planilha1")
str(dResultadosPesquisa)

# ---------- Boxplots - Questões com Escala LIKERT ----------------

library(tidyverse)
library(ggplot2)

# -- Usando Reshape para "fundir" os boxplots
library(reshape2)

#----------------------------------------------------------------------------------------------------
#--- Questões do Grupo GR2 - Aproximadamente quantos por cento do meu tempo na companhia é dedicado a

#  Minhas atribuições 
#  Pensar melhorias para o processo
#  Executar melhorias no processo
#  Estudo e Aprendizagem
#  Rotinas Administrativas (relatório de frequência, RAC, DSMS, etc)
#  Outra atividade
#  Não sei dizer

vMeltGR2 <-melt(dResultadosPesquisa, measure.vars=c("GR2_Atribuicoes_N", "GR2_Pensar_Melhorias_N", "GR2_Executar_Melhorias_N", "GR2_Estudo_N", "GR2_Rotina_N", "GR2_Outras_N"))


# VERSÃO LEGENDA ABREVIADA
vMeltGR2 %>% mutate(var_reord=reorder(variable, value, FUN = median)) %>% 
  ggplot(aes(Respondente, value, fill = variable)) + 
  geom_boxplot() + xlab("") + ylab("Não Sei	Dizer/Nenhum(0%) - Pouco(15%) - Algum(30%) - Boa parte(50%) - A maioria(70%) - Quase todo(90%) - Todo(100%)") + 
  geom_point(show.legend = TRUE) +
  facet_grid(.~var_reord, scale="free_x") + 
  ggtitle("Aproximadamente quantos por cento do meu tempo na companhia é dedicado a") + theme_bw() +
  scale_fill_discrete(name="Questões",
                      breaks=c("GR2_Atribuicoes_N", "GR2_Pensar_Melhorias_N", "GR2_Executar_Melhorias_N", "GR2_Estudo_N", "GR2_Rotina_N", "GR2_Outras_N"),
                      labels=c("Minhas atribuições", "Pensar melhorias para o processo", "Executar melhorias no processo", "Estudo e Aprendizagem", "Rotinas Administrativas (relatório de frequência, RAC, DSMS, etc)", "Outra atividade")) 

