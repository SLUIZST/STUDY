# 18/10/2018 - Sergio Luiz
# Carrega planilha de resultados da Pesquisa sobre Dasfios da TD

require(xlsx)
resultados <- read.xlsx("ANALISE.xlsx", sheetName = "Planilha1")

head(resultados)

# Loading dplyr  (biblioteca para tratamento de dados)
library(dplyr)

# Converte campo Factor para Numeric
resultados2 <- mutate(resultados, GR3_Incentivado_Melhoria_N = as.numeric(as.character(resultados$GR3_Incentivado_Melhoria)), + 
                                  GR3_Valorizado_Melhoria_N = as.numeric(as.character(resultados$GR3_Valorizado_Melhoria)) )


## ===== Testes Gráficos Simples ======

hist(resultados$GR3_Incentivado_Melhoria_N)

boxplot(Custo_R~Servico, data = resultados2)





## =====  Teste GGPLOT  ===========

# --------------------------------------------
# Carrega bibliotecas

library(tidyverse)
library(ggplot2)
##library(ggthemes)
##library(ggrepel)

# --------------------------------------------
# Filtra projetos

# Tira projetos com custo realizado = ZERO
projetos <- select(projs, Projeto, Custo_R, Servico, Solucao) %>% filter(Custo_R > 0)

# Cria coluna com coluna de Custo realizado em Milhares
projetos_M <- mutate(projetos, Custo_R_M = Custo_R/1000)

##proj_plot <- ggplot(data = projetos)

# --------------------------------------------
# Testes com Histograma

ggplot(data = projetos, aes(x = Custo_R)) + geom_histogram() + scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10")

ggplot(data = projetos, aes(x = Custo_R)) + geom_histogram() + scale_x_log10() + scale_y_log10() +
xlab("Custo Realizado (R$)") + ylab("Quantidade de Projetos")

ggplot(data = projetos, aes(x = Custo_R)) + geom_histogram(binwidth = 1) +
xlab("Custo Realizado (R$)") + ylab("Quantidade de Projetos") + ggtitle("Custo Realizado Projetos Concluídos/Encerrados")


# Teste com Custo_R_M
ggplot(data = projetos_M, aes(x = Custo_R_M)) + geom_histogram(binwidth = 500) +
xlab("Custo Realizado (MIL R$)") + ylab("Quantidade de Projetos") + ggtitle("Custo Realizado Projetos Concluídos/Encerrados")


# Teste tirando os outliers
proj_sem_outlier <- projetos_M %>% filter(Custo_R_M < 10000)
ggplot(data = proj_sem_outlier, aes(x = Custo_R_M)) + geom_histogram(binwidth = 500) + 
xlab("Custo Realizado (MIL R$)") + ylab("Quantidade de Projetos") + ggtitle("Custo Realizado Projetos Concluídos/Encerrados")

# --------------------------------------------
# BOXPLOT

# ---> COM OUTLIERS
p <- projetos_M %>% mutate(Servico=reorder(Servico, Custo_R_M, FUN = median)) %>% ggplot(aes(Servico, Custo_R_M, fill = Solucao)) + 
geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
xlab("") + ylab("Custo Realizado Milhares R$") + ggtitle("Projetos Concluídos/Encerrados")
p

# --> SEM OUTLIERS
proj_sem_outlier <- projetos_M %>% filter(Custo_R_M < 10000)
p2 <- proj_sem_outlier %>% mutate(Servico=reorder(Servico, Custo_R_M, FUN = median)) %>% ggplot(aes(Servico, Custo_R_M, fill = Solucao)) + 
geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
xlab("") + ylab("Custo Realizado Milhares R$") + ggtitle("Projetos Concluídos/Encerrados - Custo Realizado por Tipo de Serviço")
p2

# + scale_y_continuous(trans = "log100")

# --------------------------------------------


## DENSITY PLOT

# --> SEM OUTLIERS
proj_sem_outlier <- projetos_M %>% filter(Custo_R_M < 10000)
p3 <- proj_sem_outlier %>% ggplot(aes(Custo_R_M, fill = Solucao)) + geom_density(alpha = 0.2) + 
xlab("Custo Realizado Milhares R$") + ggtitle("Projetos Concluídos/Encerrados - Custo Realizado por Tipo de Solução")
p3





proj_plot + geom_point(aes(Projeto, Custo)) 
proj_plot + geom_histogram() 


