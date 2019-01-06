# --- Carrega planilha

# -- Muda o diretorio default para local da planiha de resultados
#    => Coloque o diretório onde se encontra a planilha
setwd("F:/DIRETORIO/SUBDIRETORIO1/SUBDIRETORIO2")

library(readxl)

xsheets1 <- excel_sheets("Resultados Finais Pesquisa - Tratados - 16112018 - 1.0.xlsx")

dResultadosPesquisa <- read_excel("Resultados Finais Pesquisa - Tratados - 16112018 - 1.0.xlsx", sheet="Planilha1")
str(dResultadosPesquisa)

# ---------- Boxplots - Questoes com Escala LIKERT ----------------

library(tidyverse)
library(ggplot2)

# -- Usando Reshape para "fundir" os boxplots
library(reshape2)

#---------------------------------------------------------------------------------------
#--- Questoes do Grupo GR3 - O quanto voce discorda ou concorda com as afirmacoes abaixo

#  - Consigo ser eficiente no uso do meu tempo
#  - Sinto-me incentivado pelo meu gerente a empregar parte do meu tempo para reflexao e 
#    melhoria das minhas atividades
#  - Sou valorizado por buscar a melhoria das atividades que executo
#  - Sinto-me incentivado pelo meu gerente a empregar parte do meu tempo para estudo e 
#    aprendizagem
#  - Minha equipe esta subdimensionada para realizar as atividades que dela sao demandadas
#  - Praticas e controles administrativos desnecessarios afetam 100% do uso do meu tempo 
#  - Observo as pessoas questionando as praticas pouco produtivas (Ex: "Precisamos fazer 
#    isso dessa forma?") 
#  - Minhas atribuicoes nao geram valor para a companhia 
#  - Se minha atribuicoes fossem automatizadas, geraria maior ganho para a companhia 
#  - Sinto-me capaz de assumir atribuicoes que gerariam maior valor para a companhia

vMeltGR3 <-melt(dResultadosPesquisa, measure.vars=c("GR3_Eficiente", "GR3_Incentivado_Melhoria", "GR3_Valorizado_Melhoria", "GR3_Incentivado_Estudo", "GR3_Equipe_Sub", "GR3_Ctrl_Adm", "GR3_Pessoas_Quest", "GR3_Atrib_SemValor", "GR3_Autom_Atrib", "GR3_Capaz"))

# Boxplots com legendas abreviadas
vMeltGR3 %>% mutate(var_reord=reorder(variable, value, FUN = median)) %>% 
  ggplot(aes(Respondente, value, fill = variable)) + 
  geom_boxplot() + xlab("") + ylab("(0) Nao Sei  - (1) Discordo Tot.  ==>  (7) Concordo Tot.") + 
  geom_point(show.legend = TRUE) +
  facet_grid(.~var_reord, scale="free_x") + 
  ggtitle("O quanto voce discorda ou concorda com as afirmacoes abaixo") + theme_bw() +
  scale_fill_discrete(name="Questoes",
                      breaks=c("GR3_Eficiente", "GR3_Incentivado_Melhoria", "GR3_Valorizado_Melhoria", "GR3_Incentivado_Estudo", "GR3_Equipe_Sub", "GR3_Ctrl_Adm", "GR3_Pessoas_Quest", "GR3_Atrib_SemValor", "GR3_Autom_Atrib", "GR3_Capaz"), 
                      labels=c("Consigo ser eficiente ...", "Sinto-me incentivado ...para reflexao e melhoria das minhas atividades", "Sou valorizado por buscar a melhoria ...", "Sinto-me incentivado ... para estudo e aprendizagem", "Minha equipe esta subdimensionada ...", "Praticas e controles administrativos desnecessarios ...", "Observo as pessoas questionando as praticas pouco produtivas...", "Minhas atribuicoes nao geram valor ...", "Se minha atribuicoes fossem automatizadas ...", "Sinto-me capaz de assumir atribuicoes ...")) 



#---------------------------------------------------------------------------------
#--- Questoes do Grupo GR4 - Sobre Boas Praticas que Favorecem a Gestao do Tempo

#  - Meu gestor me da total autonomia para alocar meu tempo
#  - Meu gestor incentiva que eu aloque meu tempo livremente
#  - Minha gerencia adota praticas que favorecem a produtividade e foco
#  - Eu adoto praticas de gestao de tempo que aumentam minha eficiencia e foco
#  - Frequentemente, minha gerancia reserva tempo para refletir sobre melhoria das 
#    praticas do dia a dia


vMeltGR4 <-melt(dResultadosPesquisa, measure.vars=c("GR4_Autonomia_Tempo", "GR4_Gestor_Incentiva", "GR4_Ger_Praticas", "GR4_Eu_Praticas", "GR4_Ger_Reserva_Tempo"))

# Boxplots com legendas abreviadas
vMeltGR4 %>% mutate(var_reord=reorder(variable, value, FUN = median)) %>% 
  ggplot(aes(Respondente, value, fill = variable)) + 
  geom_boxplot() + xlab("") + ylab("(0) Nao Sei  - (1) Discordo Tot.  ==>  (7) Concordo Tot.") + 
  geom_point(show.legend = TRUE) +
  facet_grid(.~var_reord, scale="free_x") + 
  ggtitle("Sobre Boas Praticas que Favorecem a Gestao do Tempo") + theme_bw() +
  scale_fill_discrete(name="Questoes",
                      breaks=c("GR4_Autonomia_Tempo", "GR4_Gestor_Incentiva", "GR4_Ger_Praticas", "GR4_Eu_Praticas", "GR4_Ger_Reserva_Tempo"),
                      labels=c("Meu gestor me da total autonomia...", "Meu gestor incentiva que eu aloque meu tempo...", "Minha gerencia adota praticas ...", "Eu adoto praticas de gestao de tempo...", "...minha gerencia reserva tempo para refletir ...")) 


#------------------------------------------------------------------------------
#--- Questoes do Grupo GR5 - Se eu decidir modificar minha alocacao de tempo

#  - Eu sempre precisaria dar ciencia ao meu gerente 
#  - Meu gerente sempre seria receptivo ao tomar ciencia sobre a mudanca 
#  - Eu seria mais sujeito a ser responsabilizado sobre qualquer falha ou atraso 
#  - Nao acho que eu deveria ter autonomia sobre minha alocacao de tempo


vMeltGR5 <-melt(dResultadosPesquisa, measure.vars=c("GR5_Dar_Ciencia", "GR5_Gestor_Receptivo", "GR5_Responsabilizado", "GR5_Não_Autonomia"))

# Boxplots com legendas abreviadas
vMeltGR5 %>% mutate(var_reord=reorder(variable, value, FUN = median)) %>% 
  ggplot(aes(Respondente, value, fill = variable)) + 
  geom_boxplot() + xlab("") + ylab("(0) Nao Sei  - (1) Discordo Tot.  ==>  (7) Concordo Tot.") + 
  geom_point(show.legend = TRUE) +
  facet_grid(.~var_reord, scale="free_x") + 
  ggtitle("Se eu decidir modificar minha alocacao de tempo") + theme_bw() +
  scale_fill_discrete(name="Questoes",
                      breaks=c("GR5_Dar_Ciencia", "GR5_Gestor_Receptivo", "GR5_Responsabilizado", "GR5_Não_Autonomia"),
                      labels=c("Eu sempre precisaria dar ciencia ...", "Meu gerente sempre seria receptivo ... ", "Eu seria mais sujeito a ser responsabilizado ...", "Nao acho que eu deveria ter autonomia ...")) 


#--------------------------------------------------------------------------------------------
#--- Questoes do Grupo GR6 - Na sua opiniao, as mudancas esperadas pela Iniciativa de TD 
#    irao:

#  - Impulsionar a Petrobras a ser lider em desenvolvimento e adocao de tecnologias digitais 
#    na industria em que esta inserida.
#  - TD ira transformar como a Petrobras gera valor para seus p?blicos de interesse
#  - Ampliar a conexao entre as pessoas 
#  - Aumentar a seguranca das atividades da Petrobras
#  - Aumentar investimento na formacao dos profissionais 
#  - Tornar a forca de trabalho mais eficiente
#  - Melhorar a qualidade do tempo dedicado ao trabalho 
#  - Gerar reducao de pessoal com sobrecarga dos remanescentes 

vMeltGR6 <-melt(dResultadosPesquisa, measure.vars=c("GR6_Impulsionar_PB", "GR6_Transformar_PB", "GR6_Conexao_Pessoas", "GR6_Seguranca_Atividades", "GR6_Formacao_Pessoal", "GR6_Pessoal_Eficiente", "GR6_Qualidade_Tempo", "GR6_Reducao_Pessoal"))

# Boxplots com legendas abreviadas
vMeltGR6 %>% mutate(var_reord=reorder(variable, value, FUN = median)) %>% 
  ggplot(aes(Respondente, value, fill = variable)) + 
  geom_boxplot() + xlab("") + ylab("(0) Nao Sei  - (1) Discordo Tot.  ==>  (7) Concordo Tot.") + 
  geom_point(show.legend = TRUE) +
  facet_grid(.~var_reord, scale="free_x") + 
  ggtitle("Na sua opiniao, as mudancas esperadas pela Iniciativa de TD irao") + theme_bw() +
  scale_fill_discrete(name="Questoes",
                      breaks=c("GR6_Impulsionar_PB", "GR6_Transformar_PB", "GR6_Conexao_Pessoas", "GR6_Seguranca_Atividades", "GR6_Formacao_Pessoal", "GR6_Pessoal_Eficiente", "GR6_Qualidade_Tempo", "GR6_Reducao_Pessoal"),
                      labels=c("Impulsionar a Petrobras a ser lider em desenvolvimento e adocao de tecnologias digitais...", "TD ira transformar como a Petrobras gera valor...", "Ampliar a conexao entre as pessoas", "Aumentar a seguranca das atividades da Petrobras", "Aumentar investimento na formacao dos profissionais", "Tornar a forca de trabalho mais eficiente", "Melhorar a qualidade do tempo dedicado ao trabalho", "Gerar reducao de pessoal com sobrecarga dos remanescentes")) 


#--------------------------------------------------------------------------------------------
#--- Questoes do Grupo GR7 - Sobre a relevancia do tema TD

#  - Mudancas como a TD sao importantes e urgentes para a Industria de oleo & Gas (O&G) e Energia
#  - A Industria de O&G e Energia procrastinou dar atencao a TD em comparacao a outras 
#    industrias
#  - A Petrobras procrastinou dar atencao a TD em comparacao a outras empresas da industria 
#    de O&G e Energia
#  - TD nao e tao disruptivo na industria na qual a Petrobras atua 
#  - E oportuno comecar essa transformacao nesse momento da companhia

                 
vMeltGR7 <-melt(dResultadosPesquisa, measure.vars=c("GR7_TD_Para_OG", "GR7_Atraso_OG", "GR7_Atraso_PB", "GR7_TD_NaoDirsuptiva", "GR7_Oportuno"))


# Boxplots com legendas abreviadas
vMeltGR7 %>% mutate(var_reord=reorder(variable, value, FUN = median)) %>% 
  ggplot(aes(Respondente, value, fill = variable)) + 
  geom_boxplot() + xlab("") + ylab("(0) N?o Sei  - (1) Discordo Tot.  ==>  (7) Concordo Tot.") + 
  geom_point(show.legend = TRUE) +
  facet_grid(.~var_reord, scale="free_x") + 
  ggtitle("Sobre a relev?ncia do tema TD") + theme_bw() +
  scale_fill_discrete(name="Questoes",
                      breaks=c("GR7_TD_Para_OG", "GR7_Atraso_OG", "GR7_Atraso_PB", "GR7_TD_NaoDirsuptiva", "GR7_Oportuno"),
                      labels=c("Mudancas como a TD sao importantes...", "A Industria de O&G e Energia procrastinou ...", "A Petrobras procrastinou dar atencao a TD ...", "TD nao e tao disruptiva na industria na qual a Petrobras atua", "E oportuno comecar essa transformacao nesse momento ...")) 


#--------------------------------------------------------------------------------------------
#--- Questoes do Grupo GR8 - Sobre o interesse no tema TD

#  - Sinto-me interessado em engajar nas acoes relacionadas a TD
#  - Somente me engajaria em acoes relacionadas a TD se fosse formalmente requisitado
#  - As pessoas do meu convivio na empresa se interessam em engajar nas acoes relacionadas a TD
#  - As pessoas nao querem mudar suas atitudes, habitos e convicoes
#  - As pessoas tem interesse suficiente para se desenvolver no tema de forma autonoma


vMeltGR8 <-melt(dResultadosPesquisa, measure.vars=c("GR8_Interessado", "GR8_Engajamento_Formal", "GR8_Pessoas_interessadas", "GR8_Nao_Mudar", "GR8_Pessoas_TD_Autonoma"))


# Boxplots com legendas abreviadas
vMeltGR8 %>% mutate(var_reord=reorder(variable, value, FUN = median)) %>% 
  ggplot(aes(Respondente, value, fill = variable)) + 
  geom_boxplot() + xlab("") + ylab("(0) Nao Sei  - (1) Discordo Tot.  ==>  (7) Concordo Tot.") + 
  geom_point(show.legend = TRUE) +
  facet_grid(.~var_reord, scale="free_x") + 
  ggtitle("Sobre o interesse no tema TD") + theme_bw() +
  scale_fill_discrete(name="Questoes",
                      breaks=c("GR8_Interessado", "GR8_Engajamento_Formal", "GR8_Pessoas_interessadas", "GR8_Nao_Mudar", "GR8_Pessoas_TD_Autonoma"),
                      labels=c("Sinto-me interessado em engajar...", "Somente me engajaria em acoes relacionadas a TD ....", "As pessoas do meu convivio na empresa se interessam ...", "As pessoas nao querem mudar ...", "As pessoas tem interesse suficiente para se desenvolver...")) 

