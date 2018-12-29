# --- Carrega planilha

# -- Muda diretorio
setwd("F:/Sergio/Estudo/Ciencia de Dados/Study/Pesquisa Desafios")

library(readxl)

xsheets1 <- excel_sheets("Resultados Finais Pesquisa.xlsx")

dResultadosPesquisa <- read_excel("Resultados Finais Pesquisa.xlsx", sheet="Planilha1")
str(dResultadosPesquisa)

# ---------- Boxplots - Quest?es com Escala LIKERT ----------------

library(tidyverse)
library(ggplot2)

# -- Usando Reshape para "fundir" os boxplots
library(reshape2)

#---------------------------------------------------------------------------------------
#--- Quest?es do Grupo GR3 - O quanto voc? discorda ou concorda com as afirma??es abaixo

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

vMeltGR3 <-melt(dResultadosPesquisa, measure.vars=c("GR3_Eficiente", "GR3_Incentivado_Melhoria", "GR3_Valorizado_Melhoria", "GR3_Incentivado_Estudo", "GR3_Equipe_Sub", "GR3_Ctrl_Adm", "GR3_Pessoas_Quest", "GR3_Atrib_SemValor", "GR3_Autom_Atrib", "GR3_Capaz"))


# VERS?O LEGENDA ABREVIADA
vMeltGR3 %>% mutate(var_reord=reorder(variable, value, FUN = median)) %>% 
  ggplot(aes(Respondente, value, fill = variable)) + 
  geom_boxplot() + xlab("") + ylab("(0) N?o Sei  - (1) Discordo Tot.  ==>  (7) Concordo Tot.") + 
  geom_point(show.legend = TRUE) +
  facet_grid(.~var_reord, scale="free_x") + 
  ggtitle("O quanto voc? discorda ou concorda com as afirma??es abaixo") + theme_bw() +
  scale_fill_discrete(name="Quest?es",
                      breaks=c("GR3_Eficiente", "GR3_Incentivado_Melhoria", "GR3_Valorizado_Melhoria", "GR3_Incentivado_Estudo", "GR3_Equipe_Sub", "GR3_Ctrl_Adm", "GR3_Pessoas_Quest", "GR3_Atrib_SemValor", "GR3_Autom_Atrib", "GR3_Capaz"),
                      labels=c("Consigo ser eficiente no uso do meu tempo", "Sinto-me incentivado pelo meu gerente...", "Sou valorizado por buscar a melhoria...", "Sinto-me incentivado ... tempo para estudo e aprendizagem", "Minha equipe est? subdimensionada...", "Pr?ticas e controles administrativos desnecess?rios ...", "Observo as pessoas questionando...", "Minhas atribui??es n?o geram valor...", "Se minha atribui??es fossem automatizadas...", "Sinto-me capaz ...")) 


# VERS?O LEGENDA COMPLETA
vMeltGR3 %>% mutate(var_reord=reorder(variable, value, FUN = median)) %>% 
  ggplot(aes(Respondente, value, fill = variable)) + 
  geom_boxplot() + xlab("") + ylab("(0) N?o Sei  - (1) Discordo Tot.  ==>  (7) Concordo Tot.") + 
  geom_point(show.legend = TRUE) +
  facet_grid(.~var_reord, scale="free_x") + 
  ggtitle("O quanto voc? discorda ou concorda com as afirma??es abaixo") + theme_bw() +
  scale_fill_discrete(name="Quest?es",
                      breaks=c("GR3_Eficiente", "GR3_Incentivado_Melhoria", "GR3_Valorizado_Melhoria", "GR3_Incentivado_Estudo", "GR3_Equipe_Sub", "GR3_Ctrl_Adm", "GR3_Pessoas_Quest", "GR3_Atrib_SemValor", "GR3_Autom_Atrib", "GR3_Capaz"),
                      labels=c("Consigo ser eficiente no uso do meu tempo", "Sinto-me incentivado pelo meu gerente a empregar parte do meu tempo para reflex?o e melhoria das minhas atividades", "Sou valorizado por buscar a melhoria das atividades que executo", "Sinto-me incentivado pelo meu gerente a empregar parte do meu tempo para estudo e aprendizagem", "Minha equipe est? subdimensionada para realizar as atividades que dela s?o demandadas", "Pr?ticas e controles administrativos desnecess?rios afetam 100% do uso do meu tempo", "Observo as pessoas questionando as pr?ticas pouco produtivas (Ex: 'Precisamos fazer isso dessa forma?')", "Minhas atribui??es n?o geram valor para a companhia", "Se minha atribui??es fossem automatizadas, geraria maior ganho para a companhia", "Sinto-me capaz de assumir atribui??es que gerariam maior valor para a companhia")) 



#---------------------------------------------------------------------------------
#--- Quest?es do Grupo GR4 - Sobre Boas Pr?ticas que Favorecem a Gest?o do Tempo

#  Meu gestor me d? total autonomia para alocar meu tempo
#  Meu gestor incentiva que eu aloque meu tempo livremente
#  Minha ger?ncia adota pr?ticas que favorecem a produtividade e foco
#  Eu adoto pr?ticas de gest?o de tempo que aumentam minha efici?ncia e foco
#  Frequentemente, minha ger?ncia reserva tempo para refletir sobre melhoria das pr?ticas do dia a dia


vMeltGR4 <-melt(dResultadosPesquisa, measure.vars=c("GR4_Autonomia_Tempo", "GR4_Gestor_Incentiva", "GR4_Ger_Praticas", "GR4_Eu_Praticas", "GR4_Ger_Reserva_Tempo"))

# VERS?O LEGENDA ABREVIADA
vMeltGR4 %>% mutate(var_reord=reorder(variable, value, FUN = median)) %>% 
  ggplot(aes(Respondente, value, fill = variable)) + 
  geom_boxplot() + xlab("") + ylab("(0) N?o Sei  - (1) Discordo Tot.  ==>  (7) Concordo Tot.") + 
  geom_point(show.legend = TRUE) +
  facet_grid(.~var_reord, scale="free_x") + 
  ggtitle("Sobre Boas Pr?ticas que Favorecem a Gest?o do Tempo") + theme_bw() +
  scale_fill_discrete(name="Quest?es",
                      breaks=c("GR4_Autonomia_Tempo", "GR4_Gestor_Incentiva", "GR4_Ger_Praticas", "GR4_Eu_Praticas", "GR4_Ger_Reserva_Tempo"),
                      labels=c("Meu gestor me d? total autonomia...", "Meu gestor incentiva que eu aloque meu tempo...", "Minha ger?ncia adota pr?ticas que favorecem a produtividade...", "Eu adoto pr?ticas de gest?o de tempo...", "...minha ger?ncia reserva tempo para refletir sobre melhoria ...")) 

# VERS?O LEGENDA COMPLETA
vMeltGR4 %>% mutate(var_reord=reorder(variable, value, FUN = median)) %>% 
  ggplot(aes(Respondente, value, fill = variable)) + 
  geom_boxplot() + xlab("") + ylab("(0) N?o Sei  - (1) Discordo Tot.  ==>  (7) Concordo Tot.") + 
  geom_point(show.legend = TRUE) +
  facet_grid(.~var_reord, scale="free_x") + 
  ggtitle("Sobre Boas Pr?ticas que Favorecem a Gest?o do Tempo") + theme_bw() +
  scale_fill_discrete(name="Quest?es",
                      breaks=c("GR4_Autonomia_Tempo", "GR4_Gestor_Incentiva", "GR4_Ger_Praticas", "GR4_Eu_Praticas", "GR4_Ger_Reserva_Tempo"),
                      labels=c("Meu gestor me d? total autonomia para alocar meu tempo", "Meu gestor incentiva que eu aloque meu tempo livremente", "Minha ger?ncia adota pr?ticas que favorecem a produtividade e foco", "Eu adoto pr?ticas de gest?o de tempo que aumentam minha efici?ncia e foco", "Frequentemente, minha ger?ncia reserva tempo para refletir sobre melhoria das pr?ticas do dia a dia")) 



#------------------------------------------------------------------------------
#--- Quest?es do Grupo GR5 - Se eu decidir modificar minha aloca??o de tempo

#  Eu sempre precisaria dar ci?ncia ao meu gerente 
#  Meu gerente sempre seria receptivo ao tomar ci?ncia sobre a mudan?a 
#  Eu seria mais sujeito a ser responsabilizado sobre qualquer falha ou atraso 
#  N?o acho que eu deveria ter autonomia sobre minha aloca??o de tempo


vMeltGR5 <-melt(dResultadosPesquisa, measure.vars=c("GR5_Dar_Ciencia", "GR5_Gestor_Receptivo", "GR5_Responsabilizado", "GR5_N?o_Autonomia"))

vMeltGR5 %>% mutate(var_reord=reorder(variable, value, FUN = median)) %>% 
  ggplot(aes(Respondente, value, fill = variable)) + 
  geom_boxplot() + xlab("") + ylab("(0) N?o Sei  - (1) Discordo Tot.  ==>  (7) Concordo Tot.") + 
  geom_point(show.legend = TRUE) +
  facet_grid(.~var_reord, scale="free_x") + 
  ggtitle("Se eu decidir modificar minha aloca??o de tempo") + theme_bw() +
  scale_fill_discrete(name="Quest?es",
                      breaks=c("GR5_Dar_Ciencia", "GR5_Gestor_Receptivo", "GR5_Responsabilizado", "GR5_N?o_Autonomia"),
                      labels=c("Eu sempre precisaria dar ci?ncia ao meu gerente", "Meu gerente sempre seria receptivo ao tomar ci?ncia sobre a mudan?a", "Eu seria mais sujeito a ser responsabilizado sobre qualquer falha ou atraso", "N?o acho que eu deveria ter autonomia sobre minha aloca??o de tempo")) 


#--------------------------------------------------------------------------------------------
#--- Quest?es do Grupo GR6 - Na sua opini?o, as mudan?as esperadas pela Iniciativa de TD ir?o

#  Impulsionar a Petrobras a ser l?der em desenvolvimento e ado??o de tecnologias digitais na ind?stria em que est? inserida.
#  TD ir? transformar como a Petrobras gera valor para seus p?blicos de interesse
#  Ampliar a conex?o entre as pessoas 
#  Aumentar a seguran?a das atividades da Petrobras
#  Aumentar investimento na forma??o dos profissionais 
#  Tornar a for?a de trabalho mais eficiente
#  Melhorar a qualidade do tempo dedicado ao trabalho 
#  Gerar redu??o de pessoal com sobrecarga dos remanescentes 

vMeltGR6 <-melt(dResultadosPesquisa, measure.vars=c("GR6_Impulsionar_PB", "GR6_Transformar_PB", "GR6_Conexao_Pessoas", "GR6_Seguranca_Atividades", "GR6_Formacao_Pessoal", "GR6_Pessoal_Eficiente", "GR6_Qualidade_Tempo", "GR6_Reducao_Pessoal"))

# VERS?O LEGENDA ABREVIADA
vMeltGR6 %>% mutate(var_reord=reorder(variable, value, FUN = median)) %>% 
  ggplot(aes(Respondente, value, fill = variable)) + 
  geom_boxplot() + xlab("") + ylab("(0) N?o Sei  - (1) Discordo Tot.  ==>  (7) Concordo Tot.") + 
  geom_point(show.legend = TRUE) +
  facet_grid(.~var_reord, scale="free_x") + 
  ggtitle("Na sua opini?o, as mudan?as esperadas pela Iniciativa de TD ir?o") + theme_bw() +
  scale_fill_discrete(name="Quest?es",
                      breaks=c("GR6_Impulsionar_PB", "GR6_Transformar_PB", "GR6_Conexao_Pessoas", "GR6_Seguranca_Atividades", "GR6_Formacao_Pessoal", "GR6_Pessoal_Eficiente", "GR6_Qualidade_Tempo", "GR6_Reducao_Pessoal"),
                      labels=c("Impulsionar a Petrobras a ser l?der em desenvolvimento e ado??o de tecnologias digitais...", "TD ir? transformar como a Petrobras gera valor...", "Ampliar a conex?o entre as pessoas", "Aumentar a seguran?a das atividades da Petrobras", "Aumentar investimento na forma??o dos profissionais", "Tornar a for?a de trabalho mais eficiente", "Melhorar a qualidade do tempo dedicado ao trabalho", "Gerar redu??o de pessoal com sobrecarga dos remanescentes")) 

# VERS?O LEGENDA COMPLETA
vMeltGR6 %>% mutate(var_reord=reorder(variable, value, FUN = median)) %>% 
  ggplot(aes(Respondente, value, fill = variable)) + 
  geom_boxplot() + xlab("") + ylab("(0) N?o Sei  - (1) Discordo Tot.  ==>  (7) Concordo Tot.") + 
  geom_point(show.legend = TRUE) +
  facet_grid(.~var_reord, scale="free_x") + 
  ggtitle("Na sua opini?o, as mudan?as esperadas pela Iniciativa de TD ir?o") + theme_bw() +
  scale_fill_discrete(name="Quest?es",
                      breaks=c("GR6_Impulsionar_PB", "GR6_Transformar_PB", "GR6_Conexao_Pessoas", "GR6_Seguranca_Atividades", "GR6_Formacao_Pessoal", "GR6_Pessoal_Eficiente", "GR6_Qualidade_Tempo", "GR6_Reducao_Pessoal"),
                      labels=c("Impulsionar a Petrobras a ser l?der em desenvolvimento e ado??o de tecnologias digitais na ind?stria em que est? inserida", "TD ir? transformar como a Petrobras gera valor para seus p?blicos de interesse", "Ampliar a conex?o entre as pessoas", "Aumentar a seguran?a das atividades da Petrobras", "Aumentar investimento na forma??o dos profissionais", "Tornar a for?a de trabalho mais eficiente", "Melhorar a qualidade do tempo dedicado ao trabalho", "Gerar redu??o de pessoal com sobrecarga dos remanescentes")) 


#--------------------------------------------------------------------------------------------
#--- Quest?es do Grupo GR7 - Sobre a relev?ncia do tema TD

#  Mudan?as como a TD s?o importantes e urgentes para a Ind?stria de ?leo & G?s (O&G) e Energia
#  A Ind?stria de O&G e Energia procrastinou dar aten??o ? TD em compara??o a outras ind?strias
#  A Petrobras procrastinou dar aten??o a TD em compara??o a outras empresas da ind?stria de O&G e Energia
#  TD n?o ? t?o disruptivo na ind?stria na qual a Petrobras atua 
#  ? oportuno come?ar essa transforma??o nesse momento da companhia

                 
vMeltGR7 <-melt(dResultadosPesquisa, measure.vars=c("GR7_TD_Para_OG", "GR7_Atraso_OG", "GR7_Atraso_PB", "GR7_TD_NaoDirsuptiva", "GR7_Oportuno"))


# VERS?O LEGENDA ABREVIADA
vMeltGR7 %>% mutate(var_reord=reorder(variable, value, FUN = median)) %>% 
  ggplot(aes(Respondente, value, fill = variable)) + 
  geom_boxplot() + xlab("") + ylab("(0) N?o Sei  - (1) Discordo Tot.  ==>  (7) Concordo Tot.") + 
  geom_point(show.legend = TRUE) +
  facet_grid(.~var_reord, scale="free_x") + 
  ggtitle("Sobre a relev?ncia do tema TD") + theme_bw() +
  scale_fill_discrete(name="Quest?es",
                      breaks=c("GR7_TD_Para_OG", "GR7_Atraso_OG", "GR7_Atraso_PB", "GR7_TD_NaoDirsuptiva", "GR7_Oportuno"),
                      labels=c("Mudan?as como a TD s?o importantes...", "A Ind?stria de O&G e Energia procrastinou ...", "A Petrobras procrastinou dar aten??o a TD ...", "TD n?o ? t?o disruptivo na ind?stria na qual a Petrobras atua", "? oportuno come?ar essa transforma??o nesse momento ...")) 

# VERS?O LEGENDA COMPLETA
vMeltGR7 %>% mutate(var_reord=reorder(variable, value, FUN = median)) %>% 
  ggplot(aes(Respondente, value, fill = variable)) + 
  geom_boxplot() + xlab("") + ylab("(0) N?o Sei  - (1) Discordo Tot.  ==>  (7) Concordo Tot.") + 
  geom_point(show.legend = TRUE) +
  facet_grid(.~var_reord, scale="free_x") + 
  ggtitle("Sobre a relev?ncia do tema TD") + theme_bw() +
  scale_fill_discrete(name="Quest?es",
                      breaks=c("GR7_TD_Para_OG", "GR7_Atraso_OG", "GR7_Atraso_PB", "GR7_TD_NaoDirsuptiva", "GR7_Oportuno"),
                      labels=c("Mudan?as como a TD s?o importantes e urgentes para a Ind?stria de ?leo & G?s (O&G) e Energia", "A Ind?stria de O&G e Energia procrastinou dar aten??o ? TD em compara??o a outras ind?strias", "A Petrobras procrastinou dar aten??o a TD em compara??o a outras empresas da ind?stria de O&G e Energia", "TD n?o ? t?o disruptivo na ind?stria na qual a Petrobras atua", "? oportuno come?ar essa transforma??o nesse momento da companhia")) 


#--------------------------------------------------------------------------------------------
#--- Quest?es do Grupo GR8 - Sobre o interesse no tema TD

#  Sinto-me interessado em engajar nas a??es relacionadas ? TD
#  Somente me engajaria em a??es relacionadas ? TD se fosse formalmente requisitado
#  As pessoas do meu conv?vio na empresa se interessam em engajar nas a??es relacionadas ? TD
#  As pessoas n?o querem mudar suas atitudes, h?bitos e convic??es
#  As pessoas t?m interesse suficiente para se desenvolver no tema de forma aut?noma


vMeltGR8 <-melt(dResultadosPesquisa, measure.vars=c("GR8_Interessado", "GR8_Engajamento_Formal", "GR8_Pessoas_interessadas", "GR8_Nao_Mudar", "GR8_Pessoas_TD_Autonoma"))


# VERS?O LEGENDA ABREVIADA
vMeltGR8 %>% mutate(var_reord=reorder(variable, value, FUN = median)) %>% 
  ggplot(aes(Respondente, value, fill = variable)) + 
  geom_boxplot() + xlab("") + ylab("(0) N?o Sei  - (1) Discordo Tot.  ==>  (7) Concordo Tot.") + 
  geom_point(show.legend = TRUE) +
  facet_grid(.~var_reord, scale="free_x") + 
  ggtitle("Sobre o interesse no tema TD") + theme_bw() +
  scale_fill_discrete(name="Quest?es",
                      breaks=c("GR8_Interessado", "GR8_Engajamento_Formal", "GR8_Pessoas_interessadas", "GR8_Nao_Mudar", "GR8_Pessoas_TD_Autonoma"),
                      labels=c("Sinto-me interessado em engajar...", "Somente me engajaria em a??es relacionadas ? TD ....", "As pessoas do meu conv?vio na empresa se interessam ...", "As pessoas n?o querem mudar ...", "As pessoas t?m interesse suficiente para se desenvolver...")) 

# VERS?O LEGENDA COMPLETA
vMeltGR8 %>% mutate(var_reord=reorder(variable, value, FUN = median)) %>% 
  ggplot(aes(Respondente, value, fill = variable)) + 
  geom_boxplot() + xlab("") + ylab("(0) N?o Sei  - (1) Discordo Tot.  ==>  (7) Concordo Tot.") + 
  geom_point(show.legend = TRUE) +
  facet_grid(.~var_reord, scale="free_x") + 
  ggtitle("Sobre o interesse no tema TD") + theme_bw() +
  scale_fill_discrete(name="Quest?es",
                      breaks=c("GR8_Interessado", "GR8_Engajamento_Formal", "GR8_Pessoas_interessadas", "GR8_Nao_Mudar", "GR8_Pessoas_TD_Autonoma"),
                      labels=c("Sinto-me interessado em engajar nas a??es relacionadas ? TD", "Somente me engajaria em a??es relacionadas ? TD se fosse formalmente requisitado", "As pessoas do meu conv?vio na empresa se interessam em engajar nas a??es relacionadas ? TD", "As pessoas n?o querem mudar suas atitudes, h?bitos e convic??es", "As pessoas t?m interesse suficiente para se desenvolver no tema de forma aut?noma")) 

