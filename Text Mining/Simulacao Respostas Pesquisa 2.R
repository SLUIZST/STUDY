# Testar simulação de respostas ao protótipo 2 da pesquisa

item_likert <- seq(1:7)
item_likert

# Grupo de Perguntas Q01 = Observo que a liderança na minha gerência ...
# Simulando a resposta á pergunta Q01_01 = é exercida com base na autoridade (comando e controle)

# Tamanho da Amostra
N <- 1000

resp_Q01_01 <- sample(item_likert, N, replace = TRUE)
# resp_Q01_01

avg <- mean(resp_Q01_01)
sdv <- sd(resp_Q01_01)

avg
sdv

# teste com histograma
hist(resp_Q01_01)


# Teste com quartis
quartis <- seq(0.05, 0.95, 0.05)

observed_quantiles <- quantile(resp_Q01_01, quartis)
quartis_teoricos <- qnorm(quartis, avg, sdv)

plot(quartis_teoricos, observed_quantiles)
abline(0,1)

# Simulação de Monte Carlo

B <- 1000
mc_events <- replicate(B, {
     resp_Q01_01 <- sample(item_likert, N, replace = TRUE)
     mean(resp_Q01_01)
     })

# Verificando se a distribuição das probabilidades e aproximadamente normal
#mc_events
hist(mc_events)

# The Central Limit Theorem -- or the CLT for short tells us that when the number 
# of independent draws-- also call the sample size-- is large, the probability 
# distribution of the sum of these draws is approximately normal.

mean(mc_events)
sd(mc_events)




