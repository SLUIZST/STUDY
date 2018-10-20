# Testar simulacao de respostas ao prototipo 2 da pesquisa

item_likert <- seq(1:7)
item_likert

# Grupo de Perguntas Q01 = Observo que a lideranca na minha ger?ncia ...
# Simulando a resposta da pergunta Q01_01 = exercida com base na autoridade (comando e controle)

# Tamanho da Amostra
N <- 1000

resp_Q01_01 <- sample(item_likert, N, replace = TRUE, prob=c(0.1, 0.15, 0.1, 0.05, 0.2, 0.3, 0.1))
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

# Simulacao de Monte Carlo

B <- 1000
mc_events <- replicate(B, {
     resp_Q01_01 <- sample(item_likert, N, replace = TRUE, prob=c(0.1, 0.15, 0.1, 0.05, 0.2, 0.3, 0.1))
     mean(resp_Q01_01)
     })

# Verificando se a distribuicaoo das probabilidades e aproximadamente normal
#mc_events
hist(mc_events)

# The Central Limit Theorem -- or the CLT for short tells us that when the number 
# of independent draws-- also call the sample size-- is large, the probability 
# distribution of the sum of these draws is approximately normal.

mean(mc_events)
sd(mc_events)


# Simulando boxplots
N <- 250
resp_Q01_01 <- sample(item_likert, N, replace = TRUE, prob=c(0.1, 0.15, 0.1, 0.05, 0.2, 0.3, 0.1))
resp_Q01_02 <- sample(item_likert, N, replace = TRUE, prob=c(0.1, 0.05, 0.1, 0.15, 0.3, 0.2, 0.1))


resp_df <- data.frame("Pergunta" = "Q1", "Nota" = resp_Q01_01, stringsAsFactors = FALSE)
resp_df

rbind(resp_df, "Q2", resp_Q01_02)
resp_df






