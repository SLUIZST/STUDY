# --- Caret Package

library(dslabs)
library(caret)
library(tidyverse)

# Comprehension Check: Caret Package

# Q1
# In the exercise in Q6 from Comprehension Check: Trees and Random Forests, we saw that 
# changing nodesize to 50 and setting maxnodes to 25 yielded smoother results. Let's use 
# the train function to help us pick what the values of nodesize and maxnodes should be.

# From the caret description of methods, we see that we can't tune the maxnodes parameter 
# or the nodesize argument with randomForests. So we will use the __Rborist__ package and 
# tune the minNode argument. Use the train function to try values 
# minNode <- seq(25, 100, 25). Set the seed to 1.

# Which value minimizes the estimated RMSE?


?randomForest

library(rpart)
n <- 1000
sigma <- 0.25
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)

# Q6 previous assets
library(randomForest)
fit <- randomForest(y ~ x, data = dat, nodesize = 50, maxnodes = 25)   #BLANK
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = 2)

fit <- train(y ~ ., method = "Rborist",
             tuneGrid = data.frame(predFixed=1, minNode = seq(25, 100, 25)),
             data = dat)

fit

# Explanation
# The following code can be used to determine that 50 minimizes the estimated RMSE:
  
set.seed(1)
library(caret)
fit <- train(y ~ ., method = "Rborist",   
             tuneGrid = data.frame(predFixed = 1, 
                                   minNode = seq(25, 100, 25)),
             data = dat)
ggplot(fit)

