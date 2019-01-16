# --- Comprehension Check: Logistic Regression
library(caret)
library(tidyverse)

# Q1
# Define a dataset using the following code: 

set.seed(2)
make_data <- function(n = 1000, p = 0.5, 
                      mu_0 = 0, mu_1 = 2, 
                      sigma_0 = 1,  sigma_1 = 1){
  
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}
dat <- make_data()

# Note that we have defined a variable x that is predictive of a binary outcome y: 
dat$train %>% ggplot(aes(x, color = y)) + geom_density()

# Generate 25 different datasets changing the difference between the two classes using 
# delta <- seq(0, 3, len=25) and plot accuracy vs mu_1.

# Which is the correct plot?


# TIP
# Compute accuracy by fitting logistic regression y~x using train dataset and 
# predict.glm on test dataset (be sure to set type='response'). Probabilities will 
# be returned. Lets say you call this vector p_y. Code y_hat <- ifelse(p_y>=0.5, 1,0). 
# The rest follows.

dat$train %>% glm(y ~ x, data = .) %>% tidy()


class(dat$train$x)
class(dat$train$y)

fit <- lm(y ~ x, data = dat$train) 

# Accuracy of the first case
#fit <- lm(y ~ x, data = dat$train)
#y_hat <- predict(fit, dat$test)
#mean(y_hat == dat$test$y)

# Trying to discover the accuracy via best cutoff
cutoff <- seq(0, 3, by=0.2)

accuracy <- map_dbl(cutoff, function(x1){
  y_hat <- ifelse(dat$train$x > x1, 1, 0) %>%
    factor(levels = levels(dat$test$y))   
  mean(y_hat == dat$test$y)
})

max(accuracy)

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

min(dat$train$x)
max(dat$train$x)




# --- using delta

set.seed(2)
make_data <- function(n = 1000, p = 0.5, 
                      mu_0 = 0, mu_1 = mu_0 + d, 
                      sigma_0 = 1,  sigma_1 = 1){
  
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}
dat <- make_data()









# --------------------- TEST

y <- rbinom(1000, 1, 0.5)
f_0 <- rnorm(1000, 0, 1)
f_1 <- rnorm(1000, 2, 1)
x <- ifelse(y == 1, f_1, f_0)

test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

#l <- list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
#         test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))

l <- list(train = data.frame(x = x, y = y) %>% slice(-test_index),
          test = data.frame(x = x, y = y) %>% slice(test_index))

fit <- lm(y ~ x, data = l$train)
y_hat <- predict(fit, l$test) 
mean(y_hat == l$test$y)

confusionMatrix(data = x_hat, reference = x)

class(x_hat)
class(l$test$x)
