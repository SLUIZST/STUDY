# Trees and Random Forests


library(dslabs)
library(caret)
library(tidyverse)

# -- Comprehension Check: Trees and Random Forests

# https://courses.edx.org/courses/course-v1:HarvardX+PH125.8x+2T2018/courseware/3f70797a568946e195863415b94d25fd/78cfd6af400d452c9aa9f8070403e55c/4?activate_block_id=block-v1%3AHarvardX%2BPH125.8x%2B2T2018%2Btype%40video%2Bblock%4010b8485b1ac0425e979adc450c93e421

# Q1
# Create a simple dataset where the outcome grows 0.75 units on average for every increase in a predictor, 
# using this code:
  
library(rpart)
n <- 1000
sigma <- 0.25
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)

# Which code correctly uses rpart to fit a regression tree and saves the result to fit?

fit <- rpart(y ~ ., data = dat)


# Q2
# Which of the following plots correctly shows the final tree obtained in Q1?

text(fit, cex=0.75)

# Explanation
# The plot can be made using the following code:
  
plot(fit)
text(fit)

# Q3
# Below is most of the code to make a scatter plot of y versus x along with the predicted values based on the fit.

  dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  #BLANK
    geom_step(aes(x, y_hat), col=2)  
  
#  Which line of code should be used to replace #BLANK in the code above?
    

# Q4
# Now run Random Forests instead of a regression tree using randomForest from the __randomForest__ package, 
# and remake the scatterplot with the prediction line. Part of the code is provided for you below.
  
library(randomForest)
fit <- randomForest(y ~ x, data = dat) 

dat %>% 
    mutate(y_hat = predict(fit)) %>% 
    ggplot() +
    geom_point(aes(x, y)) +
    geom_step(aes(x, y_hat), col = 2)
  
  
# Q5
# Use the plot function to see if the Random Forest from Q4 has converged or if we need more trees.
# Which is the correct plot to assess whether the Random Forest has converged?

plot(fit)  
  
# Explanation
plot(fit) # will allow you to make the plot.  


# Q6
# It seems that the default values for the Random Forest result in an estimate that is too flexible (unsmooth). 
# Re-run the Random Forest but this time with a node size of 50 and a maximum of 25 nodes. Remake the plot.
# Part of the code is provided for you below.

library(randomForest)
fit <- randomForest(y ~ x, data = dat, nodesize = 50, maxnodes = 25)   #BLANK
  dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = 2)

# What code should replace #BLANK in the provided code?

? randomForest

# Explanation
# We see that using randomForest(y ~ x, data = dat, nodesize = 50, maxnodes = 25) yields smoother results. 
# We'll pick up with this exercise after we learn more about the caret package
  


