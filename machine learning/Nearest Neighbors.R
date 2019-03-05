# ===== Nearest Neighbors


# Comprehension Check: Nearest Neighbors

# Q1
# Previously, we used logistic regression to predict sex based on height. Now we are going to use knn to do the same. 
# Use the code described in these videos to select the F_1 measure and plot it against k. Compare to the F_1 of about 
# 0.6 we obtained with regression. Set the seed to 1.

# What is the max value of F_1?
# At what value of k does the max occur?

library(caret)
library(dslabs)
library(tidyverse)

data(heights)

y <- heights$sex
x <- heights$height

set.seed(1)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]

# As in F_1 score
cutoff <- seq(61, 70)
F_1 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(train_set$sex))
})

plot(cutoff, F_1)

# Using Logistic Regression

lm_fit <- mutate(train_set, y = as.numeric(sex == "Female")) %>%
          lm(y ~ height, data = .)
#p_hat <- predict(lm_fit, test_set)
#y_hat <- ifelse(p_hat > 0.6, "Female", "Male") %>% factor()
y_hat <- predict(lm_fit, train_set) %>% factor()
F_meas(data = y_hat, reference = factor(train_set$sex))


set.seed(1)
ks <- seq(1, 101, 1)

accuracy <- map_df(ks, function(k){
  fit <- knn3(sex ~ height, data = train_set, k = k)
  
  y_hat <- predict(fit, train_set, type = "class")
  
  F_1 <- F_meas(data = y_hat, reference = factor(train_set$sex))
  
  list(k = k, F_1 = F_1)
})

accuracy %>% ggplot(aes(k,F_1)) + geom_line()

max_F_1 <- max(accuracy$F_1)
max_F_1

max_k <- accuracy$k[which.max(max_F_1)]
max_k

# Q2
# Next we will use the same gene expression example used in the Comprehension Check: Distance exercises. You can 
# load it like this:
  
library(dslabs)
data("tissue_gene_expression")

# Split the data into training and test sets, and report the accuracy you obtain. 
# Try it for k = 1, 3, 5, 7, 9, 11. Set the seed to 1.

y <- tissue_gene_expression$y
x <- tissue_gene_expression$x

set.seed(1)
index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

train_set_x = x[-index,] 
test_set_x = x[index,] 
train_set_y = y[-index] 
test_set_y = y[index] 

ks <- seq(1, 11, 2)

accur <- map_df(ks, function(k){
  set.seed(1)
  fit <- knn3(train_set_x, train_set_y, k = k)
  
  y_hat <- predict(fit, test_set_x, type = "class") %>% factor(levels = levels(test_set_y))
  
  cm <- confusionMatrix(data = y_hat, reference = test_set_y)
  acc <- cm$overall["Accuracy"]

  list(k = k, acc = acc)
})


# TESTE -> merging
train_set = (list('x' = train_set_x, 'y' = train_set_y))
test_set = (list('x' = test_set_x, 'y' = test_set_y))

accuracy2 <- sapply(ks,function(k){ 
    knn_fitq2 <- knn3(y ~ x, data=train_set, k=k)

    y_hat <- predict(knn_fitq2, test_set,type="class") %>% factor(levels = levels(test_set$y))
    
    cm <- confusionMatrix(data = y_hat, reference = test_set$y)
    acc <- cm$overall["Accuracy"]
    
    list(k = k, acc = acc)
})

# TESTING 2
set.seed(1)
ks <- seq(1, 11, 2)

accur <- map_df(ks, function(k){
  #set.seed(1)
  index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  train_set_x = x[-index,] 
  test_set_x = x[index,] 
  train_set_y = y[-index] 
  test_set_y = y[index] 
  
  fit <- knn3(train_set_x, train_set_y, k = k)
  
  y_hat <- predict(fit, test_set_x, type = "class") %>% factor(levels = levels(test_set_y))
  
  cm <- confusionMatrix(data = y_hat, reference = test_set_y)
  acc <- cm$overall["Accuracy"]
  
  list(k = k, acc = acc)
})

# TESTING 3

accur <- map_df(ks, function(k){
  #set.seed(1)
  fit <- knn3(train_set_x, train_set_y, k = k)
  
  y_hat <- predict(fit, train_set_x, type = "class") %>% factor(levels = levels(train_set_y))
  
  cm <- confusionMatrix(data = y_hat, reference = train_set_y)
  acc <- cm$overall["Accuracy"]
  
  list(k = k, acc = acc)
})

# => OK for k = 5 and K = 9





# https://courses.edx.org/courses/course-v1:HarvardX+PH125.8x+2T2018/courseware/7c50a2fc6d44433fa902be8ed097f301/2b3a4f4bd3324c45aa8395f5c3bbfd1d/5?activate_block_id=block-v1%3AHarvardX%2BPH125.8x%2B2T2018%2Btype%40problem%2Bblock%40d33afa6058b8458b9a88b524b889eeb3

