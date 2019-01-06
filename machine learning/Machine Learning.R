library(tidyverse)

# ---------------------------------------------------------------------------
# Introduction to Machine Learning   
# ---------------------------------------------------------------------------

# Notation
# An Example

# ---------------------------------------------------------------------------
# Machine Learning Basics - Basics of Evaluating Machine Learning Algorithms  
# ---------------------------------------------------------------------------

# -- Caret package, training and test sets, and overall accuracy


# first example, we will use the heights data set in the ds labs package
library(dslabs)
data(heights)

# We start by defining the outcome and predictors. In this example, we have only one predictor.
y <- heights$sex
x <- heights$height

library(caret)
set.seed(2)
test_index <- createDataPartition(y,  times = 1, p = 0.5, list = FALSE)

# We can use this index to define the training set and test set 

train_set <- heights[-test_index,]
test_set <- heights[test_index,]

# develop an algorithm using only the training set. Let's start by developing the simplest  
# possible machine learning algorithm-- guessing the outcome

y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE)

# In machine learning applications, it is useful to use factors to represent the 
# categorical outcomes.

y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE) %>% 
         factor(levels = levels(test_set$sex))

# The overall accuracy is simply defined as the overall proportion that is 
# predicted correctly

mean(y_hat == test_set$sex)

# Not surprisingly, our accuracy is about 50%-- we're guessing.Now, can we do better?
# Exploratory data as it suggests we can because on average, males are slightly taller 
# than females.

heights %>% group_by(sex) %>% summarize(mean(height), sd(height))

# But how do we make use of this insight? Let's try a simple approach.

y_hat <- ifelse(x > 62, "Male", "Female") %>% factor(levels = levels(test_set$sex))

mean(y == y_hat)

# we use the cutoff of 62 inches, but we can examine the accuracy obtained for other 
# cutoffs and then take the value that provides the best result

# it is important that we pick the best value on the training set. The test set is 
# only for evaluation

# We examined the accuracy we obtain with 10 different cutoffs and pick the one 
# yielding the best result

library(tidyverse)

cutoff <- seq(61,70)

accuracy <- map_dbl(cutoff, function(x){
          y_hat <- ifelse(train_set$height > x, "Male", "Female") %>%
          factor(levels = levels(test_set$sex))   
          mean(y_hat == train_set$sex)
})

max(accuracy)

plot(cutoff, accuracy)

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

# Now, we can test this cut off on our test set to make sure accuracy is
# not overly optimistic.

y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>%
         factor(levels = levels(test_set$sex))   

y_hat <- factor(y_hat)

mean(y_hat == test_set$sex)

# We see that is a bit lower than the accuracy observed on the training set, 
# but it's still better than guessing.
# And by testing on a data that we did not train on, we know it is not due to overfitting.


# ----------------- Comprehension Check: Basics of Evaluating Machine Learning Algorithms

library(dslabs)

mnist <- read_mnist()

str(mnist)

y <- mnist$train$labels

y[5] + y[6]
y[5] > y[6]



# -------------- Confusion Matrix

# Generally speaking, overall accuracy can be a deceptive measure
# To see this, we'll start by constructing what is referred to as the confusion matrix

table(predicted=y_hat, actual = test_set$sex)

# If we compute the accuracy separately for each sex, we get the following
test_set %>% mutate(y_hat = y_hat) %>% group_by(sex) %>% 
            summarize(accuracy = mean(y_hat == sex))

# In fact, we're calling close to half females males.This is because of the prevalence. 
# There are more males in the data sets than females 

prev <- mean (y == "Male")
prev

# So when computing overall accuracy, the high percentage of mistakes made for females is 
# outweighted by the gains in correct calls for men.

# A general improvement to using over accuracy is to study sensitivity and specificity 
# separately. To define sensitivity and specificity, we need a binary outcome

library(e1071)
confusionMatrix(data = y_hat, reference = test_set$sex)


#-------------- Balanced accuracy and F1 score

# The F_meas function in the cara package computes the summary with beta defaulting 
# to one.
# So let's rebuild our prediction algorithm, but this time maximizing the F score 
# instead of overall accuracy.

library(tidyverse)

cutoff <- seq(61,70)

F_1 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>%
    factor(levels = levels(test_set$sex))   
  F_meas(data = y_hat, reference = factor(train_set$sex))
})

plot(cutoff, F_1)

max(F_1)

best_cutoff <- cutoff[which.max(F_1)]
best_cutoff

# A cutoff of 66 inches makes much more sense than 64
# Furthermore, it balances the specificity and sensitivity of our confusion matrix 
# as seen here.
y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>%
  factor(levels = levels(test_set$sex))   

confusionMatrix(data = y_hat, reference = test_set$sex)



# ----- Prevalence matters in practice




# ---- Comprehension Check: Confusion Matrix
library(dslabs)
library(dplyr)
library(lubridate)

data("reported_heights")

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

# Q1
# What is the propotion of females in class and online? (That is, calculate the 
# proportion of the in class students who are female and the proportion of the online 
# students who are female.)

dat %>% group_by(type) %>% summarize(prop_female = mean(sex == "Female"))

# Q2
# If you used the type variable to predict sex, what would the prediction accuracy be?


# --- If I used train and test set.....

library(caret)
set.seed(2)
test_index <- createDataPartition(y,  times = 1, p = 0.5, list = FALSE)

# We can use this index to define the training set and test set 

train_set <- dat[-test_index,]
test_set <- dat[test_index,]

# --- But I won't. Let's simulate the prediction using the type

y_hat <- ifelse(x == "inclass", "Female", "Male") %>%
         factor(levels = levels(y))   

mean(y_hat == y)

# ANSWER
y_hat <- ifelse(x == "online", "Male", "Female") %>% 
  factor(levels = levels(y))
mean(y_hat==y)


# Q3
# Write a line of code using the table function to show the confusion matrix, assuming 
# the prediction is y_hat and the truth is y.
table(prediction = y_hat, actual = y)

# Q4
# What is the sensitivity of this prediction?

confusionMatrix(data = y_hat, reference = y)

# ----- Comprehension Check: Practice with Machine Learning
library(tidyverse)

# We will practice building a machine learning algorithm using a new dataset, iris, 
# that provides multiple predictors for us to use to train. To start, we will remove 
# the setosa species and we will focus on the versicolor and virginica iris species 
# using the following code:
  
library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

# Q1
# First let us create an even split of the data into train and test partitions using 
# createDataPartition. The code with a missing line is given below:

set.seed(2)
# line of code
test_index <- createDataPartition(y,  times = 1, p = 0.5, list = FALSE)

test <- iris[test_index,]
train <- iris[-test_index,]

# Answer
# Correct: Good choice! The createDataPartition function has a number of parameters that 
# allow the user to specify a test/training partition by the percentage of data that goes
# to training. See the associated help file.

# Q2
# Next we will figure out the singular feature in the dataset that yields the greatest 
# overall accuracy. You can use the code from the introduction and from Q1 to start your
# analysis.
# Using only the train iris data set, which of the following is the singular feature for 
# which a smart cutoff (simple search) yields the greatest overall accuracy?

train %>% group_by(Species) %>% summarize(mean(Sepal.Length), sd(Sepal.Length))
train %>% group_by(Species) %>% summarize(mean(Sepal.Width), sd(Sepal.Width))
train %>% group_by(Species) %>% summarize(mean(Petal.Length), sd(Petal.Length))
train %>% group_by(Species) %>% summarize(mean(Petal.Width), sd(Petal.Width))

# Petal.Length has the greater mean difference between the two Species and the greater 
# standard deviation difference


# Explanation
# This sample code can be used to determine that Petal.Length is the most accurate singular
# feature.

foo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==train$Species)
  })
}
predictions <- apply(train[,-5],2,foo)
sapply(predictions,max)	

# Q3
# Using the smart cutoff value calculated on the training data, what is the overall 
# accuracy in the test data?


cutoff <- seq(4.0, 6.0, by = 0.1)

F_1 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>%
    factor(levels = levels(test$Species))   
  
  y_hat <- factor(test$Species)
  
  F_meas(data = y_hat, reference = factor(test$Species))
})

# Checking  "Error in F_meas.default(data = y_hat, reference = factor(y)) : 
#           input data must have the same two levels 
length(levels(factor(test$Species)))
length(levels(y_hat))
length(levels(y))

plot(cutoff, F_1)

max(F_1)

best_cutoff <- cutoff[which.max(F_1)]
best_cutoff

# chutando o best cutoff
y_hat <- ifelse(train$Petal.Length > 4.8, "virginica", "versicolor") %>%
  factor(levels = levels(test$Species))  

mean(y_hat == train$Species)  

confusionMatrix(data = y_hat, reference = test$Species)

# Checking in the test data
y_hat <- ifelse(test$Petal.Length > 4.8, "virginica", "versicolor") %>%
  factor(levels = levels(test$Species))  

mean(y_hat == test$Species)  

confusionMatrix(data = y_hat, reference = test$Species)

# Explanation
# The code below can be used to calculate the overall accuracy:
  
predictions <- foo(train[,3])
rangedValues <- seq(range(train[,3])[1],range(train[,3])[2],by=0.1)
cutoffs <-rangedValues[which(predictions==max(predictions))]

y_hat <- ifelse(test[,3]>cutoffs[1],'virginica','versicolor')
mean(y_hat==test$Species)

# ----------- Q4
# Notice that we had an overall accuracy greater than 96% in the training data, but 
# the overall accuracy was lower in the test data. This can happen often if we overtrain. 
# In fact, it could be the case that a single feature is not the best choice. For example,
# a combination of features might be optimal. Using a single feature and optimizing the 
# cutoff as we did on our training data can lead to overfitting.

# Given that we know the test data, we can treat it like we did our training data to see 
# if the same feature with a different cutoff will optimize our predictions.

# Which feature best optimizes our overall accuracy?

# using  the explannation of the Q2

foo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==test$Species)
  })
}
predictions <- apply(test[,-5],2,foo)
sapply(predictions,max)	

# ==>  Petal.Width

# comparing mean e sd of the predictors
test %>% group_by(Species) %>% summarize(mean(Sepal.Length), sd(Sepal.Length))
test %>% group_by(Species) %>% summarize(mean(Sepal.Width), sd(Sepal.Width))
test %>% group_by(Species) %>% summarize(mean(Petal.Length), sd(Petal.Length))
test %>% group_by(Species) %>% summarize(mean(Petal.Width), sd(Petal.Width))

# Q5
# Now we will perform some exploratory data analysis on the data.
# Notice that Petal.Length and Petal.Width in combination could potentially be more 
# information than either feature alone.

# Optimize the combination of the cutoffs for Petal.Length and Petal.Width in the train 
# data and report the overall accuracy when applied to the test dataset. For simplicity, 
# create a rule that if either the length OR the width is greater than the length cutoff 
# or the width cutoff then virginica or versicolor is called. (Note, the F1 will be 
# similarly high in this example.)

# What is the overall accuracy for the test data now?

# cutoffs for Petal.Length
predictions_1 <- foo(train[,3])
rangedValues_1 <- seq(range(train[,3])[1],range(train[,3])[2],by=0.1)
cutoffs_1 <-rangedValues_1[which(predictions_1==max(predictions_1))]

# cutoffs for Petal.Width
predictions_2 <- foo(train[,4])
rangedValues_2 <- seq(range(train[,4])[1],range(train[,4])[2],by=0.1)
cutoffs_2 <-rangedValues_2[which(predictions_2==max(predictions_2))]

y_hat <- ifelse(test[,3]>cutoffs_1[1],'virginica','versicolor')

y_hat <- ifelse(test[,4]>cutoffs_2[1],'virginica','versicolor')

y_hat <- ifelse(test[,3]>cutoffs_1[1] & test[,4]>cutoffs_2[1],'virginica','versicolor')

mean(y_hat==test$Species)

# Explanation
# The following code can be used to calculate this overall accuracy:
  
library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

plot(iris,pch=21,bg=iris$Species)

set.seed(2)
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

petalLengthRange <- seq(range(train[,3])[1],range(train[,3])[2],by=0.1)
petalWidthRange <- seq(range(train[,4])[1],range(train[,4])[2],by=0.1)
cutoffs <- expand.grid(petalLengthRange,petalWidthRange)

id <- sapply(seq(nrow(cutoffs)),function(i){
  y_hat <- ifelse(train[,3]>cutoffs[i,1] | train[,4]>cutoffs[i,2],'virginica','versicolor')
  mean(y_hat==train$Species)
}) %>% which.max

optimalCutoff <- cutoffs[id,] %>% as.numeric
y_hat <- ifelse(test[,3]>optimalCutoff[1] & test[,4]>optimalCutoff[2],'virginica','versicolor')
mean(y_hat==test$Species)


# ========== Comprehension Check: Conditional Probabilities Review

set.seed(1)
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

# Q2
# What is the probability that a test is positive?
mean(test)

# Q3
# What is the probability that an individual has the disease if the test is negative?
mean(disease[test==0])

# Q4
# What is the probability that you have the disease if the test is positive?
#  Remember: calculate the conditional probability the disease is positive assuming a 
#  positive test.
mean(disease[test==1])

# Explanation
# The probability of having the disease given a positive test can be calculated using 
# mean(disease[test==1]==1)
mean(disease[test==1]==1)

# Q5
# If the test is positive, what is the relative risk of having the disease?
# First calculate the probability of having the disease given a positive test, then 
# normalize it against the disease prevalence.  

# Explanation
# The relative risk can be calculated using mean(disease[test==1]==1)/mean(disease==1)
mean(disease[test==1]==1)/mean(disease==1)

#--- Comprehension Check: Conditional Probabilities Practice

# --- Q1
# We are now going to write code to compute conditional probabilities for being male in 
# the heights dataset. Round the heights to the closest inch. Plot the estimated 
# conditional probability  for each .

# Part of the code is provided here:
  
library(dslabs)
data("heights")
#MISSING CODE
heights %>% 
  mutate(height = round(height)) %>%
  group_by(height) %>%
  summarize(p = mean(sex == "Male")) %>%
qplot(height, p, data =.)

# -- Q2
# In the plot we just made in Q1 we see high variability for low values of height. 
# This is because we have few data points. This time use the quantile (\ 0.1,0.2,\dots,0.9\)
# and the cut function to assure each group has the same number of points. Note that for 
# any numeric vector x, you can create groups based on quantiles like this: 
# cut(x, quantile(x, seq(0, 1, 0.1)), include.lowest = TRUE).

# Part of the code is provided here:
  
ps <- seq(0, 1, 0.1)
heights %>% 
#MISSING CODE
mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>%  
group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)

# -- Q3
# You can generate data from a bivariate normal distrubution using the MASS package using 
# the following code.

Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

#And make a quick plot using plot(dat).

# Using an approach similar to that used in the previous exercise, let's estimate the 
# conditional expectations and make a plot. Part of the code has been provided for you:

ps <- seq(0, 1, 0.1)
dat %>% 
#MISSING CODE	
  mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(y = mean(y), x = mean(x)) %>%
	qplot(x, y, data =.)

# ========= Linear Regression for Prediction   

library(HistData)

# --- Comprehension Check: Linear Regression

# Q1
# Create a data set using the following code:
  
set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

# Use the caret package to partition the dataset into test and training sets of equal size. 
# Train a linear model and calculate the RMSE. Repeat this exercise 100 times and report the
# mean and standard deviation of the RMSEs. (Hint: You can use the code shown in a previous 
# course inside a call to replicate using a seed of 1.

# -- Testing RMSE
#actual <- c(1.1, 1.9, 3.0, 4.4, 5.0, 5.6)
#predicted <- c(0.9, 1.8, 2.5, 4.5, 5.0, 6.2)
#rmse(actual, predicted)

library(caret)
#set.seed(2)
#y <- dat$y
#test_index <- createDataPartition(y,  times = 1, p = 0.50, list = FALSE)

#train_set <- dat %>% slice(-test_index)
#test_set  <- dat %>% slice(test_index)

# another way
#train_set <- dat[-test_index,]
#test_set  <- dat[test_index,]

#avg <- mean(train_set$x)
#mean((avg - train_set$x)^2)

#library(ModelMetrics)
#rmse(test_set$x, train_set$x)

#length(train_set$x)
#length(test_set$x)

set.seed(1)
N <- 100
rmse_vet <- replicate(N, {
  y <- dat$y
  test_index <- createDataPartition(y,  times = 1, p = 0.50, list = FALSE)
  
  train_set <- dat %>% slice(-test_index)
  test_set  <- dat %>% slice(test_index)

  fit <- lm(y ~ x, data = train_set)
  fit$coef
  
  y_hat <- predict(fit, test_set)
  
  #When calculating the RMSE, take the square root of the average of the squared differences. 
  #RMSE<-sqrt(mean((y_hat - test$y)^2))
  #rmse(y_hat, train_set$x)  
  
  sqrt(mean((y_hat - test_set$y)^2))
})

#x <- !is.nan(rmse_vet)
#rmse_vet2 <- rmse_vet[x]

#x <- !is.infinite(rmse_vet2)
#rmse_vet3 <- rmse_vet2[x]


mean(rmse_vet)
sd(rmse_vet)

# Q2
# Now we will repeat the above but using larger datasets. Repeat the previous exercise but 
# for datasets with n <- c(100, 500, 1000, 5000, 10000). Save the average and standard 
# deviation of RMSE from the 100 repetitions using a seed of 1. 
# Hint: use the sapply or map functions.


rmse_f <- function(n){
  #set.seed(1)
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
         data.frame() %>% setNames(c("x", "y"))
  
  set.seed(1)
  N <- 100
  rmse_vet <- replicate(N, {
    y <- dat$y
    test_index <- createDataPartition(y,  times = 1, p = 0.50, list = FALSE)
    
    train_set <- dat %>% slice(-test_index)
    test_set  <- dat %>% slice(test_index)
    
    fit <- lm(y ~ x, data = train_set)
    
    y_hat <- predict(fit, test_set)
    
    #When calculating the RMSE, take the square root of the average of the squared differences. 
    #RMSE<-sqrt(mean((y_hat - test$y)^2))
    #rmse(y_hat, train_set$x)  
    
    sqrt(mean((y_hat - test_set$y)^2))
  })
  
  list(mean(rmse_vet), sd(rmse_vet))
} 

n <- c(100, 500, 1000, 5000, 10000)

# TEST
r100 <- rmse_f(100)
mean(r100)
sd(r100)

r500 <- rmse_f(500)
mean(r500)
sd(r500)

r1000 <- rmse_f(1000)
mean(r1000)
sd(r1000)

r5000 <- rmse_f(5000)
mean(r5000)
sd(r5000)

r10000 <- rmse_f(10000)
mean(r10000)
sd(r10000)

set.seed(1)
rmse_sapply <- sapply(n, rmse_f)

# CORRECT
# mean 100 2.488661
# sd 10000 0.01680585


