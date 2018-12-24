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

# Q4
# Notice that we had an overall accuracy greater than 96% in the training data, but 
# the overall accuracy was lower in the test data. This can happen often if we overtrain. 
# In fact, it could be the case that a single feature is not the best choice. For example,
# a combination of features might be optimal. Using a single feature and optimizing the 
# cutoff as we did on our training data can lead to overfitting.

# Given that we know the test data, we can treat it like we did our training data to see 
# if the same feature with a different cutoff will optimize our predictions.

# Which feature best optimizes our overall accuracy?


