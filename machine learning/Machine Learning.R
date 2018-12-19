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


# -- Confusion Matrix







# -----

library(dslabs)

mnist <- read_mnist()

str(mnist)

y <- mnist$train$labels

y[5] + y[6]
y[5] > y[6]



