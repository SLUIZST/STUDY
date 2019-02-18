# ==  Linear regression for prediction

# To quickly make the connection between regression and machine learning, we will reformulate Galton's study with 
# heights: a continuous outcome.

library(HistData)
library(tidyverse)

galton_heights <- GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight)


# Suppose you are tasked with building a machine learning algorithm that predicts the son's height Y using the father's 
# height  X. Let's generate testing and training sets:
  
library(caret)

y <- galton_heights$son
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

train_set <- galton_heights %>% slice(-test_index)
test_set <- galton_heights %>% slice(test_index)

# In this case, if we were just ignoring the father's height and guessing the son's height, we would guess the average 
# height of sons.

avg <- mean(train_set$son)
avg

# Our squared loss is:
  
mean((avg - test_set$son)^2)

# We also introduced least squares as a method for estimating the slope ??0 and intercept ??1:
  
fit <- lm(son ~ father, data = train_set)
fit$coef

# This gives us an estimate of the conditional expectation:
  # ^f (x)=38+0.47x

# We can see that this does indeed provide an improvement over our guessing approach.

y_hat <- fit$coef[1] + fit$coef[2]*test_set$father
mean((y_hat - test_set$son)^2)


# === The predict function

# The predict function is very useful for machine learning applications. This function takes a fitted object from 
# functions such as lm or glm (we learn about glm soon) and a data frame with the new predictors for which to predict. 
# So in our current example, we would use predict like this:
  
y_hat <- predict(fit, test_set)

# Using predict, we can get the same results as we did previously:
  
y_hat <- predict(fit, test_set)
mean((y_hat - test_set$son)^2)




