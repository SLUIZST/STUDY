# === The confusion matrix, prevalence, sensitivity and specificity

library(caret)
library(dslabs)

data(heights)
y <- heights$sex
x <- heights$height

set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]

# Overall accuracy

y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE) %>% 
  factor(levels = levels(test_set$sex))

mean(y_hat == test_set$sex)


# Can we do better? Exploratory data analysis suggests we can because, on average, 
# males are slightly taller than females:
heights %>% group_by(sex) %>% summarize(mean(height), sd(height))

y_hat <- ifelse(x > 62, "Male", "Female") %>% factor(levels = levels(test_set$sex))

mean(y == y_hat)

cutoff <- seq(61, 70)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
})

plot(cutoff, accuracy)

max(accuracy)
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)
mean(y_hat == test_set$sex)

# The confusion matrix

table(predicted = y_hat, actual = test_set$sex)

test_set %>% 
  mutate(y_hat = y_hat) %>%
  group_by(sex) %>% 
  summarize(accuracy = mean(y_hat == sex))

# the prevalence of males in this dataset is high
prev <- mean(y == "Male")
prev

# This can actually be a big problem in machine learning





