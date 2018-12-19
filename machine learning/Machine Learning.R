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




# -----

library(dslabs)

mnist <- read_mnist()

str(mnist)

y <- mnist$train$labels

y[5] + y[6]
y[5] > y[6]



