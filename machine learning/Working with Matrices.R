# -----  Working with Matrices



# --- Comprehension Check: Working with Matrices

# Q1
# Which line of code correctly creates a 100 by 10 matrix of randomly generated normal 
# numbers and assigns it to x?

x <- matrix(rnorm(100*10), 100, 10)

# Q2
# Write the line of code that would give you the specified information about the matrix x 
# that you generated in q1. Do not include any spaces in your line of code.

# Dimension of x
dim(x)

# Number of rows of x.
length(x[,1])
nrow(x)  # OK

# Number of columns of x.
length(x[1,])
ncol(x)  # OK

# Q3
# Which of the following lines of code would add the scalar 1 to row 1, the scalar 2 
# to row 2, and so on, for the matrix x?
# Select ALL that apply.

x1 <- x
x1 <- x1 + seq(nrow(x))  # OK

x2 <- x
x2 <- sweep(x2, 1, 1:nrow(x),"+")   # OK

# Q4
# Which of the following lines of code would add the scalar 1 to column 1, the scalar 2 
# to column 2, and so on, for the matrix x?
# Select ALL that apply.

x3 <- x
x3 <- sweep(x3, 2, 1:ncol(x), FUN = "+")  # OK

# Q5
# Which code correctly computes the average of each row of x?

rowMeans(x)  # OK
mean(x)

# Which code correctly computes the average of each column of x?
colMeans(x)


# Q6
# For each digit in the mnist training data, compute the proportion of pixels that are in 
# the grey area, defined as values between 50 and 205. (To visualize this, you can make a 
# boxplot by digit class.)

# What proportion of pixels are in the grey area overall, defined as values between 50 and
# 205?

library(dslabs)
mnist <- read_mnist()
head(mnist)

x <- mnist$train$images[1:1000,] 
y <- mnist$train$labels[1:1000]

# TEST
grid <- matrix(x[3,], 28, 28)
image(1:28, 1:28, grid)
image(1:28, 1:28, grid[,28:1])

library(tidyverse)

avg <- rowMeans(x)
data.frame(labels = as.factor(y), row_averages = avg) %>%
  ggplot(aes(labels, row_averages)) +
  geom_boxplot()


# binarizing the data
bin_x <- x
bin_x[bin_x < 50  |  bin_x >  205] <- 0 
bin_x[bin_x >= 50 &  bin_x <= 205] <- 1

avg_gray <- rowMeans(bin_x)
data.frame(labels = as.factor(y), row_averages = avg_gray) %>%
  ggplot(aes(labels, row_averages)) +
  geom_boxplot()

mean(bin_x)  # OK

# Other way
avg_gray2 <- rowMeans(x >= 50 & x <= 205)
mean(avg_gray2)




