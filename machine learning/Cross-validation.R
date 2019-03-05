# ==== Cross-validation

# Comprehension Check: Cross-validation

library(tidyverse)

set.seed(1996)
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]


fit <- train(x_subset, y, method = "glm")
fit$results


library(devtools)
devtools::install_bioc("genefilter")
library(genefilter)
tt <- colttests(x, y)



source("https://bioconductor.org/biocLite.R")
biocLite("genefilter")

library(genefilter)
tt <- colttests(x, y)

# Which of the following lines of code correctly creates a vector of the p-values called pvals?
  
pvals <- tt$dm
pvals <- tt$statistic
# pvals <- tt
pvals <- tt$p.value  # OK

# Q3
# Create an index ind with the column numbers of the predictors that were "statistically 
# significantly" associated with y. Use a p-value cutoff of 0.01 to define "statistically 
# significantly."

# How many predictors survive this cutoff?

ind <- which(pvals < 0.01)
length(ind)

# https://courses.edx.org/courses/course-v1:HarvardX+PH125.8x+2T2018/courseware/7c50a2fc6d44433fa902be8ed097f301/2faa56e611464cf4bfd42b13ac7fffa5/5?activate_block_id=block-v1%3AHarvardX%2BPH125.8x%2B2T2018%2Btype%40html%2Bblock%40ee5777f9494a4e9e8a8e999541b2e1e7

# suggest that you clear your workspace using:
rm(list = ls())

# Explanation
# The number of predictors that survive the cutoff can be found using this code:
ind <- which(pvals <= 0.01)
length(ind)

# https://courses.edx.org/courses/course-v1:HarvardX+PH125.8x+2T2018/courseware/7c50a2fc6d44433fa902be8ed097f301/2faa56e611464cf4bfd42b13ac7fffa5/5?activate_block_id=block-v1%3AHarvardX%2BPH125.8x%2B2T2018%2Btype%40html%2Bblock%40ee5777f9494a4e9e8a8e999541b2e1e7

# Q4
# Now re-run the cross-validation after redefinining x_subset to be the subset of x defined 
# by the columns showing "statistically significant" association with y.
# What is the accuracy now?

# In Q4 we have
# to extract from x the columns that correspond to a p-value <= 0.01 (why wouldn't you try something 
#  of the kind x[,ind]?);
# to build up a new x_subset with these columns;
# to apply again the train() function just as we did in Q1.

library(caret)

set.seed(1996)
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x2 <- x[ ,ind]
dim(x2)

#colnames(x2) <- paste("x", 1:ncol(x2), sep = "_")
#y2 <- rbinom(n, 1, 0.5) %>% factor()
#x_subset_2 <- x2

fit2 <- train(x2, y, method = "glm")
fit2$results

# Accuracy = 0.7536868 

# Q5
# Re-run the cross-validation again, but this time using kNN. Try out the following grid 
# k = seq(101, 301, 25) of tuning parameters. Make a plot of the resulting accuracies.

# Which code is correct?

fit3 <- train(x2, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit3)


# Q6
# In the previous exercises, we see that despite the fact that x and y are completely 
# independent, we were able to predict y with accuracy higher than 70%. 
# We must be doing something wrong then.
# What is it?

# Explanation
# Because we used the entire dataset to select the columns in the model, the accuracy is 
# too high. The selection step needs to be included as part of the cross-validation 
# algorithm, and then the cross-validation itself is performed after the column selection 
# step.

# As a follow-up exercise, try to re-do the cross-validation, this time including the 
# selection step in the cross-validation algorithm. The accuracy should now be close to 
# 50%.



# Q7
# Use the train function to predict tissue from gene expression in the 
# tissue_gene_expression dataset. Use kNN.

# What value of k works best?

library(dslabs)
data("tissue_gene_expression")

y4 <- tissue_gene_expression$y
x4<- tissue_gene_expression$x

fit4 <- train(x4, y4, method = "knn", tuneGrid = data.frame(k = seq(1, 11, 2)))
ggplot(fit4)


# https://courses.edx.org/courses/course-v1:HarvardX+PH125.8x+2T2018/courseware/7c50a2fc6d44433fa902be8ed097f301/2faa56e611464cf4bfd42b13ac7fffa5/2?activate_block_id=block-v1%3AHarvardX%2BPH125.8x%2B2T2018%2Btype%40problem%2Bblock%40bc87e11ba4794b77b9bbd5673ee904e3



# --- Comprehension Check: Bootstrap

library(tidyverse)
library(dslabs)
data("mnist_27")


# Q1
# The createResample function can be used to create bootstrap samples. For example, 
# we can create 10 bootstrap samples for the mnist_27 dataset like this:
  
set.seed(1995)
indexes <- createResample(mnist_27$train$y, 10)

# How many times do 3, 4, and 7 appear in the first resampled index?

dim(indexes)
str(indexes)

mean(indexes$Resample01==3)
mean(indexes$Resample01==3) * length(indexes$Resample01)
mean(indexes$Resample01==4)
mean(indexes$Resample01==4) * length(indexes$Resample01)
mean(indexes$Resample01==7)
mean(indexes$Resample01==7) * length(indexes$Resample01)

# Explanation
# You can find the number of times each digit appears using this code:
  
sum(indexes[[1]] == 3)
sum(indexes[[1]] == 4)
sum(indexes[[1]] == 7)


# Q2
# We see that some numbers appear more than once and others appear no times. 
# This has to be this way for each dataset to be independent. Repeat the exercise for 
# all the resampled indexes.

# What is the total number of times that 3 appears in all of the resampled indexes?

mean(indexes$Resample01==3)
mean(indexes$Resample02==3)
mean(indexes$Resample03==3)
mean(indexes$Resample04==3)
mean(indexes$Resample05==3)
mean(indexes$Resample06==3)
mean(indexes$Resample07==3)
mean(indexes$Resample08==3)
mean(indexes$Resample09==3)
mean(indexes$Resample10==3)


totappearances_3 <- (mean(indexes$Resample01==3) * length(indexes$Resample01)) +
                    (mean(indexes$Resample02==3) * length(indexes$Resample02)) +
                    (mean(indexes$Resample03==3) * length(indexes$Resample03)) +
                    (mean(indexes$Resample04==3) * length(indexes$Resample04)) +
                    (mean(indexes$Resample05==3) * length(indexes$Resample05)) +
                    (mean(indexes$Resample06==3) * length(indexes$Resample06)) +
                    (mean(indexes$Resample07==3) * length(indexes$Resample07)) +
                    (mean(indexes$Resample08==3) * length(indexes$Resample08)) +
                    (mean(indexes$Resample09==3) * length(indexes$Resample09)) +
                    (mean(indexes$Resample10==3) * length(indexes$Resample10))


# Explanation
# You can find the number of times 3 appears using this code:

x=sapply(indexes, function(ind){
    sum(ind == 3)
  })
sum(x)


# Q3
# Generate a random dataset using the following code:
  
set.seed(1)
y <- rnorm(100, 0, 1)

# Estimate the 75th quantile, which we know is qnorm(0.75), with the sample quantile: quantile(y, 0.75).

# Run a Monte Carlo simulation with 10,000 repetitions to learn the expected value and standard error of 
# this random variable. Set the seed to 1.

B <- 10^4

set.seed(1)

Qtls <- replicate(B, {
  y <- rnorm(100, 0, 1)
  q <- quantile(y, 0.75)
})


qqnorm(Qtls)
qqline(Qtls)

mean(Qtls)
sd(Qtls)


# Explanation
# The following code can be used to run the simulation and calculate the expected value and standard error:
  
set.seed(1)
B <- 10000
q_75 <- replicate(B, {
  y <- rnorm(100, 0, 1)
  quantile(y, 0.75)
})

mean(q_75)
sd(q_75)


# Q4
# In practice, we can't run a Monte Carlo simulation. Use 10 bootstrap samples to estimate the standard error using 
# just the initial sample y. Set the seed to 1.

set.seed(1)
indxs <- createResample(y, 10)

x=sapply(indxs, function(ind){
  quantile(y[ind], 0.75)
})

mean(x)
sd(x)

# Explanation
# The following code can be used to take 10 bootstrap samples and calculate the expected value and standard error:
  
set.seed(1)
indexes <- createResample(y, 10)
q_75_star <- sapply(indexes, function(ind){
  y_star <- y[ind]
  quantile(y_star, 0.75)
})
mean(q_75_star)
sd(q_75_star)


# Q5
# Repeat the exercise from Q4 but with 10,000 bootstrap samples instead of 10. Set the seed to 1.

set.seed(1)
indxs <- createResample(y, 10000)

x=sapply(indxs, function(ind){
  quantile(y[ind], 0.75)
})

mean(x)
sd(x)


# Explanation
# The following code can be used to take 10,000 bootstrap samples and calculate the expected value and standard error:
  
set.seed(1)
indexes <- createResample(y, 10000)
q_75_star <- sapply(indexes, function(ind){
  y_star <- y[ind]
  quantile(y_star, 0.75)
})
mean(q_75_star)
sd(q_75_star)

