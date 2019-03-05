# ==== Generative Models

# -- Comprehension Check: Generative Models

# In the following exercises, we are going to apply LDA and QDA to the tissue_gene_expression dataset. We will start 
# with simple examples based on this dataset and then develop a realistic example.

# -- Q1
# Create a dataset of samples from just cerebellum and hippocampus, two parts of the brain, and a predictor matrix 
# with 10 randomly selected columns using the following code:
  
library(dslabs)
library(caret)
library(tidyverse)

set.seed(1993)
data("tissue_gene_expression")
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

# Use the train function to estimate the accuracy of LDA.
# What is the accuracy?


# First way - creating partitions...
index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

train_set_x = x[-index,] 
train_set_y = y[-index] 

test_set_x = x[index,] 
test_set_y = y[index] 

train_set = (list('x' = train_set_x, 'y' = train_set_y))
test_set = (list('x' = test_set_x, 'y' = test_set_y))

train_lda <- train(y ~ ., method = "lda", data = train_set)  # Doesn't work this way


# Second way - using only x and y, they can be considered the training set, they're samples!

train_lda<-train(x, y,method="lda")



# - Q2
# In this case, LDA fits two 10-dimensional normal distributions. Look at the fitted model by looking at the 
# finalModel component of the result of train. Notice there is a component called means that includes the estimated 
# means of both distributions. Plot the mean vectors against each other and determine which predictors (genes) 
# appear to be driving the algorithm.

# Which TWO genes appear to be driving the algorithm?


train_lda$finalModel$means

cerebellum_v <- train_lda$finalModel$means[1,]
hippocampus_v <- train_lda$finalModel$means[2,]

plot(cerebellum_v, hippocampus_v)  # That's not enough. It's lacking the labels of the genes in the plot


# https://courses.edx.org/courses/course-v1:HarvardX+PH125.8x+2T2018/courseware/7c50a2fc6d44433fa902be8ed097f301/cf9e5492064f4e7e9787ed0d8c27d1e9/7?activate_block_id=block-v1%3AHarvardX%2BPH125.8x%2B2T2018%2Btype%40html%2Bblock%402cfab1d0efa04413b089059cbb9d347e

library(ggplot2)

means <- data.frame(t(train_lda$finalModel$means)) 
means <- means %>% mutate(gene = as.factor(rownames(means)))

means %>% ggplot(aes(x = cerebellum, y = hippocampus, colour = gene, label = gene)) +
  ggtitle("LDA Means - Cerebellum vs Hippocampus") +
  geom_point() +
  geom_text(aes(label=gene)) +
  theme(legend.position="none")  


# Explanation
# The following code can be used to make the plot:
  
  t(train_lda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()


# - Q3
# Repeat the exercise in Q1 with QDA.
# Create a dataset of samples from just cerebellum and hippocampus, two parts of the brain, and a predictor matrix 
# with 10 randomly selected columns using the following code:
    
  set.seed(1993)
  data("tissue_gene_expression")
  ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
  y <- droplevels(tissue_gene_expression$y[ind])
  x <- tissue_gene_expression$x[ind, ]
  x <- x[, sample(ncol(x), 10)]

# Use the train function to estimate the accuracy of QDA.
# What is the accuracy?
  
  # using only x and y, they can be considered the training set, they're samples!
  
  train_qda<-train(x, y,method="qda")  
  train_qda
  
#  Explanation
# The following code can be used to estimate the accuracy of QDA:
    
fit_qda <- train(x, y, method = "qda")
fit_qda$results["Accuracy"]


# - Q4
# Which TWO genes drive the algorithm when using QDA instead of LDA?
  
library(ggplot2)

means <- data.frame(t(train_qda$finalModel$means)) 
means <- means %>% mutate(gene = as.factor(rownames(means)))

means %>% ggplot(aes(x = cerebellum, y = hippocampus, colour = gene, label = gene)) +
  ggtitle("LDA Means - Cerebellum vs Hippocampus") +
  geom_point() +
  geom_text(aes(label=gene)) +
  theme(legend.position="none")  


# Explanation
# The following code can be used to make the plot to evaluate which genes are driving the algorithm:

t(fit_qda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()

# Q5
# One thing we saw in the previous plots is that the values of the predictors correlate in 
# both groups: some predictors are low in both groups and others high in both groups. 
# The mean value of each predictor found in colMeans(x) is not informative or useful for 
# prediction and often for purposes of interpretation, it is useful to center or scale 
# each column. This can be achieved with the preProcessing argument in train. Re-run LDA 
# with preProcessing = "scale". Note that accuracy does not change, but it is now easier 
# to identify the predictors that differ more between groups than based on the plot made 
# in Q2.

colMeans(x)

train_lda<-train(x, y,method="lda", preProcess = "scale")

train_lda$finalModel$means

# https://courses.edx.org/courses/course-v1:HarvardX+PH125.8x+2T2018/courseware/7c50a2fc6d44433fa902be8ed097f301/cf9e5492064f4e7e9787ed0d8c27d1e9/6?activate_block_id=block-v1%3AHarvardX%2BPH125.8x%2B2T2018%2Btype%40problem%2Bblock%4094d0e754b4f3458ab02b66542fd19e94

# Indeed, the right approach is to identify the two predictors/genes that have the largest 
# differences between the two samples cerebellum and hippocampus. The algorithm should use
# a "center" preprocessing and not a "scale" preprocessing, as instructed.

# -> mileading insuctions -> the correct is preProcess = "center"

train_lda<-train(x, y,method="lda", preProcess = "center")

train_lda$finalModel$means

train_lda$finalModel$means[1,] - train_lda$finalModel$means[2,]


# Explanation
# The following code can be used to make the plot to evaluate which genes are driving the 
# algorithm after scaling:
  
fit_lda <- train(x, y, method = "lda", preProcess = "center")
fit_lda$results["Accuracy"]
t(fit_lda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(predictor_name, hippocampus)) +
  geom_point() +
  coord_flip()

# You can see that it is different genes driving the algorithm now. This is because the 
# predictor means change.
# In the previous exercises we saw that both LDA and QDA approaches worked well. For 
# further exploration of the data, you can plot the predictor values for the two genes 
# with the largest differences between the two groups in a scatter plot to see how they 
# appear to follow a bivariate distribution as assumed by the LDA and QDA approaches, 
# coloring the points by the outcome, using the following code:
  
d <- apply(fit_lda$finalModel$means, 2, diff)
ind <- order(abs(d), decreasing = TRUE)[1:2]
plot(x[, ind], col = y)

# Q6
# Now we are going to increase the complexity of the challenge slightly: we will consider 
# all the tissue types. Use the following code to create your dataset:
  
set.seed(1993)
data("tissue_gene_expression")
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]


# What is the accuracy using LDA?

train_lda<-train(x, y,method="lda")
train_lda

# Explanation
# The following code can be used to obtain the accuracy of the LDA:
  
fit_lda <- train(x, y, method = "lda", preProcess = c("center"))
fit_lda$results["Accuracy"]

# We see that the results are slightly worse when looking at all of the tissue types 
# instead of only selected ones. You can use the confusionMatrix function to learn more 
# about what type of errors we are making, like this: confusionMatrix(fit_lda).



  
  