


# -------- Comprehension Check: Distance

# Q1
# Load the following dataset:
  
library(dslabs)
data("tissue_gene_expression")

# This dataset includes a matrix x:
  
dim(tissue_gene_expression$x)

# This matrix has the gene expression levels of 500 genes from 189 biological samples 
# representing seven different tissues. The tissue type is stored in y:
  
table(tissue_gene_expression$y)

# Which of the following lines of code computes the Euclidean distance between each 
# observation and stores it in the object d?

d <- dist(tissue_gene_expression$x)  # OK

# Q2
# Compare the distances between observations 1 and 2 (both cerebellum), observations 39 and 
# 40 (both colon), and observations 73 and 74 (both endometrium).

# Distance-wise, are samples from tissues of the same type closer to each other?


tissue_gene_expression$y[1:2]

x_1 <- tissue_gene_expression$x[1,]
x_2 <- tissue_gene_expression$x[2,]

tissue_gene_expression$y[39:40]

x_39 <- tissue_gene_expression$x[39,]
x_40 <- tissue_gene_expression$x[40,]

tissue_gene_expression$y[73:74]

x_73 <- tissue_gene_expression$x[73,]
x_74 <- tissue_gene_expression$x[74,]

# distance from tissues of the same type
sqrt(sum((x_1-x_2)^2))

sqrt(sum((x_39-x_40)^2))

sqrt(sum((x_73-x_74)^2))

# distance from tissues of the diferent types
sqrt(sum((x_1-x_39)^2))
sqrt(sum((x_1-x_40)^2))
sqrt(sum((x_1-x_73)^2))
sqrt(sum((x_1-x_74)^2))

sqrt(sum((x_2-x_39)^2))
sqrt(sum((x_2-x_40)^2))
sqrt(sum((x_2-x_73)^2))
sqrt(sum((x_2-x_74)^2))

sqrt(sum((x_39-x_73)^2))
sqrt(sum((x_39-x_74)^2))

sqrt(sum((x_40-x_73)^2))
sqrt(sum((x_40-x_74)^2))

# Explanation
# You can calculate the distances using the following code:
  
ind <- c(1, 2, 39, 40, 73, 74)
as.matrix(d)[ind,ind]

# Q3
# Make a plot of all the distances using the image function to see if the pattern you 
# observed in Q2 is general.

# Which code would correctly make the desired plot?

image(as.matrix(d))

# Explanation
# When we examine the plot, we do see that the pattern holds and that samples from the 
# same tissue are closest to each other, although there do appear to be some additional 
# close distances between tissue types as well.




