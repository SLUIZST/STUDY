# ----------------------------------------------------------
# Data Import
# ----------------------------------------------------------

# ---- Paths and the Working Directory

#  Working directory
getwd()

# Changing the working directory 
setwd("F:/Sergio/Estudo/Ciencia de Dados/Study/Data Wrangling/Arquivos")

# Using Dslabs
path <- system.file("extdata", package="dslabs")
list.files(path)

filename <- "murders.csv"
fullpath <- file.path(path, filename)
fullpath

# copying the file to the working directory
setwd("F:/Sergio/Estudo/Ciencia de Dados/Study/Data Wrangling/Arquivos")
file.copy(fullpath, getwd())

# Checking if the file exists in the working directory
file.exists(filename)


# ---- The readr and readxl Packages
library(readr)

# function excel_sheets to view the names of the sheets inside the excel file

# viewing the first lines in the file
read_lines("murders.csv", n_max = 3)

# reading the cvs file
dat <- read_csv(filename)

# -> creates a tibble
head(dat)












