# ----------------------------------------------------------
# Data Import
# ----------------------------------------------------------

# ---- Paths and the Working Directory ----------------

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


# --------------- The readr and readxl Packages -------------------
library(readr)

# function excel_sheets to view the names of the sheets inside the excel file

# viewing the first lines in the file (confirm if there's a header in the file, 
#  if it's comma delimited)
read_lines("murders.csv", n_max = 3)

# reading the cvs file
dat <- read_csv(filename)

# -> creates a tibble
head(dat)


# ---- Importing Data Using R-base Functions ----------------------

# --> read.table, read.csv and read.delim

dat2 <- read.csv(fullpath)

# Difference 1 -> Now we have a dataframe, not a table
class(dat2)

# Difference 2 -> Characters converted to factors
class(dat2$abb)
class(dat2$region)

# To avoid this
dat3 <- read.csv(fullpath, stringsAsFactors = FALSE)
class(dat3$abb)
class(dat3$region)


# ------------ Downloading Files from the Internet ----------------------

url <- "https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv"

datweb <- read.csv(url)
datweb

# If you want a local copy of the file
getwd()
setwd("F:/Sergio/Estudo/Ciencia de Dados/Study/Data Wrangling/Arquivos")
download.file(url, "murdersWEB.csv")

# tempdir() and tempfile()

tempfile()

tmp_filename <- tempfile()
download.file(url, tmp_filename)
datweb2 <- read_csv(tmp_filename)
file.remove(tmp_filename)

datweb2

tempdir()










