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


# ----------------------------------------------------------
# String Processing
# ----------------------------------------------------------

# ---------

yes <- c("5 feet 7inches", "5 7")
no <- c("5ft 9 inches", "5 ft 9 inches")
s <- c(yes, no)

converted <- s %>% 
  str_replace("feet|foot|ft", "'") %>% 
  str_replace("inches|in|''|\"", "") %>% 
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")

pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
str_detect(converted, pattern)


#-- Fixing the replacement, it lacked space treatment
converted <- s %>% 
  str_replace("\\s*(feet|foot|ft)\\s*", "'") %>% 
  str_replace("\\s*(inches|in|''|\")\\s*", "") %>% 
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")



# ================== String Processing Part 3 ===============

# --- Using Groups and Quantifiers

# Four clear patterns of entries have arisen along with some other minor problems:

# 1 - Many students measuring exactly 5 or 6 feet did not enter any inches. For example, 6' - our pattern requires that inches be included.
# 2 - Some students measuring exactly 5 or 6 feet entered just that number.
# 3 - Some of the inches were entered with decimal points. For example 5'7.5''. Our pattern only looks for two digits.
# 4 - Some entires have spaces at the end, for example 5 ' 9.
# 5 - Some entries are in meters and some of these use European decimals: 1.6, 1,7.
# 6 - Two students added cm.
# 7 - One student spelled out the numbers: Five foot eight inches.

# Case 1
yes <- c("5", "6", "5")
no <- c("5'", "5''", "5'4")
s <- c(yes, no)
str_replace(s, "^([4-7])$", "\\1'0")

# Cases 2 and 4
str_replace(s, "^([56])'?$", "\\1'0")

# Case 3 
pattern <- "^[4-7]\\s*'\\s*(\\d+\\.?\\d*)$"

# Case 5
yes <- c("1,7", "1, 8", "2, " )
no <- c("5,8", "5,3,2", "1.7")
s <- c(yes, no)
str_replace(s, "^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2")

# Trimming
str_trim("5 ' 9 ")

# To upper and to lower case
s <- c("Five feet eight inches")
str_to_lower(s)

# Putting it into a function
convert_format <- function(s){
  s %>%
    str_replace("feet|foot|ft", "'") %>% #convert feet symbols to '
    str_replace_all("inches|in|''|\"|cm|and", "") %>%  #remove inches and other symbols
    str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") %>% #change x.y, x,y x y
    str_replace("^([56])'?$", "\\1'0") %>% #add 0 when to 5 or 6
    str_replace("^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2") %>% #change european decimal
    str_trim() #remove extra space
}

# function that converts words to numbers
words_to_numbers <- function(s){
  str_to_lower(s) %>%  
    str_replace_all("zero", "0") %>%
    str_replace_all("one", "1") %>%
    str_replace_all("two", "2") %>%
    str_replace_all("three", "3") %>%
    str_replace_all("four", "4") %>%
    str_replace_all("five", "5") %>%
    str_replace_all("six", "6") %>%
    str_replace_all("seven", "7") %>%
    str_replace_all("eight", "8") %>%
    str_replace_all("nine", "9") %>%
    str_replace_all("ten", "10") %>%
    str_replace_all("eleven", "11")
}

# Now we can see which problematic entries remain:
  
converted <- problems %>% words_to_numbers %>% convert_format
remaining_problems <- converted[not_inches_or_cm(converted)]
pattern <- "^[4-7]\\s*'\\s*\\d+\\.?\\d*$"
index <- str_detect(remaining_problems, pattern)
remaining_problems[!index]


# -- Question 1
s <- c("5'10", "6'1\"", "5'8inches", "5'7.5")
tab <- data.frame(x = s)

extract(data = tab, col = x, into = c("feet”, “inches", "decimal"), 
        regex = "(\\d)'(\\d{1,2})(\\.)?")


extract(data = tab, col = x, into = c("feet", "inches", "decimal"), 
        regex = "(\\d)'(\\d{1,2})(\\.\\d+)")

extract(data = tab, col = x, into = c("feet", "inches", "decimal"), 
        regex = "(\\d)'(\\d{1,2})\\.\\d+?")

# - This is right
extract(data = tab, col = x, into = c("feet", "inches", "decimal"), 
        regex = "(\\d)'(\\d{1,2})(\\.\\d+)?")    

# -----------------------


