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



library("stringr", lib.loc="~/R/win-library/3.4")

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

# -------------------------------------------------------

# ----- String Splitting


# Question 1

staff <- "Mandy, Chris and Laura"

str_split(staff, ",|and")
str_split(staff, ", | and ")         # OK    
str_split(staff, ",\\s|\\sand\\s")   # OK
str_split(staff, "\\s?(,|and)\\s?")

# Question 2

schedule <- data.frame(day=c("Monday", "Tuesday"), staff=c("Mandy, Chris and Laura", "Steve, Ruth and Frank"))

library("tidyr", lib.loc="~/R/win-library/3.4")
tidy <- schedule %>% mutate(staff = str_split(staff, ", | and ")) %>% unnest()  # OK

tidy <- separate(schedule, staff, into = c("s1","s2","s3"), sep = ",") %>% 
  gather(key = s, value = staff, s1:s3)

tidy <- schedule %>% 
  mutate(staff = str_split(staff, ", | and ", simplify = TRUE)) %>% 
  unnest()


# -------------------------------------------------------
# --- Case Study: Extracting a Table from a PDF

# One of the datasets provided in dslabs shows scientific funding rates by gender in the Netherlands:
  
library(dslabs)
data("research_funding_rates")
research_funding_rates 

# Downloading the data

library("pdftools")
temp_file <- tempfile()
url <- "http://www.pnas.org/content/suppl/2015/09/16/1510159112.DCSupplemental/pnas.201510159SI.pdf"
download.file(url, temp_file)
txt <- pdf_text(temp_file)
file.remove(temp_file)

raw_data_research_funding_rates <- txt[2]

data("raw_data_research_funding_rates")

# Looking at the download
library("dplyr")
raw_data_research_funding_rates %>% head

# Each line on the page, including the table rows, is separated by the symbol for newline: \n.
# We can therefore can create a list with the lines of the text as elements:

library("stringr")  
tab <- str_split(raw_data_research_funding_rates, "\n")

# Because we start off with just one element in the string, we end up with a list with just one entry:
tab <- tab[[1]]

# By examining this object,
tab %>% head

# we see that the information for the column names is the third and forth entires:
  
the_names_1 <- tab[3]
the_names_2 <- tab[4]

# Extracting the table data
the_names_1

the_names_1 <- the_names_1 %>%
  str_trim() %>%
  str_replace_all(",\\s.", "") %>%
  str_split("\\s{2,}", simplify = TRUE)
the_names_1

# Now let's look at the second line:

the_names_2

the_names_2 <- the_names_2 %>%
  str_trim() %>%
  str_split("\\s+", simplify = TRUE)
the_names_2

# Now we can join these to generate one name for each column:
  
tmp_names <- str_c(rep(the_names_1, each = 3), the_names_2[-1], sep = "_")
the_names <- c(the_names_2[1], tmp_names) %>%
  str_to_lower() %>%
  str_replace_all("\\s", "_")
the_names


# Now we are ready to get the actual data. By examining the tab object, we notice 
# that the information is in lines 6 through 14. We can use str_split again to 
# achieve our goal:
library("tidyverse")

new_research_funding_rates <- tab[6:14] %>%
  str_trim %>%
  str_split("\\s{2,}", simplify = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  setNames(the_names) %>%
  mutate_at(-1, parse_number)
new_research_funding_rates %>% head()

# We can see that the objects are identical:
  
identical(research_funding_rates, new_research_funding_rates)


# -------------------------------------------------------
# ---  Recoding

# Question 1

dat <- gapminder %>% filter(region == "Middle Africa") %>% 
  mutate(country_short = recode(country, 
                                "Central African Republic" = "CAR", 
                                "Congo, Dem. Rep." = "DRC",
                                "Equatorial Guinea" = "Eq. Guinea"))


# ============ Dates, Times, and Text Mining  ======================

# -- Text Mining

# --- Case study: Trump Tweets




