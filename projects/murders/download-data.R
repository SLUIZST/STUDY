url <- "https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv"
dest_file <- "projects/murders/data/murders.csv"
download.file(url, destfile = dest_file)

getwd()