library(tidyverse)
murders <- read.csv("projects/murders/data/murders.csv")
murders <- murders %>% mutate(region = factor(region), rate = total / population * 10^5)
save(murders, file = "projects/murders/rda/murders.rda")



getwd()
