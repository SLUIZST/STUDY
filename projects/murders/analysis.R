library(tidyverse)
load("projects/murders/rda/murders.rda")

murders %>% mutate(abb = reorder(abb, rate)) %>% ggplot(aes(abb,rate)) + 
  geom_bar(width = 0.5, stat = "identity", color = "black") +
  coord_flip()

getwd()

ggsave("projects/murders/figs/barplot.png")