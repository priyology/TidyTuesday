devtools::install_github("alastairrushworth/tdf")
library(tdf)
library(tidyverse)

editions <- tibble::glimpse(editions)
as.data.frame(editions)
(editions)

#Question: What is the age of each winner?

p0 <- ggplot(editions, aes(x=edition, y=age, color = nationality)) + 
  geom_jitter() + 
  theme_classic()

print(p0)
  
#Question: Over time, which countries have won the Tour de France?