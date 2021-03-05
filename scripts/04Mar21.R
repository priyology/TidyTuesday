#TidyTuesday - Super Bowl Ads

## load packages
library(tidyverse)

## Read in the data
youtube <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-02/youtube.csv')
View(youtube) ## View dataset
summary(youtube) ## look at min/max values of different data
unique(youtube$brand) ## list individual brands

## Graph idea - simple bar plot: total # of views per brand between 2000 & 2020

yt.sum <- youtube %>%
filter(funny == TRUE) %>%
group_by(brand) %>% 
mutate(sum(view_count))

View(yt.sum)
