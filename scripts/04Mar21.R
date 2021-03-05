#TidyTuesday - Super Bowl Ads

## load packages
library(tidyverse)

## Read in the data
youtube <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-02/youtube.csv')
View(youtube) ## View dataset
summary(youtube) ## look at min/max values of different data
unique(youtube$brand) ## list individual brands

## Graph idea - simple bar plot: total # of views per brand between 2000 & 2020

## summarize #views of funny super bowl ads per brand over the years
yt.fun <- youtube %>%
  select(brand, year, funny, view_count) %>% 
  filter(funny == TRUE) %>% 
  group_by(brand, year) %>% 
  mutate(sum = sum(view_count))

## data for Toyota only
yt.fun.Toyota <- yt.fun %>% 
  filter(brand == "Toyota")

View(yt.fun)

## make a circular bar plot
p <- ggplot(yt.fun, aes(x=as.factor(brand), y=view_count)) +
  geom_bar(stat="identity", fill=alpha("blue", 0.3)) +
  ylim(0,400000) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank())#,
  #  plot.margin = unit(rep(-2,4), "cm")) #+
 # coord_polar(start = 0)

# geom_text(data=label_data, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) 
p

