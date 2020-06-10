library(tidyverse)
library(lubridate)
library(stringr)
install.packages("scales")

#get the data
firsts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/firsts.csv')
science <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/science.csv')


View(mj_life)

#scope out the data
View(firsts)
View(science)

#how many occupations are there -- how many are unique
length(science$occupation_s)
unique(science$occupation_s)  #many held multiple occupations


#histogram of differnt job types
ggplot(science, aes(x = occupation_s)) + geom_histogram(stat = "count") + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90))

#create a category to identify whether inventor or not
science$occupation_s <- tolower(science$occupation_s)
x <- str_detect(science$occupation_s, "inventor")

science$inventor <- ifelse(str_detect(science$occupation_s, "inventor"), "inventor", "not")


#create a dataframe for 
TL <- data.frame(birth = science$birth,
                 achievement = paste(science$name, ": ", science$inventions_accomplishments),
                 false_date = as.Date(paste("01-", "01-", science$birth)),
                 occupation = science$occupation_s,
                 inventor = science$inventor)

head(TL)
