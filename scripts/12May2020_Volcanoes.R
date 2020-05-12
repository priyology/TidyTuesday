#install.packages(c("ggmap", "mapview"))

#load tidyverse & map libraries
library(tidyverse)
library(ggmap)

#Get the data
volcano <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv')

View(volcano)
unique(volcano$region)

#Filter for volcanoes in the Oceania region
OceaniaVolcanoes <- volcano %>% 
  filter(region == c("Melanesia and Australia", "New Zealand to Fiji"))

#latlong for Oceania
LatLong <- c(lon = 140.0188, lat = -22.7359)

#design the map
VolcanoMap <- get_map(location = LatLong, source = "stamen", maptype = "watercolor", crop = FALSE)

?register_google
