#install.packages(c("ggmap", "mapview"))

#load tidyverse & map libraries
library(tidyverse)
library(ggmap)
library(patchwork)
library(fishualize)
library(hrbrthemes)

#Get the data
volcano <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv')

View(volcano)
unique(volcano$region)

min(volcano$elevation) #-2500
max(volcano$elevation) #6879

#Filter for volcanoes in the Oceania region
Oceania.Volcanoes <- volcano %>% 
  filter(region == c("Melanesia and Australia", "New Zealand to Fiji")) %>% 
  filter(elevation > -1000)

View(Oceania.Volcanoes)

#latlong for Oceania
LatLong <- c(lon = 140.0188, lat = -22.7359)

#register_google(key = "AIzaSyDFrgkgP4nl2JXFcaz5KeMJDX4ZqbZgK6A")

#design the map
VolcanoMap <- get_map(location = LatLong, source = "google", maptype = "satellite", crop = FALSE, zoom = 3)

p1 <- ggmap(VolcanoMap) +
geom_point(aes(x = longitude, y = latitude, color = elevation, size = elevation), data = Oceania.Volcanoes,
  alpha = .6, size = 5) +
 scale_color_fish(option = "Lutjanus_sebae", direction = -1) +
  labs(x = 'Longitude', y = 'Latitude') + ggtitle('Volcano Elevation Across Oceania') +
  theme_ft_rc() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="none")

p1

p2 <- ggplot(Oceania.Volcanoes, aes(x = major_rock_1, y = elevation, color = elevation, size = elevation), size = 5) +
  geom_point(stat = "identity") +
  scale_color_fish(option = "Lutjanus_sebae", direction = -1) +
  labs(x = 'Rock Type', y = 'Elevation') + ggtitle('Rock Type in Oceania Correlates with Elevation') +
  theme_classic() +
  theme_ft_rc() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.8))

p2

p1 + p2

ggsave("fig_output/OceaniaVolcanoes_12May2020.png")


