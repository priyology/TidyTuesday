library(tidyverse)
library(hrbrthemes)
library(hrbrmisc)

# Get the Data

polls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/polls.csv')
rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/rankings.csv')

# Or read in with tidytuesdayR package (https://github.com/thebioengineer/tidytuesdayR)
# PLEASE NOTE TO USE 2020 DATA YOU NEED TO USE tidytuesdayR version from GitHub

# Either ISO-8601 date or year/week works!

# Install via devtools::install_github("thebioengineer/tidytuesdayR")

as.data.frame(polls)
glimpse(polls)

## only look at data in 1989 - the year I was born
polls89 <- polls %>% 
  filter (year == 1989)

glimpse(polls89)

plot.89 <- ggplot(data = polls89, aes(x = artist, y = rank)) +
  geom_segment( aes(x= artist, xend= artist, y= 0, yend = rank), color="skyblue") +
  geom_point( color="blue", size=4, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(title="Top Rap Artist rankings in 1989",
       x ="Ranking", y = "Rap / Hip Hop Artist") +
  theme_ft_rc()

print(plot.89)

## only look at data in 2005 - the year I was 16
polls05 <- polls %>% 
  filter (year == 2005)

glimpse(polls05)

plot.05 <- ggplot(data = polls05, aes(x = artist, y = rank)) +
  geom_segment( aes(x= artist, xend= artist, y= 0, yend = rank), color="skyblue") +
  geom_point( color="blue", size=4, alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(title="Top Rap Artist rankings in 2005",
       x ="Ranking", y = "Rap / Hip Hop Artist") +
  theme_ft_rc()

print(plot.05)


## most recent year
max(polls$year) #2019

#only look at data in 2019
polls19 <- polls %>% 
  filter (year == 2019)

glimpse(polls19)

plot.19 <- ggplot(data = polls19, aes(x = artist, y = rank)) +
  geom_segment( aes(x= artist, xend= artist, y= 0, yend = rank), color="grey") +
  geom_point( color="orange", size=4) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  labs(title="Top Rap Artist rankings in 2019",
       x ="Ranking", y = "Rap / Hip Hop Artist") +
  theme_ft_rc()

print(plot.19)



