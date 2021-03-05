#### Load Data ====

devtools::install_github("alastairrushworth/tdf")
library(tidyverse)
library(tdf)
library(ggcorrplot)

winners <- tdf::editions %>% 
  select(-stage_results)

View(winners2)

winners2 <- winners %>% 
  separate(start_date, c("year", "month", "day"))

head(winners2)
glimpse(winners2)

as.numeric(c("year", "month", "day"))

#### This is the part where I make some basic plots and look for a fun data story ====

#Question: What is the age of each winner?

p.age <- ggplot(winners2, aes(x=edition, y=age, color = nationality)) + 
  geom_jitter() + 
  theme_classic()

print(p.age)

#Question: Is there a relationship between age & stages won?

p.stageW <- ggplot(winners2, aes(x=age, y=stage_wins, color = nationality)) + 
  geom_jitter() + 
  theme_classic()

print(p.stageW)

#Question: Is there a relationship between age & stages led?

p.stageL <- ggplot(winners2, aes(x=age, y=stages_led, color = nationality)) + 
  geom_jitter() + 
  theme_classic()

print(p.stageL)
  
#Question: How many times has each country won & in what year?

CountryCount <-  winners2 %>% 
  count(nationality)

as.data.frame(CountryCount)

p.CountryCount <- ggplot(CountryCount, aes(x=nationality, y=n, fill = nationality)) + 
  geom_bar(stat = "identity") + 
  theme_classic()

print(p.CountryCount)

#Question: does the distance change with each edition?

p.dist <- ggplot(winners2, aes(x=edition, y=distance, color = nationality)) + 
  geom_jitter() + 
  theme_classic()

print(p.dist)

#### Plotting more prettily: does time_overall change with distance?

# Add density for each point
winners2$density <- fields::interp.surface(
  MASS::kde2d(d$a, d$b), d[,c("a", "b")])

p.DistYear <- ggplot(winners2, aes(x = edition, y = distance, color = nationality)) + 
  geom_jitter(size = 5, show.legend = TRUE) +
  ggtitle("Race Distance Has Decreased Over Time Due to Viewers' Decreasing Attentions Spans") +
  xlab("Edition") + ylab("Distance (total kilometers)") +
  theme_classic() +
  theme(plot.title = element_text(color="#36c9c2", size=14, face="bold"),
        axis.title.x = element_text(color="#36c9c2", size=14, face="bold"),
        axis.title.y = element_text(color="#36c9c2", size=14, face="bold"))

print(p.DistYear)

ggsave("fig_output/DistYear_7Apr2020.png")
