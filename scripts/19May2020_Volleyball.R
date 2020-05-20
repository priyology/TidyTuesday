# load libraries
library(tidyverse)
library(hrbrthemes)
library(ggalt)

# Get the Data
vb_matches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-19/vb_matches.csv', guess_max = 76000)

View(vb_matches)

#### QUESTION: Are you more likely to win if you are in your home country? ===============


#subset the data & create an index so you can identify when a player IS or ISN'T playing a game in their country of origin 

Volleyball <- vb_matches %>% 
  select(country, year, gender, w_p1_country, w_p2_country, l_p1_country, l_p2_country) %>% 
  mutate(Wp1_HomeC = ifelse(country == w_p1_country, 0.5, -0.5),
         Wp2_HomeC = ifelse(country == w_p2_country, 0.5, -0.5),
         Lp1_HomeC = ifelse(country == l_p1_country, 0.5, -0.5),
         Lp2_HomeC = ifelse(country == l_p2_country, 0.5, -0.5)) %>% 
 mutate(W_HomeC = Wp1_HomeC + Wp2_HomeC,
        L_HomeC = Lp1_HomeC + Lp2_HomeC)

as.numeric(Volleyball$year)

#View(Volleyball)

# create a dataframe where ... 
## 1 = both players were in their country of origin
## 0 = ONE player was in their country of origin
## -1 = neither player was in their country of origin
Volleyball2 <- Volleyball %>% 
  select(country, gender, year, W_HomeC, L_HomeC) %>% 
  pivot_longer(
    cols = ends_with("_HomeC"),
    names_to = "Win_Lose",
    values_to = "HomeCountry",
    values_drop_na = TRUE)

View(Volleyball2)

#break up into a data.frames based on wins / losses


# the number of times a team wins when both/one/none are from that country
Volleyball2.W <- Volleyball2 %>% 
  group_by(country, Win_Lose) %>% 
  filter(Win_Lose == "W_HomeC") %>% 
  summarise(Win_both = sum(HomeCountry == 1),
            Win_one = sum(HomeCountry == 0),
            Win_none = sum(HomeCountry == -1))

Volleyball2.W



#DumbellPlot, package = ggalt

ggplot(Volleyball2.W, aes(y = country, x = Win_both, xend = Win_none)) + 
  geom_dumbbell(size=3, color="#e3e2e1", 
                colour_x = "tomato", colour_xend = "blue",
                dot_guide=TRUE, dot_guide_size=0.25) +
  labs(x=NULL, y=NULL, title="Wins: Red = Both in Country, Blue = None in Country") +
  theme_minimal()

ggplot(Volleyball2.L, aes(y = country, x = Lose_both, xend = Lose_none)) + 
  geom_dumbbell(size=3, color="#e3e2e1", 
                colour_x = "tomato", colour_xend = "blue",
                dot_guide=TRUE, dot_guide_size=0.25) +
  labs(x=NULL, y=NULL, title="Losses: Red = Both in Country, Blue = None in Country") +
  theme_minimal()


#X is -1 thru +1, vertical line that goes through 0 & countries are all the way going down, postivie/negative size of the dots are on either side of a vertical line 


## Other Code!



Volleyball3 <- Volleyball2 %>% 
  group_by(year, gender, country, Win_Lose) %>% 
  summarise(Win_Val = sum(HomeCountry)) 

Volleyball4 <- Volleyball3 %>%
  select(year, gender, country, Win_Val) %>% 
  group_by(country) %>% 
  summarise(maxWV = max(Win_Val),
            minWV = min(Win_Val))

#random dotplot to get my bearings ##FailPlot
ggplot(Volleyball3, aes(x = Win_Val, y = country, color = Win_Lose)) +
  geom_count() +
  facet_wrap(.~ gender) +
  theme_classic()

table(Volleyball$country)
