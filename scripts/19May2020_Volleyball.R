# Get the Data

vb_matches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-19/vb_matches.csv', guess_max = 76000)

View(vb_matches)

unique(vb_matches$country)

# load libraries
library(tidyverse)
library(hrbrthemes)



#QUESTION: Are you more likely to win if you are in your home country?


#subset the data
Volleyball <- vb_matches %>% 
  select(country, year, gender, w_p1_country, w_p2_country, l_p1_country, l_p2_country) %>% 
  mutate(Wp1_HomeC = ifelse(country == w_p1_country, 0.5, -0.5),
         Wp2_HomeC = ifelse(country == w_p2_country, 0.5, -0.5),
         Lp1_HomeC = ifelse(country == l_p1_country, 0.5, -0.5),
         Lp2_HomeC = ifelse(country == l_p2_country, 0.5, -0.5)) %>% 
 mutate(W_HomeC = Wp1_HomeC + Wp2_HomeC,
        L_HomeC = Lp1_HomeC + Lp2_HomeC)
  
Volleyball2 <- Volleyball %>% 
  select(country, gender, year, W_HomeC, L_HomeC) %>% 
  pivot_longer(
    cols = ends_with("_HomeC"),
    names_to = "Win_Lose",
    values_to = "HomeCountry",
    values_drop_na = TRUE) 

  as.character(year)

glimpse(Volleyball2)


  
Volleyball3 <- Volleyball2 %>% 
  as.character()
  group_by(year)


ggplot(Volleyball2, aes(x = gender, y = HomeCountry, color = Win_Lose)) +
  geom_count() +
  facet_wrap(.~ country) +
  theme_classic()
  
  

View(Volleyball2)


#X is -1 thru +1, vertical line that goes through 0 & countries are all the way going down, postivie/negative size of the dots are on either side of a vertical line 



#Plot it!


