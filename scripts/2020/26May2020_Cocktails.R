#load libraries
library(tidyverse)
library(patchwork)
library(ggridges)
library(forcats)
library(hrbrthemes)

#Get the data!
cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/cocktails.csv')


# QUESTION: what is the maximum number of ingredients / range of ingredients that each glass holds?

#make sure all glasses are the same case (lowercase)

cocktails$glass <- tolower(cocktails$glass)
View(cocktails)

#abbreviated data.frame
C <- cocktails %>%
  select(drink, category, glass, ingredient_number, ingredient) %>% 
  filter(category == "Cocktail") %>%
  group_by(glass)

View(C)


#basic ggridges plot
ggplot(C, aes(x = ingredient_number, y = glass, fill = glass)) +
  geom_density_ridges() +
  theme_ridges() + 
  labs(xlab = "Ingredient #", 
        ylab = "Glass Type", 
        title = "How many ingredients can fit inside a cocktail glass?", 
        subtitle = "Yes, this is an erroneous analysis",
        caption = "by @priyology") +
  theme_ft_rc() +
  theme(legend.position = "none")

ggsave("fig_output/CocktailGlassvIngredients_26May2020.jpg", width = 8, height = 6)


#### other code =======================

#data exploration
View(cocktails)
unique(cocktails$ingredient)
length(cocktails$ingredient)
unique(cocktails$glass)
unique(cocktails$category)

#look at the number of drinks in each category
cocktails %>% 
  count(category, sort = TRUE, name = "n_drinks")
